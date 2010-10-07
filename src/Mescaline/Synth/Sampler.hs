{-# LANGUAGE BangPatterns #-}
module Mescaline.Synth.Sampler (
    Sampler
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.Process
import           Control.Monad.Reader
import           Data.Accessor ((^.))
import           Mescaline (Time)
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache.Server (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache.Server as BC
import           Mescaline.Synth.Pattern (SynthParams)
import qualified Mescaline.Synth.Pattern as P
import           Sound.OpenSoundControl (OSC(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Transport (Transport)
import           Sound.SC3 hiding (Output, free, gate, sync)
import           Sound.SC3.Lang.Collection
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as Conn
import           Sound.SC3.Server.Monad as S
import           Sound.SC3.Server.Notification (n_end)
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.State as State

data Voice = Voice NodeId Buffer deriving (Eq, Show)

buffer :: Voice -> Buffer
buffer (Voice _ b) = b

diskBufferSize :: Int
diskBufferSize = 8192*8

-- | Attack-sustain-release envelope parameter constructor.
asr :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
asr aT sL rT c = env [0, sL, 0] [aT, rT] c 1 (-1)

-- | Whether to use a gated envelope or one with a fixed duration.
voiceGateEnvelope :: Bool
voiceGateEnvelope = False

-- | Gated envelope.
voiceEnvGate :: UGen
voiceEnvGate = envGen AR gate 1 0 1 RemoveSynth (asr attackTime sustainLevel releaseTime [EnvLin])
    where gate         = control KR "gate" 1
          sustainLevel = control KR "sustainLevel" 1
          attackTime   = control KR "attackTime" 0
          releaseTime  = control KR "releaseTime" 0

-- | Fixed duration envelope.
voiceEnvFixed :: UGen
voiceEnvFixed = envGen AR 1 1 0 1 RemoveSynth (envLinen attackTime dur releaseTime sustainLevel)
    where dur          = control KR "dur" 0
          sustainLevel = control KR "sustainLevel" 1
          attackTime   = control KR "attackTime" 0
          releaseTime  = control KR "releaseTime" 0

-- | Voice envelope.
voiceEnv :: UGen
voiceEnv = if voiceGateEnvelope then voiceEnvGate else voiceEnvFixed

-- | Convert a N-channel UGen to a two-channel UGen.
toStereo :: UGen -> UGen
toStereo u =
    case mceChannels u of
        [x]    -> pan2 x pan amp
        [x, y] -> balance2 x y pan amp
        xs     -> sum (map (\[x, y] -> balance2 x y pan amp) $ clump 2 xs)
    where
        pan = control KR "pan" 0
        amp = control KR "amp" 1

output :: UGen -> UGen
output = offsetOut (control KR "out" 0)

voiceDef :: Int -> UGen
voiceDef n = output $ toStereo $ vDiskIn n bufnum rate NoLoop * voiceEnv
    where
        bufnum = control KR "bufnum" (-1)
        rate   = bufRateScale KR bufnum

voiceDefMem :: Int -> UGen
voiceDefMem n = output $ toStereo $ playBuf n bufnum rate 1 (control KR "start" (0)) NoLoop DoNothing * voiceEnv
    where
        bufnum = control KR "bufnum" (-1)
        rate   = bufRateScale KR bufnum

voiceDefName :: Int -> String
voiceDefName 1  = "es.globero.mescaline.voice_1"
voiceDefName 2  = "es.globero.mescaline.voice_2"
voiceDefName nc = "es.globero.mescaline.voice_" ++ (show nc)

startVoice :: Voice -> Time -> Unit.Unit -> SynthParams -> OSC
startVoice (Voice nid buf) time unit params =
    let timeTag = if time <= 0
                  then OSC.immediately
                  else OSC.UTCr (time + params ^. P.latency)
    in Bundle timeTag
        [s_new (voiceDefName $ BC.numChannels buf) (fromIntegral nid) AddToTail 0
            ([ ("bufnum", fromIntegral $ BC.uid buf),
              ("attackTime",   params ^. P.attackTime),
              ("releaseTime",  params ^. P.attackTime),
              ("sustainLevel", params ^. P.sustainLevel) ]
              ++
              if voiceGateEnvelope
                then []
                else [("dur", Unit.duration unit)])
            ]

stopVoice :: Voice -> Time -> SynthParams -> OSC
stopVoice (Voice nid _) time params =
    if voiceGateEnvelope
        then Bundle (OSC.UTCr (time + params ^. P.latency))
                [n_set1 (fromIntegral nid) "gate" (params ^. P.gateLevel)]
        else Bundle OSC.immediately []

allocVoice :: BufferCache -> Unit.Unit -> (Voice -> Maybe OSC) -> Server Voice
allocVoice cache unit completion = do
    nid <- alloc nodeId
    buf <- BC.allocBuffer
            (completion . Voice nid)
            cache
            (BC.allocBytes
                (SourceFile.numChannels (Unit.sourceFile unit))
                diskBufferSize)
                          
    return $ Voice nid buf

freeVoice :: BufferCache -> Voice -> Server ()
freeVoice cache (Voice _ buf) = do
    S.sync $ b_close (fromIntegral $ BC.uid buf)
    BC.freeBuffer cache buf

playUnit :: BufferCache -> Time -> Unit.Unit -> SynthParams -> Server ()
playUnit cache time unit params = do
    voice@(Voice nid buf) <- allocVoice cache unit (\voice ->
            Just $ b_read'
                    (startVoice voice time unit params)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
                    (-1) 0 1)
    -- tu <- utcr
    -- FIXME: Why is this necessary?!
    S.unsafeSync
    -- S.send (startVoice voice time unit params)
    -- print (t-tu, t+dur-tu)
    liftIO $ OSC.pauseThreadUntil (time + dur)
    S.send $ stopVoice voice (time + dur) params
    S.waitFor $ n_end nid
    -- tu' <- utcr
    -- liftIO $ putStrLn ("node end: " ++ show voice)
    freeVoice cache voice
    return ()
    where
        dur = Unit.duration unit
        sourceFile = Unit.sourceFile unit

data Input =
    Reset
  | PlayUnit !Time !Unit.Unit !SynthParams

data Output =
    UnitStarted Time Unit.Unit
  | UnitStopped Time Unit.Unit

type Sampler = Handle Input Output

new :: Transport t => t -> Process.ServerOptions -> IO Sampler
new t serverOpts = do
    conn <- Conn.new (State.new serverOpts) t
    cache <- flip runServer conn $ do
        S.sync $ Bundle OSC.immediately [ d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                                          d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]
        BC.new (replicate 4 (BC.allocBytes 1 diskBufferSize) ++ replicate 4 (BC.allocBytes 1 diskBufferSize))
    spawn $ loop conn cache
    where
        loop !conn !cache = do
            -- TODO: Create a ServerT monad transformer!
            x <- recv
            case x of
                Reset ->
                    liftIO $ flip runServer conn $ do
                        S.send (g_freeAll [0])
                        BC.release cache
                PlayUnit t u p -> do
                    h <- self
                    io $ flip runServer conn $ do
                        fork $ do
                            io $ notifyListeners h $ UnitStarted t u
                            -- io $ putStrLn $ "playUnit: " ++ show (t, u, p)
                            playUnit cache t u p
                            -- io $ putStrLn $ "stoppedUnit: " ++ show (t, u, p)
                            io $ notifyListeners h $ UnitStopped t u
                    return ()
            loop conn cache
