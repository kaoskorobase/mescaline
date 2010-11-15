{-# LANGUAGE CPP #-}

#define SCHEDULE_COMPLETION_BUNDLES 0

module Mescaline.Synth.Sampler.Model (
    Sampler
  , new
  , free
  , playUnit
) where

import           Control.Monad.Reader
import           Data.Accessor ((^.))
import           Mescaline (Duration, Time)
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache.Server (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache.Server as BC
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import           Mescaline.Synth.Pattern.Event (Synth)
import qualified Mescaline.Synth.Pattern.Event as P
import           Sound.OpenSoundControl (OSC(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 hiding (Output, free, gate, sync)
import           Sound.SC3.Lang.Collection
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Monad (NodeId, Server)
import qualified Sound.SC3.Server.Monad as S
import           Sound.SC3.Server.Notification (n_end)

data Voice = Voice { voiceId :: NodeId
                   , buffer  :: Buffer
                   } deriving (Eq, Show)

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
        rate   = bufRateScale KR bufnum * control KR "rate" 1

voiceDefMem :: Int -> UGen
voiceDefMem n = output $ toStereo $ playBuf n bufnum rate 1 (control KR "start" (0)) NoLoop DoNothing * voiceEnv
    where
        bufnum = control KR "bufnum" (-1)
        rate   = bufRateScale KR bufnum * control KR "rate" 1

voiceDefName :: Int -> String
voiceDefName 1  = "es.globero.mescaline.voice_1"
voiceDefName 2  = "es.globero.mescaline.voice_2"
voiceDefName nc = "es.globero.mescaline.voice_" ++ (show nc)

synthBounds :: Synth -> (Time, Duration)
synthBounds synth = (Unit.onset (synth ^. P.unit) + off, max 0 (dur - off))
    where
        off = synth ^. P.offset
        dur = synth ^. P.duration

startVoice :: Voice -> Time -> Synth -> OSC
startVoice voice time synth =
    let timeTag = if time <= 0
                  then OSC.immediately
                  else OSC.UTCr (time + synth ^. P.latency)
    in Bundle timeTag
        [s_new (voiceDefName $ BC.numChannels $ buffer voice) (fromIntegral $ voiceId voice) AddToTail 0
            ([ ("bufnum", fromIntegral $ BC.uid $ buffer voice)
             , ("rate", synth ^. P.rate)
             , ("attackTime",   synth ^. P.attackTime)
             , ("releaseTime",  synth ^. P.attackTime)
             , ("sustainLevel", synth ^. P.sustainLevel)
             ] ++
               if voiceGateEnvelope
                  then []
                  else [ ("dur", snd (synthBounds synth)) ])
             ]

stopVoice :: Voice -> Time -> Synth -> OSC
stopVoice voice time synth =
    if voiceGateEnvelope
        then Bundle (OSC.UTCr (time + synth ^. P.latency))
                [n_set1 (fromIntegral $ voiceId voice) "gate" (synth ^. P.gateLevel)]
        else Bundle OSC.immediately []

allocVoice :: BufferCache -> Unit.Unit -> (Voice -> Maybe OSC) -> Server Voice
allocVoice cache unit completion = do
    nid <- S.alloc S.nodeId
    buf <- BC.allocBuffer
            (completion . Voice nid)
            cache
            (BC.allocBytes
                (SourceFile.numChannels (Unit.sourceFile unit))
                diskBufferSize)
    return $ Voice nid buf

freeVoice :: BufferCache -> Voice -> Server ()
freeVoice cache voice = do
    S.free S.nodeId (voiceId voice)
    S.sync $ S.send $ b_close $ fromIntegral $ BC.uid $ buffer voice
    BC.freeBuffer cache (buffer voice)

data Sampler = Sampler {
    bufferCache  :: !BufferCache
  , playUnitFunc :: !(Sampler -> Time -> Synth -> Server ())
  }

new :: Server Sampler
new = do
    b <- liftIO $ do
        conf <- Config.getConfig
        either (const $ return False) return (Config.get conf "Synth" "scheduleCompletionBundles")

    liftIO $ Log.noticeM "Synth" $ (if b then "U" else "Not u") ++ "sing completion bundle scheduling."

    -- Load synthdefs
    S.sync $ S.send $ Bundle OSC.immediately [ d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                                               d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]

    -- Pre-allocate disk buffers, mono and stereo
    let nb = 64
    cache <- BC.new $
        replicate nb (BC.allocBytes 1 diskBufferSize)
     ++ replicate nb (BC.allocBytes 2 diskBufferSize)

    return $ Sampler cache (if b then playUnit_schedComplBundles else playUnit_noSchedComplBundles)

free :: Sampler -> Server ()
free sampler = do
    S.async $ S.send $ g_freeAll [0]
    BC.release (bufferCache sampler)

playUnit :: Sampler -> Time -> Synth -> Server ()
playUnit sampler = (playUnitFunc sampler) sampler

playUnit_noSchedComplBundles :: Sampler -> Time -> Synth -> Server ()
playUnit_noSchedComplBundles sampler time synth = do
    voice <- allocVoice cache unit (const Nothing)
    S.sync $ S.send $ b_read
                        (fromIntegral (BC.uid (buffer voice)))
                        (SourceFile.path sourceFile)
                        (truncate (SourceFile.sampleRate sourceFile * fst (synthBounds synth)))
                        (-1) 0 1
    _ <- S.send (startVoice voice time synth) `S.syncWith` n_end (voiceId voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = synth ^. P.unit
        sourceFile = Unit.sourceFile unit

playUnit_schedComplBundles :: Sampler -> Time -> Synth -> Server ()
playUnit_schedComplBundles sampler time synth = do
    voice <- allocVoice cache unit (\voice ->
            Just $
                b_read'
                    (startVoice voice time synth)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset (synth ^. P.unit))
                    (-1) 0 1)
    -- tu <- utcr
    -- FIXME: Why is this necessary?!
    S.unsafeSync
    -- print (t-tu, t+dur-tu)
    liftIO $ OSC.pauseThreadUntil (time + dur)
    _ <- S.send (stopVoice voice (time + dur) synth) `S.syncWith` n_end (voiceId voice)
    -- tu' <- utcr
    -- liftIO $ putStrLn ("node end: " ++ show voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = synth ^. P.unit
        dur        = Unit.duration unit
        sourceFile = Unit.sourceFile unit
