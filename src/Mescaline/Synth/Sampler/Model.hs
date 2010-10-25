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
import           Mescaline (Time)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache.Server (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache.Server as BC
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import           Mescaline.Synth.Pattern.Event (SynthParams)
import qualified Mescaline.Synth.Pattern.Event as P
import           Sound.OpenSoundControl (OSC(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 hiding (Output, free, gate, sync)
import           Sound.SC3.Lang.Collection
#if SCHEDULE_COMPLETION_BUNDLES == 1
import           Sound.SC3.Server.Command.Completion
#endif -- SCHEDULE_COMPLETION_BUNDLES
import           Sound.SC3.Server.Monad as S
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
startVoice voice time unit params =
    let timeTag = if time <= 0
                  then OSC.immediately
                  else OSC.UTCr (time + params ^. P.latency)
    in Bundle timeTag
        [s_new (voiceDefName $ BC.numChannels $ buffer voice) (fromIntegral $ voiceId voice) AddToTail 0
            ([ ("bufnum", fromIntegral $ BC.uid $ buffer voice),
              ("attackTime",   params ^. P.attackTime),
              ("releaseTime",  params ^. P.attackTime),
              ("sustainLevel", params ^. P.sustainLevel) ]
              ++
              if voiceGateEnvelope
                then []
                else [("dur", Unit.duration unit)])
            ]

stopVoice :: Voice -> Time -> SynthParams -> OSC
stopVoice voice time params =
    if voiceGateEnvelope
        then Bundle (OSC.UTCr (time + params ^. P.latency))
                [n_set1 (fromIntegral $ voiceId voice) "gate" (params ^. P.gateLevel)]
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
freeVoice cache voice = do
    S.sync $ b_close $ fromIntegral $ BC.uid $ buffer voice
    BC.freeBuffer cache (buffer voice)

newtype Sampler = Sampler { bufferCache :: BufferCache }

new :: Server Sampler
new = do
    S.sync $ Bundle OSC.immediately [ d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                                      d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]
    cache <- BC.new (replicate 4 (BC.allocBytes 1 diskBufferSize) ++ replicate 4 (BC.allocBytes 1 diskBufferSize))
    return $ Sampler cache

free :: Sampler -> Server ()
free sampler = do
    S.send (g_freeAll [0])
    BC.release (bufferCache sampler)

playUnit :: Sampler -> Time -> Unit.Unit -> SynthParams -> Server ()
playUnit sampler time unit params = do
    voice <- allocVoice cache unit (\voice ->
            Just $
#if SCHEDULE_COMPLETION_BUNDLES == 1
                b_read'
                    (startVoice voice time unit params)
#else
                b_read
#endif -- SCHEDULE_COMPLETION_BUNDLES
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
                    (-1) 0 1)
    -- tu <- utcr
    -- FIXME: Why is this necessary?!
    S.unsafeSync
#if SCHEDULE_COMPLETION_BUNDLES != 1
    S.send (startVoice voice time unit params)
#endif -- SCHEDULE_COMPLETION_BUNDLES
    -- print (t-tu, t+dur-tu)
    liftIO $ OSC.pauseThreadUntil (time + dur)
    S.send $ stopVoice voice (time + dur) params
    _ <- S.waitFor $ n_end $ voiceId voice
    -- tu' <- utcr
    -- liftIO $ putStrLn ("node end: " ++ show voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        dur        = Unit.duration unit
        sourceFile = Unit.sourceFile unit
