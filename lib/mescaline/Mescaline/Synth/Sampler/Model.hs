module Mescaline.Synth.Sampler.Model (
    Sampler
  , Options(..)
  , new
  , free
  , playUnit
) where

import           Control.Monad.Reader
import           Data.Accessor
import           Data.Maybe (mapMaybe)
import           Mescaline (Duration, Time)
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database as DB
import           Mescaline.Synth.BufferCache.Server (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache.Server as BC
import           Mescaline.Synth.Sampler.Params (Params)
import qualified Mescaline.Synth.Sampler.Params as P
import           Sound.OpenSoundControl (OSC(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 hiding (Output, free, gate, sync)
import           Sound.SC3.Lang.Collection.SequenceableCollection
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Monad (BusId, NodeId, Server, waitFor)
import qualified Sound.SC3.Server.Monad as S
import           Sound.SC3.Server.Notification (n_end)
import           System.Directory
import           System.FilePath

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
output x = mrg [ offsetOut (control KR "out" 0) x
               , out (control KR "sendBus1" 102) (x * control KR "sendLevel1" 0)
               , out (control KR "sendBus2" 104) (x * control KR "sendLevel2" 0) ]

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

data SynthBounds = SynthBounds {
    onset       :: {-# UNPACK #-} !Time
  , duration    :: {-# UNPACK #-} !Duration
  , attackTime  :: {-# UNPACK #-} !Duration
  , sustainTime :: {-# UNPACK #-} !Duration
  , releaseTime :: {-# UNPACK #-} !Duration
  }

synthBounds :: Params -> SynthBounds
synthBounds params =
    SynthBounds
        (DB.unitOnset (P.unit params) + P.offset params)
        dur atime' stime' rtime'
    where
        dur    = P.duration params
        -- dur'   = dur - synth ^. P.offset
        atime  = max 0 (P.attackTime params)
        rtime  = max 0 (P.releaseTime params)
        stime  = dur - (atime + rtime)
        (atime', stime', rtime') =
            if stime < 0
                then if atime == 0
                     then (0, 0, min dur rtime)
                     else if rtime == 0
                          then (min dur atime, 0, 0)
                          else let r = atime / (atime + rtime)
                               in (dur * r, 0, dur * (1 - r))
                else (atime, stime, rtime)

data Effect = Effect {
    fxId      :: Int
  , fxName    :: String
  , fxPath    :: FilePath
  , sendBus   :: BusId
  , fxNode    :: Maybe NodeId
  , fxParam   :: Params -> Double
  }

mkFxPath :: String -> FilePath -> FilePath
mkFxPath name defDir = defDir </> (name ++ ".scsyndef")

newEffect :: FilePath -> Int -> (Params -> Double) -> String -> Server Effect
newEffect defDir i acc name = do
    let path = mkFxPath name defDir
    e <- liftIO $ doesFileExist path
    nid <- if e then liftM Just (S.alloc S.nodeIdAllocator) else return Nothing
    -- FIXME: Bus hardcoded for now, because allocRange is not implemented yet
    -- bus <- S.alloc (S.busId AR)
    let bus = fromIntegral (100 + i*2)
    return $ Effect i name path bus nid acc

fxLoadMsg :: Effect -> Maybe OSC
fxLoadMsg fx =
    case fxNode fx of
        Nothing -> Nothing
        Just nid -> Just $ d_load'
                            (s_new (fxName fx) (fromIntegral nid) AddToTail 0
                             [ ("sendBus", fromIntegral (sendBus fx)) ])
                            (fxPath fx)

sendBusArg :: Effect -> (String, Double)
sendBusArg fx = ("sendBus" ++ show (fxId fx), fromIntegral (sendBus fx))

fxParamMsg :: Params -> Effect -> Maybe OSC
fxParamMsg params fx =
    case fxNode fx of
        Nothing -> Nothing
        Just nid -> Just $ n_set1 (fromIntegral nid) "param" (fxParam fx params)

startVoice :: Voice -> Time -> Params -> SynthBounds -> [Effect] -> OSC
startVoice voice time params bounds effects =
    let timeTag = if time <= 0
                  then OSC.immediately
                  else OSC.UTCr time
    in Bundle timeTag $
        [ s_new (voiceDefName $ BC.numChannels $ buffer voice) (fromIntegral $ voiceId voice) AddToHead 0
            ([ ("bufnum", fromIntegral $ BC.uid $ buffer voice)
             , ("rate", P.rate params)
             , ("amp", P.sustainLevel params)
             , ("pan", P.pan params)
             , ("attackTime", attackTime bounds)
             , ("releaseTime", releaseTime bounds)
             , ("sendLevel1", P.sendLevel1 params)
             , ("sendLevel2", P.sendLevel2 params)
             ]
             ++
               if voiceGateEnvelope
                  then []
                  else [ ("dur", sustainTime bounds) ]
             ++ map sendBusArg effects) ]
        ++ mapMaybe (fxParamMsg params) effects

stopVoice :: Voice -> Time -> Params -> OSC
stopVoice voice time params =
    if voiceGateEnvelope
        then Bundle (OSC.UTCr time)
                [ n_set1 (fromIntegral $ voiceId voice) "gate" (P.gateLevel params) ]
        else Bundle OSC.immediately []

allocVoice :: BufferCache -> DB.SourceFile -> (Voice -> Maybe OSC) -> Server Voice
allocVoice cache file completion = do
    nid <- S.alloc S.nodeIdAllocator
    buf <- BC.allocBuffer
            (completion . Voice nid)
            cache
            (BC.allocBytes
                (DB.sourceFileNumChannels file)
                diskBufferSize)
    return $ Voice nid buf

freeVoice :: BufferCache -> Voice -> Server ()
freeVoice cache voice = do
    S.free S.nodeIdAllocator (voiceId voice)
    S.sync $ b_close (fromIntegral (BC.uid (buffer voice)))
    BC.freeBuffer cache (buffer voice)

data Sampler = Sampler {
    bufferCache  :: !BufferCache
  , effects :: [Effect]
  , playUnitFunc :: !(Sampler -> Time -> Params -> Server ())
  }

data Options = Options {
    synthdefDir :: FilePath
  , sendEffect1 :: Maybe String
  , sendEffect2 :: Maybe String
  , scheduleCompletionBundles :: Bool
  } deriving (Eq, Show)

new :: Options -> Server Sampler
new opts = do
    fx1 <- newEffect (synthdefDir opts) 1 P.fxParam1 (maybe "" id (sendEffect1 opts))
    fx2 <- newEffect (synthdefDir opts) 2 P.fxParam2 (maybe "" id (sendEffect2 opts))
    let fx = [fx1, fx2]

    -- Load synthdefs
    S.sync $ Bundle OSC.immediately $ [ d_recv (synthdef (voiceDefName 1) (voiceDef 1))
                                      , d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]
                                      ++ mapMaybe fxLoadMsg fx

    -- Pre-allocate disk buffers, mono and stereo
    let nb = 64
    cache <- BC.new $
        replicate nb (BC.allocBytes 1 diskBufferSize)
     ++ replicate nb (BC.allocBytes 2 diskBufferSize)

    return $ Sampler cache fx (if scheduleCompletionBundles opts then playUnit_schedComplBundles else playUnit_noSchedComplBundles)

free :: Sampler -> Server ()
free sampler = do
    S.send $ g_freeAll [0]
    BC.release (bufferCache sampler)

playUnit :: Sampler -> Time -> Params -> Server ()
playUnit sampler = (playUnitFunc sampler) sampler

playUnit_noSchedComplBundles :: Sampler -> Time -> Params -> Server ()
playUnit_noSchedComplBundles sampler time params = do
    voice <- allocVoice cache sourceFile (const Nothing)
    let bounds = synthBounds params
    S.sync $ b_read
               (fromIntegral (BC.uid (buffer voice)))
               (DB.sourceFileUrl sourceFile)
               (truncate (DB.sourceFileSampleRate sourceFile * onset bounds))
               (-1) 0 True
    startVoice voice time params bounds (effects sampler)
        `waitFor` n_end (voiceId voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = P.unit params
        sourceFile = P.file params

playUnit_schedComplBundles :: Sampler -> Time -> Params -> Server ()
playUnit_schedComplBundles sampler time params = do
    let bounds = synthBounds params
    voice <- allocVoice cache sourceFile (const Nothing)
    b_read'
        (startVoice voice time params bounds (effects sampler))
        (fromIntegral (BC.uid (buffer voice)))
        (DB.sourceFileUrl sourceFile)
        (truncate (DB.sourceFileSampleRate sourceFile * onset bounds))
        (-1) 0 True
        `waitFor` n_end (voiceId voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = P.unit params
        sourceFile = P.file params
