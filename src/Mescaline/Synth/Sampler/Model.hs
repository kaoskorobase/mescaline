{-# LANGUAGE CPP #-}

#define SCHEDULE_COMPLETION_BUNDLES 0

module Mescaline.Synth.Sampler.Model (
    Sampler
  , new
  , free
  , playUnit
) where

import           Control.Monad.Reader
import           Data.Accessor
import           Data.Maybe (mapMaybe)
import           Mescaline (Duration, Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database.Entity as DB
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache.Server (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache.Server as BC
import qualified Mescaline.FeatureSpace.Unit as Unit
import           Mescaline.Pattern.Event (Synth)
import qualified Mescaline.Pattern.Event as P
import           Sound.OpenSoundControl (OSC(..))
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 hiding (Output, free, gate, sync)
import           Sound.SC3.Lang.Collection
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Monad (BusId, NodeId, Server)
import           Sound.SC3.Server.Monad (syncWith)
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

synthBounds :: Synth -> SynthBounds
synthBounds synth =
    SynthBounds
        (Unit.onset (synth ^. P.unit) + synth ^. P.offset)
        dur atime' stime' rtime'
    where
        dur    = synth ^. P.duration
        -- dur'   = dur - synth ^. P.offset
        atime  = max 0 (synth ^. P.attackTime)
        rtime  = max 0 (synth ^. P.releaseTime)
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
  , fxParam   :: Accessor Synth Double
  }

mkFxPath :: String -> FilePath -> FilePath
mkFxPath name defDir = defDir </> (name ++ ".scsyndef")

newEffect :: Int -> Accessor Synth Double -> String -> Server Effect
newEffect i acc name = do
    defDir <- liftIO $ App.getUserDataPath "synthdefs"
    let path = mkFxPath name defDir
    e <- liftIO $ doesFileExist path
    nid <- if e then liftM Just (S.alloc S.nodeId) else return Nothing
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

fxParamMsg :: Synth -> Effect -> Maybe OSC
fxParamMsg synth fx =
    case fxNode fx of
        Nothing -> Nothing
        Just nid -> Just $ n_set1 (fromIntegral nid) "param" (getVal (fxParam fx) synth)

startVoice :: Voice -> Time -> Synth -> SynthBounds -> [Effect] -> OSC
startVoice voice time synth bounds effects =
    let timeTag = if time <= 0
                  then OSC.immediately
                  else OSC.UTCr (time + synth ^. P.latency)
    in Bundle timeTag $
        [ s_new (voiceDefName $ BC.numChannels $ buffer voice) (fromIntegral $ voiceId voice) AddToHead 0
            ([ ("bufnum", fromIntegral $ BC.uid $ buffer voice)
             , ("rate", synth ^. P.rate)
             , ("amp", synth ^. P.sustainLevel)
             , ("pan", synth ^. P.pan)
             , ("attackTime", attackTime bounds)
             , ("releaseTime", releaseTime bounds)
             , ("sendLevel1", synth ^. P.sendLevel1)
             , ("sendLevel2", synth ^. P.sendLevel2)
             ]
             ++
               if voiceGateEnvelope
                  then []
                  else [ ("dur", sustainTime bounds) ]
             ++ map sendBusArg effects) ]
        ++ mapMaybe (fxParamMsg synth) effects

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
                (DB.sourceFileNumChannels (Unit.sourceFile unit))
                diskBufferSize)
    return $ Voice nid buf

freeVoice :: BufferCache -> Voice -> Server ()
freeVoice cache voice = do
    S.free S.nodeId (voiceId voice)
    S.sync $ b_close (fromIntegral (BC.uid (buffer voice)))
    BC.freeBuffer cache (buffer voice)

data Sampler = Sampler {
    bufferCache  :: !BufferCache
  , effects :: [Effect]
  , playUnitFunc :: !(Sampler -> Time -> Synth -> Server ())
  }

new :: Server Sampler
new = do
    conf <- liftIO Config.getConfig

    let b = either (const False) id (Config.get conf "Synth" "scheduleCompletionBundles")
    liftIO $ Log.noticeM "Synth" $ (if b then "U" else "Not u") ++ "sing completion bundle scheduling."


    fx1 <- newEffect 1 P.fxParam1 (either (const "") id (Config.get conf "Synth" "sendEffect1"))
    fx2 <- newEffect 2 P.fxParam2 (either (const "") id (Config.get conf "Synth" "sendEffect2"))
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

    return $ Sampler cache fx (if b then playUnit_schedComplBundles else playUnit_noSchedComplBundles)

free :: Sampler -> Server ()
free sampler = do
    S.async $ g_freeAll [0]
    BC.release (bufferCache sampler)

playUnit :: Sampler -> Time -> Synth -> Server ()
playUnit sampler = (playUnitFunc sampler) sampler

playUnit_noSchedComplBundles :: Sampler -> Time -> Synth -> Server ()
playUnit_noSchedComplBundles sampler time synth = do
    voice <- allocVoice cache unit (const Nothing)
    let bounds = synthBounds synth
    S.sync $ b_read
               (fromIntegral (BC.uid (buffer voice)))
               (DB.sourceFileUrl sourceFile)
               (truncate (DB.sourceFileSampleRate sourceFile * onset bounds))
               (-1) 0 1
    startVoice voice time synth bounds (effects sampler)
        `syncWith` n_end (voiceId voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = synth ^. P.unit
        sourceFile = Unit.sourceFile unit

playUnit_schedComplBundles :: Sampler -> Time -> Synth -> Server ()
playUnit_schedComplBundles sampler time synth = do
    let bounds = synthBounds synth
    voice <- allocVoice cache unit (const Nothing)
    b_read'
        (startVoice voice time synth bounds (effects sampler))
        (fromIntegral (BC.uid (buffer voice)))
        (DB.sourceFileUrl sourceFile)
        (truncate (DB.sourceFileSampleRate sourceFile * onset bounds))
        (-1) 0 1
        `syncWith` n_end (voiceId voice)
    freeVoice cache voice
    return ()
    where
        cache      = bufferCache sampler
        unit       = synth ^. P.unit
        sourceFile = Unit.sourceFile unit
