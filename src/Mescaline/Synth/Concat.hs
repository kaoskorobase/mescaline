module Mescaline.Synth.Concat where

import           Data.Accessor ((^.))
import           Mescaline
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache as BC
import qualified Mescaline.Synth.Pattern as P

import Sound.SC3 hiding (free, gate, sync, uid)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Process

import qualified Data.Set as Set

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad            (join)

import Sound.OpenSoundControl
import Sound.OpenSoundControl.Transport

import           Sound.SC3.Lang.Collection
import           Sound.SC3.Lang.Pattern
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection  as C
import qualified Sound.File.Sndfile as SF

import qualified Sound.Analysis.Meapsoft as Meap

data Voice = Voice State.NodeId Buffer

buffer :: Voice -> Buffer
buffer (Voice _ b) = b

diskBufferSize :: Int
diskBufferSize = 8192*8*4

-- | Attack-sustain-release envelope parameter constructor.
asr :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
asr aT sL rT c = env [0, sL, 0] [aT, rT] c 1 (-1)

voiceGateEnvelope :: Bool
voiceGateEnvelope = False

voiceEnvGate :: UGen
voiceEnvGate = envGen AR gate 1 0 1 RemoveSynth (asr attackTime sustainLevel releaseTime [EnvLin])
    where gate         = control KR "gate" 1
          sustainLevel = control KR "sustainLevel" 1
          attackTime   = control KR "attackTime" 0
          releaseTime  = control KR "releaseTime" 0

voiceEnvFixed :: UGen
voiceEnvFixed = envGen AR 1 1 0 1 RemoveSynth (envLinen attackTime dur releaseTime sustainLevel)
    where dur          = control KR "dur" 0
          sustainLevel = control KR "sustainLevel" 1
          attackTime   = control KR "attackTime" 0
          releaseTime  = control KR "releaseTime" 0

voiceEnv :: UGen
voiceEnv = if voiceGateEnvelope then voiceEnvGate else voiceEnvFixed

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

bundle :: Double -> [OSC] -> OSC
bundle  = Bundle . UTCr

bundle' :: [OSC] -> OSC
bundle' = Bundle (NTPi 1)

allocVoice :: Transport t => Connection t -> BufferCache -> Unit.Unit -> (Voice -> Maybe OSC) -> IO Voice
allocVoice conn cache unit completion = do
    nid <- atomically (State.alloc (State.nodeId $ C.state conn))
    buf <- BC.allocBuffer conn cache (SourceFile.numChannels $ Unit.sourceFile unit) (\buf -> completion (Voice nid buf))
    return $ Voice nid buf

freeVoice :: Transport t => Connection t -> BufferCache -> Voice -> IO ()
freeVoice conn cache (Voice _ buf) = do
    C.sync conn $ b_close (fromIntegral $ BC.uid buf)
    BC.freeBuffer conn cache buf

startVoice :: Voice -> Unit.Unit -> P.SynthParams -> Double -> OSC
startVoice (Voice nid buf) unit params time =
    bundle (time + params ^. P.latency)
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

stopVoice :: Voice -> P.SynthParams -> Double -> OSC
stopVoice (Voice nid _) params time =
    if voiceGateEnvelope
        then bundle (time + params ^. P.latency)
                [n_set1 (fromIntegral nid) "gate" (params ^. P.gateLevel)]
        else bundle' []

playUnit :: Transport t => Connection t -> BufferCache -> Unit.Unit -> P.SynthParams -> Double -> IO ()
playUnit conn cache unit params t = do
    voice@(Voice nid buf) <- allocVoice conn cache unit (\voice ->
            Just $ b_read'
                    (startVoice voice unit params t)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
                    (-1) 0 1)
    -- tu <- utcr
    C.unsafeSync conn -- Why is this necessary?!
    -- C.send conn (startVoice voice t)
    -- print (t-tu, t+dur-tu)
    pauseThreadUntil (t + dur)
    C.send conn $ stopVoice voice params (t + dur)
    C.waitFor conn $ nodeEnded $ fromIntegral nid
    -- tu' <- utcr
    -- putStrLn ("node end: " ++ show (tu' - tu))
    freeVoice conn cache voice
    return ()
    where
        dur = Unit.duration unit
        sourceFile = Unit.sourceFile unit
        nodeEnded i (Message "/n_end" [Int j, _, _, _, _]) = j == i
        nodeEnded _ _                                      = False
        

eitherToIO :: Either String a -> IO a
eitherToIO e = case e of
                    Left l -> error l
                    Right r -> return r

p1 l = reverse $ take 8 $ drop (length l `div` 3) l
p2 l = (take 4 l') ++ (reverse $ take 4 $ drop 4 l')
    where l' = drop (length l `div` 3 * 2) l

data Sampler t = Sampler (Connection t) BufferCache

initSampler :: Transport t => Connection t -> IO BufferCache
initSampler conn = do
    C.send conn $ bundle' [notify True]
                            -- , dumpOSC TextPrinter]
    C.sync conn $ bundle' [d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                           d_recv (synthdef (voiceDefName 2) (voiceDef 2))]
    BC.newWith conn diskBufferSize (replicate 4 1 ++ replicate 4 2)

newSampler :: IO (Sampler UDP)
newSampler = do
    s <- atomically (State.newState Process.defaultServerOptions)
    t <- Process.openTransport opts "127.0.0.1" :: IO UDP
    conn <- C.new s t
    cache <- initSampler conn
    return (Sampler conn cache)
    where
        opts = Process.defaultRTOptionsUDP
    
freeSampler :: Transport t => Sampler t -> IO ()
freeSampler (Sampler conn cache) = do
    C.send conn (g_freeAll [0])
    BC.free conn cache

-- Disk based sampler
playPatternDisk :: Sampler UDP -> P.Pattern -> IO ()
playPatternDisk (Sampler conn cache) = P.execute f
    where
        opts = Process.defaultRTOptionsUDP
        f e t = C.fork conn (\conn' -> playUnit conn' cache (e ^. P.unit) (e ^. P.synth) t) >> return ()

-- Memory based sampler
-- fixSegDur segs = (zipWith (\(Segment o _) d -> Segment o d) segs (zipWith (\(Segment o1 _) (Segment o2 _) -> o2 - o1) segs (tail segs)))
-- 
-- playFileMem :: Sampler UDP -> FilePath -> IO ()
-- playFileMem (Sampler conn cache) path = do
--     C.send conn (notify True) -- FIXME: Race condition!
--     bid <- atomically $ (State.alloc (State.bufferId $ C.state conn) :: STM State.BufferId)
--     C.sync conn (Bundle (NTPi 1) [d_recv (synthdef (voiceDefName 1) (voiceDefMem 1)),
--                                   d_recv (synthdef (voiceDefName 2) (voiceDefMem 2)),
--                                   b_allocRead (fromIntegral bid) path 0 (-1)])
-- 
--     putStrLn "playFileMem: Buffers loaded"
-- 
--     sf@(SoundFile _ info) <- openSoundFile path
--     segs <- eitherToIO =<< readSegments (path ++ ".seg")
--     utc0 <- utcr
--     time <- newIORef utc0
--     let onset0 = seg_onset (head segs)
-- 
--     mapM_ (\seg@(Segment onset dur) -> do
--             utc <- readIORef time
--             -- let utc = utc0 + onset - onset0
--             -- print (utc - utc0 + onset0)
--             -- print (round ((seg_onset seg - (utc - utc0 + onset0)) * 1000))
--             nid <- atomically $ (State.alloc (State.nodeId $ C.state conn) :: STM State.NodeId)
--             C.send conn $ Bundle (UTCr (utc+latency)) [
--                             s_new (voiceDefName (SF.channels info)) (fromIntegral nid) AddToTail 0
--                                   [("bufnum", fromIntegral bid), ("start", fromIntegral (SF.samplerate info) * onset),("dur", dur)]
--                                   ]
--             pauseThreadUntil (utc+dur)
--             C.send conn $ Bundle (UTCr (utc+dur+latency)) [
--                             n_set1 (fromIntegral nid) "gate" 0
--                             ]
--             modifyIORef time (+dur)
--             )
--         (segs)
--     where
--         opts = Process.defaultRTOptionsUDP

playPattern = playPatternDisk