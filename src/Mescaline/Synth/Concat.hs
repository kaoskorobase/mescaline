module Mescaline.Synth.Concat where

import           Mescaline
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache as BC
import qualified Mescaline.Synth.Pattern as P

import Sound.SC3 hiding (free, sync, uid)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Process

import qualified Data.Set as Set

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad            (join)

import Sound.OpenSoundControl
import Sound.OpenSoundControl.Transport

import           Sound.SC3.Lang.Pattern
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection  as C
import qualified Sound.File.Sndfile as SF

import qualified Sound.Analysis.Meapsoft as Meap

import Data.IORef

data Voice = Voice State.NodeId Buffer

allocVoice :: Transport t => Connection t -> BufferCache -> Unit.Unit -> (Voice -> Maybe OSC) -> IO Voice
allocVoice conn cache unit completion = do
    nid <- atomically (State.alloc (State.nodeId $ C.state conn))
    buf <- BC.allocBuffer conn cache (SourceFile.numChannels $ Unit.sourceFile unit) (\buf -> completion (Voice nid buf))
    return $ Voice nid buf

freeVoice :: Transport t => Connection t -> BufferCache -> Voice -> IO ()
freeVoice conn cache (Voice _ buf) = do
    forkIO $ do
        C.sync conn (b_close (fromIntegral $ BC.uid buf))
        BC.freeBuffer conn cache buf
    return ()

buffer :: Voice -> Buffer
buffer (Voice _ b) = b

voiceDefName :: Int -> String
voiceDefName 1  = "es.globero.mescaline.voice_1"
voiceDefName 2  = "es.globero.mescaline.voice_2"
voiceDefName nc = "es.globero.mescaline.voice_" ++ (show nc)

diskBufferSize :: Int
diskBufferSize = 8192*8*4
    
latency :: Double
latency = 0.05

startVoice :: Voice -> Double -> OSC
startVoice (Voice nid buf) time =
    Bundle (UTCr (time+latency)) [s_new (voiceDefName $ BC.numChannels buf) (fromIntegral nid) AddToTail 0 [("bufnum", fromIntegral $ BC.uid buf)]]

stopVoice :: Voice -> Double -> OSC
stopVoice (Voice nid _) time =
    Bundle (UTCr (time+latency)) [n_set1 (fromIntegral nid) "gate" 0]

-- | Attack-sustain-release envelope parameter constructor.
asr :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
asr aT sL rT c = env [0, sL, 0] [aT, rT] c 1 (-1)

attack = 0.0
release = 0.0
curve = EnvLin
envelope = envGen AR (control KR "gate" 1) 1 0 1 RemoveSynth (asr attack 1 release [curve])

voiceDef n    = offsetOut 0 (vDiskIn n (control KR "bufnum" (-1)) 1 NoLoop * envelope)
voiceDefMem n = offsetOut 0 (playBuf n (control KR "bufnum" (-1)) 1 1 (control KR "start" (0)) NoLoop DoNothing * envelope)
    
initSampler :: Transport t => Connection t -> IO BufferCache
initSampler conn = do
    C.send conn (notify True) -- FIXME: Race condition!
    C.sync conn (Bundle (NTPi 1) [d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                                  d_recv (synthdef (voiceDefName 2) (voiceDef 2))])
    BC.newWith conn diskBufferSize (replicate 4 1 ++ replicate 4 2)

playUnit :: Transport t => Connection t -> BufferCache -> Unit.Unit -> Double -> IO ()
playUnit conn cache unit t = do
    voice@(Voice nid buf) <- allocVoice conn cache unit (\voice ->
            Just $ b_read'
                    (startVoice voice t)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
                    (-1) 0 1)
    -- C.send conn (startVoice voice t)
    pauseThreadUntil (t + dur)
    C.send conn $ stopVoice voice (t + dur)
    C.waitFor conn $ nodeEnded $ fromIntegral nid
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
        f e t = C.fork conn (\conn' -> playUnit conn' cache (P.unit e) t) >> return ()

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