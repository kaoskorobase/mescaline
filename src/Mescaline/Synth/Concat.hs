module Mescaline.Synth.Concat where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.Chan.Chunked
import           Control.Concurrent.STM
import           Control.Monad            (join)
import           Data.Accessor ((^.))
import           Mescaline
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.BufferCache (Buffer, BufferCache)
import qualified Mescaline.Synth.BufferCache as BC
import qualified Mescaline.Synth.Pattern as P
import qualified Mescaline.Synth.SF as SF

import qualified Sound.Analysis.Meapsoft as Meap
import qualified Sound.File.Sndfile as SF

import           Sound.SC3 hiding (free, gate, sync, uid)

import           Sound.SC3.Lang.Collection
-- import           Sound.SC3.Lang.Pattern

import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as Conn
import           Sound.SC3.Server.Monad as S
import           Sound.SC3.Server.Notification (n_end)
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.State as State

import qualified Data.Set as Set

-- import Control.Concurrent

import Sound.OpenSoundControl
import Sound.OpenSoundControl.Transport

data Voice = Voice NodeId Buffer deriving (Eq, Show)

buffer :: Voice -> Buffer
buffer (Voice _ b) = b

diskBufferSize :: Int
diskBufferSize = 8192*8

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

allocVoice :: BufferCache -> Unit.Unit -> (Voice -> Maybe OSC) -> Server Voice
allocVoice cache unit completion = do
    nid <- alloc nodeId
    buf <- BC.allocBuffer cache
                          (BC.allocBytes (SourceFile.numChannels $ Unit.sourceFile unit)
                                         diskBufferSize)
                          (\buf -> completion (Voice nid buf))
    return $ Voice nid buf

freeVoice :: BufferCache -> Voice -> Server ()
freeVoice cache (Voice _ buf) = do
    S.sync $ b_close (fromIntegral $ BC.uid buf)
    BC.freeBuffer cache buf

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

playUnit :: BufferCache -> Unit.Unit -> P.SynthParams -> Double -> Server ()
playUnit cache unit params t = do
    voice@(Voice nid buf) <- allocVoice cache unit (\voice ->
            Just $ b_read'
                    (startVoice voice unit params t)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
                    (-1) 0 1)
    -- tu <- utcr
    -- FIXME: Why is this necessary?!
    S.unsafeSync
    -- C.send conn (startVoice voice t)
    -- print (t-tu, t+dur-tu)
    liftIO $ pauseThreadUntil (t + dur)
    S.send $ stopVoice voice params (t + dur)
    S.waitFor $ n_end nid
    -- tu' <- utcr
    -- liftIO $ putStrLn ("node end: " ++ show voice)
    freeVoice cache voice
    return ()
    where
        dur = Unit.duration unit
        sourceFile = Unit.sourceFile unit
        
data Sampler = Sampler Connection BufferCache

initSampler :: Server BufferCache
initSampler = do
    S.sync $ bundle' [ d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
                       d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]
    BC.newWith (replicate 4 (BC.allocBytes 1 diskBufferSize) ++ replicate 4 (BC.allocBytes 1 diskBufferSize))

newSampler :: IO Sampler
newSampler = do
    let s = State.new Process.defaultServerOptions
    t <- Process.openTransport opts "127.0.0.1" :: IO UDP
    conn <- Conn.new s t
    cache <- runServer initSampler conn
    return (Sampler conn cache)
    where
        opts = Process.defaultRTOptionsUDP
    
freeSampler :: Sampler -> IO ()
freeSampler (Sampler conn cache) = flip runServer conn $ do
    S.send (g_freeAll [0])
    BC.free cache

playEvent :: Sampler -> P.SynthEvent -> IO ThreadId
playEvent (Sampler conn cache) e = runServer (fork $ playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) conn

runSampler :: Sampler -> Chan P.SynthEvent -> IO ()
runSampler s c = loop
    where
        loop = do
            e <- readChan c
            -- print (t, e)
            -- runServer (fork $ playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) conn
            playEvent s e
            runSampler s c

-- Disk based sampler
playPatternDisk :: Double -> P.Pattern -> Chan P.Input -> Sampler -> IO ()
playPatternDisk tick pattern ichan (Sampler conn cache) = do
    ochan <- newChan
    forkIO $ loop ochan
    P.execute tick pattern ichan ochan
    where
        loop c = readChan c >>= f >> loop c
        f (_, SF.NoEvent) = return ()
        f (_, SF.Event e) = runServer (fork (playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) >> return ()) conn

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