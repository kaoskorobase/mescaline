{-# LANGUAGE Arrows, ScopedTypeVariables #-}
module Mescaline.Synth.SSF.DiskSampler
where

import           Control.Applicative
import           Control.Arrow
import           Control.CCA.Types
import           Data.Accessor ((^.))
import qualified Data.Foldable as Fold
import qualified Data.IntMap as Map
import           Data.Monoid
import           Mescaline
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile

import           Mescaline.Synth.SSF as SF
import           Mescaline.Synth.SSF.BufferCache (Buffer, BufferCache)
import qualified Mescaline.Synth.SSF.BufferCache as BC
import           Mescaline.Pattern.Event (SynthParams)
import qualified Mescaline.Pattern.Event as P

import           Prelude hiding (init)
import qualified Prelude as P

import           Sound.OpenSoundControl (OSC(..), Time(..), immediately)

import           Sound.SC3 hiding (constant, free, gate, send, sync)
import           Sound.SC3.Lang.Collection (clump)
-- import           Sound.SC3.Lang.Pattern

import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Notification (n_end)
import qualified Sound.SC3.Server.State as State

data VoiceState = Running | Stopped | Finished deriving (Enum, Eq, Show)

data Voice = Voice {
    state  :: VoiceState
  , nodeId :: State.NodeId
  , time   :: Double
  , unit   :: Unit.Unit
  , params :: SynthParams
  , buffer :: Buffer
} deriving (Eq, Show)

finishVoice :: Voice -> Voice
finishVoice v =
    case state v of
        Stopped -> v { state = Finished }
        _       -> v

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
bundle' = Bundle immediately

startVoice :: Voice -> OSC
startVoice voice =
    bundle (time voice + params voice ^. P.latency)
        [s_new (voiceDefName $ BC.numChannels $ buffer voice) (fromIntegral $ nodeId voice) AddToTail 0
            ([ ("bufnum", fromIntegral $ BC.uid $ buffer voice),
              ("attackTime",   params voice ^. P.attackTime),
              ("releaseTime",  params voice ^. P.attackTime),
              ("sustainLevel", params voice ^. P.sustainLevel) ]
              ++
              if voiceGateEnvelope
                then []
                else [("dur", Unit.duration $ unit voice)])
            ]

stopVoice :: Voice -> OSC
stopVoice voice =
    if voiceGateEnvelope
        then bundle (time voice + params voice ^. P.latency)
                [n_set1 (fromIntegral $ nodeId voice) "gate" (params voice ^. P.gateLevel)]
        else bundle' []

-- Effectful interface

allocVoice :: SF (BufferCache, Event ((Double, Unit.Unit, SynthParams), Voice -> Maybe OSC)) (BufferCache, Event Voice)
allocVoice = proc (cache, e) -> do
    e_nid <- alloc_ State.nodeId -< e
    (cache', e_buf) <- BC.allocBuffer -< (cache, fmap (\((t, u, p), completion) nid ->
                                                        ((BC.allocBytes
                                                          (SourceFile.numChannels $ Unit.sourceFile u)
                                                          diskBufferSize)
                                                        , (completion . Voice Running nid t u p))) e <*> e_nid)
    returnA -< (cache', fmap (\nid ((t, u, p), _) -> Voice Running nid t u p) e_nid <*> e <*> e_buf)

freeVoices :: SF (BufferCache, Event [Voice]) BufferCache
freeVoices = proc (cache, e_voices) -> do
    send >>> sync -< (Bundle immediately . fmap (b_close . fromIntegral . BC.uid . buffer)) <$> e_voices
    BC.freeBufferList -< (cache, fmap buffer <$> e_voices)

broadcast :: Functor f => a -> f b -> f (a, b)
broadcast a = fmap ((,) a)

ignore :: Functor f => a -> f b -> f ((), b)
ignore _ = broadcast ()

newtype IL a = IL { unIL :: [(State.NodeId, a)] }

instance Functor IL where
    fmap f (IL xs) = IL (fmap (second f) xs)

reduceIL :: forall a d . d -> IL (d -> (a, d)) -> (IL a, d)
reduceIL s0 (IL xs) = first (IL . zip (map fst xs))
                        $ Fold.foldl'
                            (\(cs, s) f -> let (c, s') = f s in (c : cs, s'))
                            ([], s0)
                            (map snd xs)

filterIL :: (a -> Bool) -> IL a -> IL a
filterIL f = IL . P.filter (f . snd) . unIL

type VoiceSF = SF () Voice
type Sampler = IL Voice
data AllocVoice = AllocVoice Double Unit.Unit P.SynthParams

voiceSF :: Voice -> VoiceSF
voiceSF voice =
    switch (pure voice &&& switchEvt voice) $ \voice -> pure $ voice { state = Stopped }
    where
        t = time voice
        dur = Unit.duration (unit voice)
        switchEvt voice =
                once voice
            >>> delayEvent_ (t + dur)
            >>> arr (fmap stopVoice)
            >>> send
            >>> waitFor (n_end (nodeId voice))
            >>> tag voice

updateSampler :: (Event Voice, IL Voice) -> Event (IL VoiceSF -> IL VoiceSF)
updateSampler (e_voice, (IL voices)) =
    Event $
        IL
        -- Add voice in event to collection
      . (event id (\voice -> ((nodeId voice, voiceSF voice):)) e_voice)
        -- Filter voices with state Finished
      . (P.filter (not . (flip elem finished) . fst))
        -- Update state to Finished for voices with state Stopped
      . fmap (\(nid, sf) ->
            if nid `elem` stopped
            then (nid, fmap (\voice -> voice { state = Finished }) sf)
            else (nid, sf))
       . unIL
    where
        finished = fmap fst . P.filter ((== Finished) . state . snd) $ voices
        stopped  = fmap fst . P.filter ((== Stopped)  . state . snd) $ voices

samplerCore :: IL VoiceSF -> SF (Event Voice) (Sampler, Event [Voice])
samplerCore vs =     core vs
                 >>>      arr (filterIL ((== Running) . state))
                      &&& arr (ilToEvent . filterIL ((== Finished) . state))
    where
        core vs = pSwitch
                    ignore reduceIL vs
                    (arr updateSampler >>> init NoEvent)
                    (\vs' f -> core (f vs'))
        ilToEvent (IL []) = NoEvent
        ilToEvent (IL xs) = Event $ fmap snd xs

sampler :: SF (Event AllocVoice) Sampler
sampler =
    proc action -> do
        rec
            bc <- init BC.newEmpty -< bc''
            -- Alloc voice
            (bc', e_voice) <- allocVoice -< (bc, fmap (\(AllocVoice t u p) -> ((t, u, p), completion)) action)
            -- Free finished voices, fed back from samplerCore's output
            bc'' <- freeVoices -< (bc', e_finished)
            -- Get running and finished voices
            (vs, e_finished) <- samplerCore (IL []) -< e_voice
        returnA -< vs
    where
        -- Hmm, how to generalize from lists?
        completion voice =
            Just $ b_read'
                    (startVoice voice)
                    (fromIntegral $ BC.uid $ buffer voice)
                    (SourceFile.path sourceFile)
                    (truncate $ SourceFile.sampleRate sourceFile * Unit.onset (unit voice))
                    (-1) 0 1
            where
                sourceFile = Unit.sourceFile (unit voice)

-- voice :: SF (Event (Action, Buffer)) Voice
-- -- voice = undefined
-- voice = proc (cache, e) -> do
--     
--     voice@(Voice nid buf) <- allocVoice cache unit (\voice ->
--             Just $ b_read'
--                     (startVoice voice unit params t)
--                     (fromIntegral $ BC.uid $ buffer voice)
--                     (SourceFile.path sourceFile)
--                     (truncate $ SourceFile.sampleRate sourceFile * Unit.onset unit)
--                     (-1) 0 1)
--     -- tu <- utcr
--     -- FIXME: Why is this necessary?!
--     S.unsafeSync
--     -- C.send conn (startVoice voice t)
--     -- print (t-tu, t+dur-tu)
--     liftIO $ pauseThreadUntil (t + dur)
--     S.send $ stopVoice voice params (t + dur)
--     S.waitFor $ n_end nid
--     -- tu' <- utcr
--     -- liftIO $ putStrLn ("node end: " ++ show voice)
--     freeVoice cache voice
--     return ()
--     where
--         dur = Unit.duration unit
--         sourceFile = Unit.sourceFile unit

-- data Sampler = Sampler Connection BufferCache
-- 
-- initSampler :: Server BufferCache
-- initSampler = do
--     S.sync $ bundle' [ d_recv (synthdef (voiceDefName 1) (voiceDef 1)),
--                        d_recv (synthdef (voiceDefName 2) (voiceDef 2)) ]
--     BC.newWith (replicate 4 (BC.allocBytes 1 diskBufferSize) ++ replicate 4 (BC.allocBytes 1 diskBufferSize))
-- 
-- newSampler :: IO Sampler
-- newSampler = do
--     let s = State.new Process.defaultServerOptions
--     t <- Process.openTransport opts "127.0.0.1" :: IO UDP
--     conn <- Conn.new s t
--     cache <- runServer initSampler conn
--     return (Sampler conn cache)
--     where
--         opts = Process.defaultRTOptionsUDP
--     
-- freeSampler :: Sampler -> IO ()
-- freeSampler (Sampler conn cache) = flip runServer conn $ do
--     S.send (g_freeAll [0])
--     BC.free cache
-- 
-- playEvent :: Sampler -> P.SynthEvent -> IO ThreadId
-- playEvent (Sampler conn cache) e = runServer (fork $ playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) conn
-- 
-- runSampler :: Sampler -> Chan P.SynthEvent -> IO ()
-- runSampler s c = loop
--     where
--         loop = do
--             e <- readChan c
--             -- print (t, e)
--             -- runServer (fork $ playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) conn
--             playEvent s e
--             runSampler s c
-- 
-- -- Disk based sampler
-- playPatternDisk :: Double -> P.Pattern -> Chan P.Input -> Sampler -> IO ()
-- playPatternDisk tick pattern ichan (Sampler conn cache) = do
--     ochan <- newChan
--     forkIO $ loop ochan
--     P.execute tick pattern ichan ochan
--     where
--         loop c = readChan c >>= f >> loop c
--         f (_, SF.NoEvent) = return ()
--         f (_, SF.Event e) = runServer (fork (playUnit cache (e ^. P.unit) (e ^. P.synth) (e ^. P.time)) >> return ()) conn
