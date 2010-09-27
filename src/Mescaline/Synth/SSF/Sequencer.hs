-- TODO: Implement start time quantization based on master clock signal.
clock :: SF Double (Event Time)
clock = (logicalTime &&& id) >>> scanl f (Nothing, NoEvent) >>> arr snd
    where
        f (Nothing, _) (globalTime, tick)
            = (Just (globalTime+tick), Event globalTime)
        f (Just localTime, _) (globalTime, tick)
            | localTime <= globalTime = (Just $ localTime+tick, Event localTime)
            | otherwise               = (Just $ localTime, NoEvent)

-- | Left-biased event merge.
mergeList :: Event a -> Event a -> Event [a]
mergeList NoEvent   NoEvent   = NoEvent
mergeList (Event l) NoEvent   = Event [l]
mergeList NoEvent   (Event r) = Event [r]
mergeList (Event l) (Event r) = Event [l, r]

type Update a = Event (a -> a)
type Changed a = Event a

sequencerSF :: Sequencer a -> SF (Update (Sequencer a)) (Event (Time, Sequencer a), Changed (Sequencer a))
sequencerSF s0 =
    proc update -> do
        rec
            c <- clock -< t
            e <- tag (Sequencer.step (undefined::Score)) -< c
            s <- accum s0 -< (foldl (.) id `fmap` (update `mergeList` e))
            t <- hold s0 >>> arr (getVal Sequencer.tick) -< s
        returnA -< (liftA2 (,) c s, s)

-- sequencerEvents :: [Unit] -> Time -> Sequencer a -> [P.SynthEvent]
sequencerEvents units t s = map (setEnv.f) is
    where
        -- is = map (\(r, c) -> r * cols s + c) $ indicesAtCursor s
        is = map fst $ indicesAtCursor s
        f i = P.SynthEvent t (units !! i) P.defaultSynth

-- Execute signal function
-- t <- Server.openTransport Server.defaultRTOptionsUDP "127.0.0.1" :: IO UDP
-- forkIO $ SF.execute (SF.Options Server.defaultServerOptions t 0.005)
--     (sequencer sequencer0 >>> first (arr (fmap (uncurry (sequencerEvents (map fst units)))))) ichan ochan
-- forkIO $ SF.execute (SF.Options Server.defaultServerOptions t 0.005)
--     (sequencer sequencer0) ichan ochan

-- Dispatch events to and from sequencer view
-- forkIO $ fix $ \loop -> do
--     x <- readChan ochan
--     case (fst.snd $ x) of
--         NoEvent -> return ()
--         -- Event es -> mapM_ (Synth.playEvent synth) es
--         Event (t, s) -> do
--             let is = map fst $ indicesAtCursor s
--             mapM_ (writeChan fspace_ichan . FeatureSpaceView.ActivateRegion . (,) t) is
--     case (snd.snd $ x) of
--         NoEvent -> return ()
--         Event s -> writeChan seq_ichan s
--     loop
