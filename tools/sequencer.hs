{-# LANGUAGE Arrows #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Data.Accessor
import           Data.Function (fix)
import           Data.Maybe
import           Database.HDBC (quickQuery')
import           Graphics.Rendering.Cairo (Render)
import           Graphics.UI.Gtk hiding (Image, Point)
import           Mescaline (Time)
import qualified Mescaline.Database as DB
import           Mescaline.Database.SqlQuery
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Concat as Synth
import qualified Mescaline.Synth.Pattern as P
import           Mescaline.Synth.Sequencer as Sequencer
import           Mescaline.Synth.SequencerView
import           Mescaline.Synth.SSF
import           Sound.OpenSoundControl hiding (Time)
import qualified System.Environment as Env
import           Prelude hiding (and, init, scanl)

import Debug.Trace

-- TODO: Implement start time quantization based on master clock signal.
clock :: SSF Double (Event Time)
clock = (logicalTime &&& identity) >>> scanl f (Nothing, NoEvent) >>> arr snd
    where
        f (Nothing, _) (globalTime, tick)
            = (Just (globalTime+tick), Event globalTime)
        f (Just localTime, _) (globalTime, tick)
            | localTime <= globalTime = (Just $ localTime+tick, Event localTime)
            | otherwise               = (Just $ localTime, NoEvent)

sequencer0 :: Sequencer ()
sequencer0 = Sequencer.cons 16 16 0.125 (Bar (-1))

sequencerOld :: SSF Double (Event (Sequencer ()))
sequencerOld = clock >>> tag (Sequencer.step (undefined::Score)) >>> accum sequencer0

accumHold :: a -> SSF (Event (a -> a)) a
accumHold a0 = scanl g a0
    where
        g a NoEvent   = a
        g a (Event f) = f a

-- Event merge paramterezied on the conflict resolution function.
mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _ NoEvent      NoEvent      = NoEvent
mergeBy _ le@(Event _) NoEvent      = le
mergeBy _ NoEvent      re@(Event _) = re
mergeBy f (Event l)    (Event r)    = Event (f l r)

-- Unbiased event merge: simultaneous occurrence is an error.
merge :: Event a -> Event a -> Event [a]
merge NoEvent   NoEvent   = NoEvent
merge (Event l) NoEvent   = Event [l]
merge NoEvent   (Event r) = Event [r]
merge (Event l) (Event r) = Event [l, r]

type Update a = Event (a -> a)
type Changed a = Event a

traceEvent e@NoEvent   = traceShow "traceEvent: NoEvent" e
traceEvent e@(Event _) = traceShow "traceEvent: Event"   e

sequencer :: Sequencer a -> SSF (Update (Sequencer a)) (Event (Time, Sequencer a), Changed (Sequencer a))
sequencer s0 =
    proc update -> do
        rec
            c <- clock -< t
            e <- tag (Sequencer.step (undefined::Score)) -< c
            s <- accum s0 -< (foldl (.) id `fmap` (update `merge` e))
            t <- hold s0 >>> arr (getVal Sequencer.tick) -< s
        returnA -< (liftA2 (,) c s, s)
-- sequencer s0 = constant noEvent &&& accum s0

eventToMaybe NoEvent = Nothing
eventToMaybe (Event x) = Just x

pipeChan f i o = do
    x <- readChan i
    case f x of
        Nothing -> return ()
        Just e  -> writeChan o e
    pipeChan f i o

-- sequencerEvents :: [Unit] -> Time -> Sequencer a -> [P.SynthEvent]
sequencerEvents units t s = map (setEnv.f) is
    where
        -- is = map (\(r, c) -> r * cols s + c) $ indicesAtCursor s
        is = map fst $ indicesAtCursor s
        f i = P.SynthEvent t (units !! i) P.defaultSynth
        setEnv = setVal (P.synth.>P.attackTime) 0.01 . setVal (P.synth.>P.releaseTime) 0.02

getUnits dbFile pattern = do
    (units, sfMap) <- DB.withDatabase dbFile $ \c -> do
        unitQuery_ (quickQuery' c)
              ((url sourceFile `like` pattern) `and` (segmentation unit `eq` Unit.Onset))
    case units of
        Left e -> fail e
        Right us -> return us

main = do
    [dbFile, pattern, n] <- Env.getArgs
    -- db <- DB.open dbDir
    -- let units = drop (read n) (DB.units db)
    units <- drop (read n) `fmap` getUnits dbFile pattern
    mapM_ print units
    
    unsafeInitGUIForThreadedRTS
    -- initGUI
    
    ichan <- newChan
    ochan <- newChan
    seqChan <- newChan
    synth <- Synth.newSampler

    dia <- dialogNew
    dialogAddButton dia stockOk ResponseOk
    contain <- dialogGetUpper dia    
    (canvas, update) <- sequencerNew 25 3 sequencer0 seqChan ichan
    boxPackStartDefaults contain canvas
    widgetShow canvas

    -- Execute signal function
    forkIO $ executeSSF 0.005 (sequencer sequencer0 >>> first (arr (fmap (uncurry (sequencerEvents units))))) ichan ochan

    -- Dispatch events to and from sequencer view
    forkIO $ fix $ \loop -> do
        x <- readChan ochan
        case (fst.snd $ x) of
            NoEvent -> return ()
            Event es -> mapM_ (Synth.playEvent synth) es
        case (snd.snd $ x) of
            NoEvent -> return ()
            Event s -> writeChan seqChan s
        loop
    
    dialogRun dia
    return ()

-- printChanE c = do
--     (t, e) <- readChan c
--     case e of
--         NoEvent -> return ()
--         Event e -> print (t, e)
--     printChanE c
-- 
-- printChan c = readChan c >>= print >> printChan c

-- main_ = do
--     ichan <- newChan
--     ochan <- newChan
--     forkIO $ executeSSF 0.05 (sequencer sequencer0 >>> arr snd) ichan ochan
--     printChanE ochan

