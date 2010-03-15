{-# LANGUAGE Arrows #-}
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
-- import Control.Monad.State
import Data.Accessor
import Data.Maybe
import Graphics.Rendering.Diagrams as D
import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk hiding (Image, Point, get)
import Graphics.UI.Gtk.Diagram
import Mescaline (Time)
import Mescaline.Synth.Sequencer as Sequencer
import Mescaline.Synth.SequencerView
import           Sound.OpenSoundControl hiding (Time)

import Control.Arrow
import Control.CCA.Types
import Mescaline.Synth.SSF
import Prelude hiding (init, scanl)

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

sequencer :: Sequencer a -> SSF (Update (Sequencer a)) (Event Time, Changed (Sequencer a))
sequencer s0 =
    proc update -> do
        rec
            c <- clock -< t
            e <- tag (Sequencer.step (undefined::Score)) -< c
            s <- accum s0 -< (foldl (.) id `fmap` (update `merge` e))
            t <- hold s0 >>> arr (getVal Sequencer.tick) -< s
        returnA -< (c, s)

eventToMaybe NoEvent = Nothing
eventToMaybe (Event x) = Just x

pipeChan f i o = do
    x <- readChan i
    case f x of
        Nothing -> return ()
        Just e  -> writeChan o e
    pipeChan f i o

main = do
    unsafeInitGUIForThreadedRTS
    dia <- dialogNew
    dialogAddButton dia stockOk ResponseOk
    contain <- dialogGetUpper dia
    ichan <- newChan
    ochan <- newChan
    seqChan <- newChan
    canvas <- sequencerNew 25 3 sequencer0 seqChan ichan
    -- forkIO $ executeSSF 0.05 (constant 0.125 >>> sequencer) ichan ochan
    forkIO $ executeSSF 0.05 (sequencer sequencer0 >>> arr snd) ichan ochan
    forkIO $ pipeChan (eventToMaybe.snd) ochan seqChan
    -- widgetSetSizeRequest canvas 600 600
    boxPackStartDefaults contain canvas
    widgetShow canvas
    dialogRun dia
    return ()

printChanE c = do
    (t, e) <- readChan c
    case e of
        NoEvent -> return ()
        Event e -> print (t, e)
    printChanE c

printChan c = readChan c >>= print >> printChan c

-- main_ = do
--     ichan <- newChan
--     ochan <- newChan
--     forkIO $ executeSSF 0.05 (sequencer sequencer0 >>> arr snd) ichan ochan
--     printChanE ochan
