{-# LANGUAGE Arrows #-}
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import Data.Maybe
import Graphics.Rendering.Diagrams as D
import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk hiding (Image, Point, get)
import Graphics.UI.Gtk.Diagram
import Mescaline.Synth.Sequencer as Sequencer
import Mescaline.Synth.SequencerView
import           Sound.OpenSoundControl hiding (Time)

import Control.Arrow
import Control.CCA.Types
import Euterpea.Signal.SF
import Mescaline.Synth.Pattern
import Prelude hiding (init, scanl)

-- TODO: Implement start time quantization based on master clock signal.
clock :: SF (Double, Double) (Event Double)
clock = scanl f (Nothing, NoEvent) >>> arr snd
    where
        f (Nothing, _) (globalTime, tick)
            = (Just (globalTime+tick), Event globalTime)
        f (Just localTime, _) (globalTime, tick)
            | localTime <= globalTime = (Just $ localTime+tick, Event localTime)
            | otherwise               = (Just $ localTime, NoEvent)

sequencer0 = Sequencer.cons 16 16 (Bar (-1))

sequencer :: SF (Double, Double) (Event (Sequencer ()))
sequencer = clock >>> tag (Sequencer.step (undefined::Score)) >>> accum sequencer0

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
    canvas <- sequencerNew 25 3 sequencer0 seqChan
    forkIO $ executeSF 0.05 (second (constant 0.125) >>> sequencer) ichan ochan
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

main_ = do
    ichan <- newChan
    ochan <- newChan
    forkIO $ executeSF 0.05 (second (constant 1) >>> sequencer) ichan ochan
    printChanE ochan
