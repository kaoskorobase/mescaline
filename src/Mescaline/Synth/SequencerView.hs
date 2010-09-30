module Mescaline.Synth.SequencerView (
    SequencerView
  , sequencerView
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Data.Accessor
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import           Mescaline (Time)
import qualified Mescaline.Application as App
import           Mescaline.Synth.Sequencer as Seq
import qualified Mescaline.UI as UI
import qualified Qt as Q
import qualified Sound.OpenSoundControl.Time as Time

type SequencerView = Q.QGraphicsSceneSc (CSequencerView)
data CSequencerView = CSequencerView

sequencerView_ :: IO (SequencerView)
sequencerView_ = Q.qSubClass (Q.qGraphicsScene ())

data Params = Params {
    boxSize   :: Double
  , padding   :: Double
  } deriving (Show)

type Fields = Map.Map (Int,Int) ((Int, Int), Q.QGraphicsRectItem ())

data State a = State {
    params    :: Params
  , sequencer :: Sequencer a
  , fields    :: Fields
  , colors    :: [Q.QColor ()]
  }

mouseHandler :: (Int, Int) -> ((Int, Int) -> IO ()) -> Q.QGraphicsRectItem () -> Q.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler coord action this evt = action coord >> Q.mousePressEvent_h this evt

initScene :: SequencerView -> Params -> Int -> Int -> ((Int, Int) -> IO ()) -> IO Fields
initScene view p rows cols action = do
    xs <- forM [0..rows - 1] $ \r -> do
            forM [0..cols - 1] $ \c -> do
                let y = padding p + fromIntegral r * (boxSize p + padding p)
                    x = padding p + fromIntegral c * (boxSize p + padding p)
                    box = Q.rectF x y (boxSize p) (boxSize p)
                    coord = (r, c)
                item <- Q.qGraphicsRectItem_nf box
                Q.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler coord action
                Q.addItem view item
                return (coord, (coord, item))
    return $ Map.fromList (concat xs)

updateScene :: MVar (State a) -> SequencerView -> SequencerView -> IO ()
updateScene stateVar this _ = do
    state <- readMVar stateVar
    Fold.mapM_ (update state) (fields state)
    where
        update state (coord, field) = do
            -- print (coord, cursor_ (sequencer state))
            b <- if sequencer state `isCursorAtIndex` coord
                    then Q.qBrush Q.edarkRed
                    else if sequencer state `isElemAtIndex` coord
                        then Q.qBrush Q.edarkGray
                        else Q.qBrush (colors state !! (fst coord `mod` length (colors state)))
            Q.setBrush field b

sequencerProcess :: Sequencer a -> Chan (Sequencer a -> Sequencer a) -> IO (Chan (Time, Sequencer a))
sequencerProcess s0 ichan = do
    ochan <- newChan
    t0 <- Time.utcr
    forkIO $ loop ichan ochan s0 t0
    return ochan
    where
        loop ichan ochan s t = do
            s' <- applyUpdates ichan s
            let t' = t + getVal Seq.tick s'
            writeChan ochan (t, s')
            Time.pauseThreadUntil t'
            let s'' = Seq.step (undefined::Seq.Score) s'
            loop ichan ochan s'' t'
        applyUpdates c s = do
            b <- isEmptyChan c
            if b
                then return s
                else do
                    f <- readChan c
                    let s' = f s
                    s' `seq` applyUpdates c s'

sequencerView :: Double -> Double -> Sequencer a -> Chan (Sequencer a -> Sequencer a) -> IO (SequencerView, Chan (Time, Sequencer a))
sequencerView boxSize padding seq0 ichan = do
    let params = Params boxSize padding
    this <- sequencerView_
    ochan <- newChan
    seq_ichan <- newChan
    seq_ochan <- sequencerProcess seq0 seq_ichan
    fields <- initScene this params (rows seq0) (cols seq0) $ \(r, c) -> do
        writeChan seq_ichan (Seq.toggle r c undefined)
    colors <- UI.defaultColorsFromFile
    state <- newMVar (State params seq0 fields colors)
    
    Q.connectSlot this "updateScene()" this "updateScene()" $ updateScene state
    Q.emitSignal this "updateScene()" ()

    forkIO $ fix $ \loop -> do
        readChan ichan >>= writeChan seq_ichan
        loop
    
    forkIO $ fix $ \loop -> do
        e@(t, s') <- readChan seq_ochan
        writeChan ochan e
        modifyMVar_ state (\s -> return $ s { sequencer = s' })
        forkIO $ Q.emitSignal this "updateScene()" ()
        loop

    return (this, ochan)
