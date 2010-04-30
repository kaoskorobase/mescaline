module Mescaline.Synth.SequencerView (
    SequencerView
  , sequencerView
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Mescaline.Synth.Sequencer as Sequencer
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Qt as Q

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
                        else Q.qBrush Q.eNoBrush
            Q.setBrush field b

sequencerView :: Double -> Double -> Sequencer a -> Chan (Sequencer a) -> Chan (Sequencer a -> Sequencer a) -> IO SequencerView
sequencerView boxSize padding seq0 ichan ochan = do
    let params = Params boxSize padding
    this <- sequencerView_
    fields <- initScene this params (rows seq0) (cols seq0) $ \(r, c) -> do
        writeChan ochan (Sequencer.toggle r c undefined)
    state <- newMVar (State params seq0 fields)
    
    Q.connectSlot this "updateScene()" this "updateScene()" $ updateScene state

    forkIO $ fix $ \loop -> do
        newSequencer <- readChan ichan
        modifyMVar_ state (\s -> return $ s { sequencer = newSequencer })
        -- putStrLn "state changed"
        Q.emitSignal this "updateScene()" ()
        loop

    return this
