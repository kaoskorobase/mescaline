module Mescaline.Synth.Sequencer.View (
    SequencerView
  , Input
  , Output
  , new
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Data.Accessor
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Synth.Sequencer.Model as Model
import qualified Mescaline.Synth.Sequencer.Process as Process
import qualified Mescaline.UI as UI
import qualified Sound.OpenSoundControl.Time as Time

import qualified Qtc.Classes.Gui                    as Qt
import qualified Qtc.Classes.Qccs_h                 as Qt
import qualified Qtc.ClassTypes.Gui                 as Qt
import qualified Qtc.Core.Base                      as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QAbstractGraphicsShapeItem as Qt
import qualified Qtc.Gui.QGraphicsRectItem          as Qt
import qualified Qtc.Gui.QGraphicsRectItem_h        as Qt
import qualified Qtc.Gui.QGraphicsScene             as Qt
import qualified Qth.Core.Rect                      as Qt

type SequencerView = Qt.QGraphicsSceneSc (CSequencerView)
data CSequencerView = CSequencerView

sequencerView_ :: IO (SequencerView)
sequencerView_ = Qt.qSubClass (Qt.qGraphicsScene ())

data Params = Params {
    boxSize   :: Double
  , padding   :: Double
  } deriving (Show)

type Fields = Map.Map (Int,Int) ((Int, Int), Qt.QGraphicsRectItem ())

data State = State {
    sequencer :: MVar (Model.Sequencer ())
  , fields    :: Fields
  , colors    :: [Qt.QColor ()]
  }

type Input  = Process.Output ()
type Output = ()

mouseHandler :: (Int, Int) -> ((Int, Int) -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler coord action this evt = action coord >> Qt.mousePressEvent_h this evt

initScene :: SequencerView -> Params -> Int -> Int -> ((Int, Int) -> IO ()) -> IO Fields
initScene view p rows cols action = do
    xs <- forM [0..rows - 1] $ \r -> do
            forM [0..cols - 1] $ \c -> do
                let y = padding p + fromIntegral r * (boxSize p + padding p)
                    x = padding p + fromIntegral c * (boxSize p + padding p)
                    box = Qt.rectF x y (boxSize p) (boxSize p)
                    coord = (r, c)
                item <- Qt.qGraphicsRectItem_nf box
                Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler coord action
                Qt.addItem view item
                return (coord, (coord, item))
    return $ Map.fromList (concat xs)

updateScene :: State -> SequencerView -> SequencerView -> IO ()
updateScene state view _ = do
    s <- readMVar (sequencer state)
    Fold.mapM_ (update s) (fields state)
    where
        update s (coord, field) = do
            -- print (coord, cursor_ (sequencer state))
            b <- if Model.isCursorAtIndex s coord
                    then Qt.qBrush Qt.eblack
                    else if Model.isElemAtIndex s coord
                        then Qt.qBrush Qt.edarkGray
                        else let cs = cycle $ concat $ map (replicate 4) $ colors state
                             in Qt.qBrush (cs !! fst coord)
            Qt.setBrush field b

process view state = do
    Process.Changed _ s _ <- recv
    io $ do
        _ <- swapMVar (sequencer state) s
        Qt.emitSignal view "updateScene()" ()
    process view state

new :: Double -> Double -> Process.Sequencer () -> IO (SequencerView, Handle Input Output)
new boxSize padding sequencer = do
    view <- sequencerView_
    
    -- FIXME: How to abstract this away?
    -- Asynchronous messages vs. synchronous queries
    modelVar <- newEmptyMVar
    sendTo sequencer $ Process.QueryModel modelVar
    model <- readMVar modelVar
    
    fields <- initScene view
                        (Params boxSize padding)
                        (Model.rows model)
                        (Model.cols model)
                        (\(r, c) -> sendTo sequencer $ Process.ToggleField r c ())
    colors <- UI.defaultColorsFromFile

    let state = State modelVar fields colors

    Qt.connectSlot view "updateScene()" view "updateScene()" $ updateScene state
    Qt.emitSignal view "updateScene()" ()

    -- forkIO $ fix $ \loop -> do
    --     readChan ichan >>= writeChan seq_ichan
    --     loop
    
    -- forkIO $ fix $ \loop -> do
    --     e@(t, s') <- readChan seq_ochan
    --     writeChan ochan e
    --     modifyMVar_ state (\s -> return $ s { sequencer = s' })
    --     forkIO $ Qt.emitSignal this "updateScene()" ()
    --     loop

    handle <- spawn $ process view state

    return (view, handle)
