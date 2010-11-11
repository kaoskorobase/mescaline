module Mescaline.Synth.Pattern.View (
    View
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
import           Control.Monad.Trans (MonadIO)
import           Data.Accessor
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Synth.Pattern.Sequencer as Model
import qualified Mescaline.Synth.Pattern.Process as Process
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
import qualified Qtc.Gui.QPen                       as Qt
import qualified Qth.Core.Rect                      as Qt

type View  = Qt.QGraphicsSceneSc (CView)
data CView = CView

newView :: IO (View)
newView = Qt.qSubClass (Qt.qGraphicsScene ())

data Params = Params {
    boxSize   :: Double
  , padding   :: Double
  } deriving (Show)

type Fields = Map.Map (Int,Int) (Qt.QGraphicsRectItem ())

data State = State {
    sequencer :: MVar (Maybe Model.Sequencer, Model.Sequencer)
  , fields    :: Fields
  , colors    :: [Qt.QColor ()]
  , fieldStyle :: (Qt.QPen (), Qt.QBrush ())
  , cursorStyle :: (Qt.QPen (), Qt.QBrush ())
  , activeStyle :: (Qt.QPen (), Qt.QBrush ())
  }

type Input  = Process.Output
type Output = ()

mouseHandler :: (Int, Int) -> ((Int, Int) -> Double -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler coord action this evt = action coord 1 >> Qt.mousePressEvent_h this evt

initScene :: View -> Params -> Int -> Int -> ((Int, Int) -> Double -> IO ()) -> IO Fields
initScene view p rows cols action = do
    xs <- forM [0..rows - 1] $ \r -> do
            forM [0..cols - 1] $ \c -> do
                let y = padding p + fromIntegral r * (boxSize p + padding p)
                    x = padding p + fromIntegral c * (boxSize p + padding p)
                    box = Qt.rectF x y (boxSize p) (boxSize p)
                    coord = (r, c)
                item <- Qt.qGraphicsRectItem box
                Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler coord action
                Qt.addItem view item
                return (coord, item)
    return $ Map.fromList (concat xs)

update :: State -> View -> IO ()
update state view = do
    (prevSeq, curSeq) <- readMVar (sequencer state)
    maybe (return ()) (mapM_ (setActive state fieldStyle) . Model.assocs) prevSeq
    mapM_ (setActive state activeStyle) (Model.assocs curSeq)
    maybe (return ()) (mapM_ (setCursor state fieldStyle . snd) . Model.cursors) prevSeq
    mapM_ (setCursor state cursorStyle . snd) (Model.cursors curSeq)
    where
        setActive state style ((row,col), value) = do
            case Map.lookup (row,col) (fields state) of
                Nothing -> return ()
                Just item -> do
                    let (p, b) = style state
                    Qt.setPen item p
                    Qt.setBrush item b
        setCursor state style cursor = do
            case Map.lookup (Model.row cursor, Model.column cursor) (fields state) of
                Nothing -> return ()
                Just item -> do
                    let (p, b) = style state
                    Qt.setPen item p
                    -- Qt.setBrush item b

updateProcess :: MonadIO m => View -> State -> ReceiverT Input Output m ()
updateProcess view state = loop
    where
        loop = do
            x <- recv
            case x of
                Process.Changed _ _ s ->
                    io $ do
                        modifyMVar_ (sequencer state) (\(_, s') -> return (Just s', s))
                        Qt.emitSignal view "update()" ()
                _ -> return ()
            loop

new :: Double -> Double -> Process.Handle -> IO (View, Handle Input Output)
new boxSize padding process = do
    view <- newView
    
    model <- query process $ Process.GetSequencer
    
    fields <- initScene view
                        (Params boxSize padding)
                        (Model.rows model)
                        (Model.cols model)
                        (\(r, c) _ -> sendTo process $ Process.ModifyCell r c (maybe (Just 1) (const Nothing)))
    -- colors <- UI.defaultColorsFromFile
    let colors = []

    modelVar <- newMVar (Nothing, model)
    
    tb <- Qt.qBrush Qt.eblack
    tw <- Qt.qBrush Qt.etransparent
    normalPen <- Qt.qPen (tb, 1::Double)
    cursorPen <- Qt.qPen (tb, 2::Double)
    activeBrush <- Qt.qBrush Qt.edarkGray
    let state = State modelVar fields colors (normalPen,tw) (cursorPen,tw) (normalPen,activeBrush)

    Qt.connectSlot view "update()" view "update()" $ update state
    Qt.emitSignal view "update()" ()

    handle <- spawn $ updateProcess view state

    return (view, handle)
