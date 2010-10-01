{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Mescaline.Synth.FeatureSpaceView (
    FeatureSpaceView
  , featureSpaceView
  , Input(..)
  , Output(..)
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import qualified Data.IntMap as Map
import qualified Data.Vector.Generic as V
import qualified Mescaline.Application as App
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace as FSpace
import qualified Mescaline.UI as UI

import qualified Qtc.Classes.Gui                    as Qt
import qualified Qtc.Classes.Gui_h                  as Qt
import qualified Qtc.Classes.Qccs                   as Qt
import qualified Qtc.Classes.Qccs_h                 as Qt
import qualified Qtc.ClassTypes.Core                as Qt
import qualified Qtc.ClassTypes.Gui                 as Qt
import qualified Qtc.Core.QEvent                    as Qt
import qualified Qtc.Enums.Base                     as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import qualified Qtc.Enums.Gui.QGraphicsItem        as Qt
import qualified Qtc.Gui.QAbstractGraphicsShapeItem as Qt
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem       as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem_h     as Qt
import qualified Qtc.Gui.QGraphicsItem              as Qt
import qualified Qtc.Gui.QGraphicsScene             as Qt
import qualified Qtc.Gui.QGraphicsSceneMouseEvent   as Qt
import qualified Qtc.Gui.QGraphicsScene_h           as Qt
import qualified Qtc.Gui.QKeyEvent                  as Qt
import qualified Qth.ClassTypes.Core                as Qt
import qualified Qth.Core.Point                     as Qt
import qualified Qth.Core.Rect                      as Qt

-- type FeatureSpaceView = Qt.QGraphicsSceneSc (CFeatureSpaceView)
type FeatureSpaceView = Qt.QGraphicsScene ()
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
-- featureSpaceView_ = Qt.qSubClass (Qt.qGraphicsScene ())
featureSpaceView_ = Qt.qGraphicsScene ()

data Input     = Update (FeatureSpace -> FeatureSpace) | ActivateRegion (Double, Int) | Deactivate Unit.Unit
newtype Output = Activate (Double, Unit.Unit)

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

-- mouseHandler :: Unit.Unit -> (Unit.Unit -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- mouseHandler unit action this evt = action unit >> Qt.mousePressEvent_h this evt

sceneMousePressHandler :: MVar Bool -> Qt.QGraphicsScene () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
sceneMousePressHandler state view evt = do
    putStrLn "sceneMousePressHandler"
    b <- Qt.button evt ()
    if ((Qt.qEnum_toInt b) == (Qt.qEnum_toInt Qt.eLeftButton))
        then Qt.ignore evt () >> swapMVar state True >> return ()
        else Qt.mousePressEvent_h view evt

sceneMouseReleaseHandler :: MVar Bool -> Qt.QGraphicsScene () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
sceneMouseReleaseHandler state view evt = do
    putStrLn "sceneMouseReleaseHandler"
    b <- Qt.button evt ()
    if ((Qt.qEnum_toInt b) == (Qt.qEnum_toInt Qt.eLeftButton))
        then swapMVar state False >> return ()
        else return ()
    Qt.mouseReleaseEvent_h view evt

sceneKeyPressEvent :: MVar State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyPressEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Shift
        then modifyMVar_ state $ \s -> return $ s { playUnits = True }
        else return ()

sceneKeyReleaseEvent :: MVar State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyReleaseEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Shift
        then modifyMVar_ state $ \s -> return $ s { playUnits = False }
        else return ()

hoverHandler :: IO () -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneHoverEvent () -> IO ()
hoverHandler action item evt = action >> Qt.hoverEnterEvent_h item evt

clusterChange :: Int -> ((FeatureSpace -> FeatureSpace) -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.GraphicsItemChange -> Qt.QVariant () -> IO (Qt.QVariant ())
clusterChange regionId onChanged item itemChange value = do
    when (itemChange == Qt.eItemPositionChange) $ do
        Qt.IPoint x y <- Qt.scenePos item ()
        putStrLn $ "clusterChange " ++ show regionId ++ " " ++ show (x, y)
        onChanged (FSpace.updateRegion regionId (\r -> r { FSpace.center = V.fromList [x, y] }))
    return value

-- mouseMoveHandler :: Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- mouseMoveHandler item evt = do
--     Qt.mouseMoveEvent_h item evt
--     Qt.IPoint x y <- Qt.scenePos item ()
--     putStrLn $ "mouseMoveEvent " ++ show (x, y)

initScene :: FeatureSpaceView -> FeatureSpace -> MVar State -> ((FeatureSpace -> FeatureSpace) -> IO ()) -> (Unit.Unit -> IO ()) -> IO ()
initScene view model state changed playUnit = do
    -- Qt.setHandler view "mousePressEvent(QGraphicsSceneMouseEvent*)" $ sceneMousePressHandler state
    -- Qt.setHandler view "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ sceneMouseReleaseHandler state
    Qt.setSceneRect view (Qt.rectF 0 0 1 1)
    Qt.setHandler view "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent state
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
    mapM_ mkUnit (units model)
    colors <- UI.defaultColorsFromFile
    mapM_ (uncurry $ mkRegion state) $ zip (Map.toList (FSpace.regions model)) colors
    where
        mkUnit u = do
            let (x, y) = pair (Feature.value (feature u))
                r = 0.005
                box = Qt.rectF (x-r) (y-r) (r*2) (r*2)
            item <- Qt.qGraphicsEllipseItem_nf box
            -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
            Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler $
                readMVar state >>= flip when (playUnit (unit u)) . playUnits
            Qt.setAcceptsHoverEvents item True
            Qt.addItem view item
        mkRegion state (index, region) color = do
            let r = radius region
                x = center region V.! 0
                y = center region V.! 1
            item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (-r) (-r) (r*2) (r*2))
            Qt.setBrush item =<< Qt.qBrush_nf color
            Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
            -- Qt.setHandler item "mouseMoveEvent(QGraphicsSceneMouseEvent*)" $ mouseMoveHandler
            Qt.addItem view item
            Qt.setPos item (Qt.pointF x y)
            Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
            modifyMVar_ state (\s -> return $ s { regionItems = item : regionItems s })

addRegions :: [Region] -> FeatureSpace -> FeatureSpace
addRegions regions fspace = foldl (\fs (i, r) -> insertRegion i r fs)
                          fspace
                          (zip [0..] regions)

inputLoop ichan ochan fspace = do
    x <- readChan ichan
    case x of
        Update f -> let fspace' = f fspace
                    in fspace' `seq` inputLoop ichan ochan fspace'
        ActivateRegion (t, i) -> do
            let (u, fspace') = activateRegion i fspace
            maybe (return ()) (\u -> writeChan ochan (Activate (t, unit u))) u
            inputLoop ichan ochan fspace'
        Deactivate _ ->
            inputLoop ichan ochan fspace

data State = State {
    regionItems :: [Qt.QGraphicsEllipseItem ()]
  , playUnits :: Bool
  }

featureSpaceView :: FeatureSpace -> Chan Input -> IO (FeatureSpaceView, Chan Output)
featureSpaceView fspace0 ichan = do
    let fspace = addRegions (replicate 4 (Region (V.fromList [0.5, 0.5]) 0.025)) fspace0
    this <- featureSpaceView_
    ochan <- newChan
    state <- newMVar (State [] False)

    initScene this fspace state (writeChan ichan . Update) (writeChan ochan . Activate . (,) (-1))

    forkIO $ inputLoop ichan ochan fspace
    
    return (this, ochan)
