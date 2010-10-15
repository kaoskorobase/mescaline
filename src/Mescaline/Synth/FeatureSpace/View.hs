{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , ExistentialQuantification #-}
module Mescaline.Synth.FeatureSpace.View (
    FeatureSpaceView
  , Input
  , Output
  , new
) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Trans
import           Data.Bits
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import qualified Data.Vector.Generic as V
-- import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace.Model (FeatureSpace)
import qualified Mescaline.Synth.FeatureSpace.Model as Model
import qualified Mescaline.Synth.FeatureSpace.Process as Process
import qualified Mescaline.UI as UI

import qualified Qtc.Classes.Gui                    as Qt
import qualified Qtc.Classes.Gui_h                  as Qt
import qualified Qtc.Classes.Object                 as Qt
import qualified Qtc.Classes.Qccs                   as Qt
import qualified Qtc.Classes.Qccs_h                 as Qt
import qualified Qtc.ClassTypes.Core                as Qt
import qualified Qtc.ClassTypes.Gui                 as Qt
import qualified Qtc.Core.Base                      as Qt
import qualified Qtc.Core.QEvent                    as Qt
import qualified Qtc.Enums.Base                     as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import qualified Qtc.Enums.Gui.QGraphicsItem        as Qt
import qualified Qtc.Enums.Gui.QGraphicsScene       as Qt
import qualified Qtc.Gui.QAbstractGraphicsShapeItem as Qt
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QCursor                    as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem       as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem_h     as Qt
import qualified Qtc.Gui.QGraphicsItem              as Qt
import qualified Qtc.Gui.QGraphicsScene             as Qt
import qualified Qtc.Gui.QGraphicsSceneMouseEvent   as Qt
import qualified Qtc.Gui.QGraphicsScene_h           as Qt
import qualified Qtc.Gui.QKeyEvent                  as Qt
import qualified Qtc.Gui.QWidget                    as Qt
import qualified Qth.ClassTypes.Core                as Qt
import qualified Qth.Core.Point                     as Qt
import qualified Qth.Core.Rect                      as Qt

type FeatureSpaceView = Qt.QGraphicsSceneSc (CFeatureSpaceView)
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
featureSpaceView_ = Qt.qSubClass (Qt.qGraphicsScene ())

type Input = Process.Output
type Output = ()

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

-- mouseHandler :: Unit.Unit -> (Unit.Unit -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- mouseHandler unit action this evt = action unit >> Qt.mousePressEvent_h this evt

-- sceneMousePressHandler :: MVar Bool -> Qt.QGraphicsScene () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- sceneMousePressHandler state view evt = do
--     putStrLn "sceneMousePressHandler"
--     b <- Qt.button evt ()
--     if ((Qt.qEnum_toInt b) == (Qt.qEnum_toInt Qt.eLeftButton))
--         then Qt.ignore evt () >> swapMVar state True >> return ()
--         else Qt.mousePressEvent_h view evt
-- 
-- sceneMouseReleaseHandler :: MVar Bool -> Qt.QGraphicsScene () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- sceneMouseReleaseHandler state view evt = do
--     putStrLn "sceneMouseReleaseHandler"
--     b <- Qt.button evt ()
--     if ((Qt.qEnum_toInt b) == (Qt.qEnum_toInt Qt.eLeftButton))
--         then swapMVar state False >> return ()
--         else return ()
--     Qt.mouseReleaseEvent_h view evt

sceneKeyPressEvent :: State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyPressEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Alt
        then do
            c <- Qt.qCursor Qt.eCrossCursor
            Qt.setCursor (Qt.objectCast view :: Qt.QWidget ()) c
            _ <- swapMVar (playUnits state) True
            return ()
        else return ()

sceneKeyReleaseEvent :: State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyReleaseEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Alt
        then do
            Qt.unsetCursor (Qt.objectCast view :: Qt.QWidget a) ()
            _ <- swapMVar (playUnits state) False
            return ()
        else return ()

hoverHandler :: IO () -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneHoverEvent () -> IO ()
hoverHandler action item evt = action >> Qt.hoverEnterEvent_h item evt

-- clusterChange :: Int -> ((FeatureSpace -> FeatureSpace) -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.GraphicsItemChange -> Qt.QVariant () -> IO (Qt.QVariant ())
-- clusterChange regionId onChanged item itemChange value = do
--     when (itemChange == Qt.eItemPositionChange) $ do
--         Qt.IPoint x y <- Qt.scenePos item ()
--         putStrLn $ "clusterChange " ++ show regionId ++ " " ++ show (x, y)
--         onChanged (Model.updateRegionById regionId (\r -> r { Model.center = V.fromList [x, y] }))
--     return value

data RegionState =
    RegionIdle
  | RegionMove !Double !Double
  | RegionResize !Double
  deriving (Eq, Show)

data Region = Region {
    regionId    :: Model.RegionId
  , regionItem  :: Qt.QGraphicsEllipseItem ()
  , regionState :: MVar RegionState
  }

data State = State {
    featureSpace :: Process.FeatureSpace
  , unitGroup    :: MVar (Maybe (Qt.QGraphicsItem ()))
  , colors       :: [Qt.QColor ()]
  , regions      :: IntMap Region
  , playUnits    :: MVar Bool
  , guiChan      :: Chan (IO ())
  }

regionMousePressHandler :: Process.FeatureSpace -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMousePressHandler _ region item evt = do
    mods <- Qt.modifiers evt ()
    if (Qt.qFlags_toInt mods) .&. (Qt.qFlags_toInt Qt.fControlModifier) /= 0
        then do
            Qt.IPoint _ ey <- Qt.scenePos evt ()
            _ <- swapMVar (regionState region) (RegionResize ey)
            return ()
        else when (Qt.qFlags_toInt mods == 0) $ do
            Qt.IPoint ix iy <- Qt.scenePos item ()
            Qt.IPoint ex ey <- Qt.scenePos evt ()
            _ <- swapMVar (regionState region) (RegionMove (ix-ex) (iy-ey))
            Qt.accept evt ()

regionMouseMoveHandler :: Process.FeatureSpace -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseMoveHandler fspace region item evt = do
    Qt.IPoint ex ey <- Qt.scenePos evt ()
    state <- readMVar (regionState region)
    case state of
        RegionIdle ->
            return ()
        RegionMove dx dy -> do
            let ix = ex + dx
                iy = ey + dy
            Qt.IRect _ _ d _ <- Qt.qboundingRect item ()
            -- putStrLn $ "Region " ++ show (regionId region) ++ " pos=" ++ show pos
            sendTo fspace $ Process.UpdateRegion $ Model.mkRegion (regionId region) (V.fromList [ix, iy]) (d/2)
            -- Qt.setPos item pos
            return ()
        RegionResize y0 -> do
            Qt.IRect _ _ d _ <- Qt.qboundingRect item ()
            let dy = y0 - ey
                d' = d + dy
                r' = d'/2
            -- putStrLn $ "Region " ++ show (regionId region) ++ " radius=" ++ show r'
            -- onChanged $ Update $ Model.updateRegionById (regionId region) (\r -> r { Model.radius = r' })
            Qt.IPoint ix iy <- Qt.scenePos item ()
            sendTo fspace $ Process.UpdateRegion $ Model.mkRegion (regionId region) (V.fromList [ix, iy]) r'
            -- Qt.qsetRect item (Qt.IRect (-r') (-r') d' d')
            _ <- swapMVar (regionState region) (RegionResize ey)
            return ()

regionMouseReleaseHandler :: Process.FeatureSpace -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseReleaseHandler _ region _ _ = do
    _ <- swapMVar (regionState region) RegionIdle
    return ()

addRegion :: FeatureSpaceView -> Model.Region -> State -> IO State
addRegion view region state = do
    let c = colors state !! Model.regionId region
        r = Model.radius region
        x = Model.center region V.! 0
        y = Model.center region V.! 1
    item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (-r) (-r) (r*2) (r*2))
    Qt.setBrush item =<< Qt.qBrush_nf c
    -- Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
    Qt.addItem view item
    Qt.setPos item (Qt.pointF x y)
    -- Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
    regionHandle <- Region (Model.regionId region) item `fmap` newMVar RegionIdle
    Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)"   $ regionMousePressHandler   (featureSpace state) regionHandle
    Qt.setHandler item "mouseMoveEvent(QGraphicsSceneMouseEvent*)"    $ regionMouseMoveHandler    (featureSpace state) regionHandle
    Qt.setHandler item "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ regionMouseReleaseHandler (featureSpace state) regionHandle
    return $ state { regions = Map.insert (regionId regionHandle) regionHandle (regions state) }

-- initScene :: FeatureSpaceView -> FeatureSpace -> MVar State -> (Input -> IO ()) -> (Unit.Unit -> IO ()) -> IO ()
-- initScene view model state onChanged playUnit = do
--     -- Qt.setHandler view "mousePressEvent(QGraphicsSceneMouseEvent*)" $ sceneMousePressHandler state
--     -- Qt.setHandler view "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ sceneMouseReleaseHandler state
--     -- Qt.setSceneRect view (Qt.rectF 0 0 1 1)
--     Qt.setItemIndexMethod view Qt.eNoIndex
--     Qt.setHandler view "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent state
--     Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
--     mapM_ mkUnit (Model.units model)
--     colors <- UI.defaultColorsFromFile
--     mapM_ (uncurry $ mkRegion state) $ zip (Map.toList (Model.regions model)) colors
--     Qt.setItemIndexMethod view Qt.eBspTreeIndex
--     -- Qt.setBspTreeDepth view 0
--     where

showUnits :: FeatureSpaceView -> State -> [Model.Unit] -> IO ()
showUnits view state us = do
    g <- takeMVar (unitGroup state)
    maybe (return ()) (Qt.removeItem view) g
    g <- Qt.qGraphicsItem_nf ()
    putMVar (unitGroup state) (Just g)
    mapM_ (addUnit g state) us
    Qt.setZValue g (-1 :: Double)
    Qt.addItem view g

addUnit :: Qt.QGraphicsItem () -> State -> Model.Unit -> IO ()
addUnit parent state unit = do
    let (x, y) = pair (Feature.value (Model.feature unit))
        r      = 0.0025 -- Model.minRadius
        box    = Qt.rectF (-r) (-r) (r*2) (r*2)
    item <- Qt.qGraphicsEllipseItem_nf box
    Qt.setParentItem item parent
    Qt.setPos item (Qt.pointF x y)
    -- Qt.setFlags item Qt.fItemIgnoresTransformations
    -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
    Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler $
        readMVar (playUnits state) >>= flip when (sendTo (featureSpace state) $ Process.ActivateUnit (-1) (Model.unit unit))
    Qt.setAcceptsHoverEvents item True

process :: forall o m b .
           (Control.Monad.Trans.MonadIO m) =>
           FeatureSpaceView -> State -> ReceiverT Process.Output o m b
process view state = do
    x <- recv
    state' <-
        case x of
            Process.DatabaseLoaded us -> do
                defer view state $ do
                    Qt.setItemIndexMethod view Qt.eNoIndex
                    showUnits view state us
                    Qt.setItemIndexMethod view Qt.eBspTreeIndex
                return state
            Process.UnitActivated t u -> do
                return state
            Process.UnitDeactivated u -> do
                return state
            Process.RegionAdded r ->
                io $ addRegion view r state
            Process.RegionChanged r -> do
                case Map.lookup (Model.regionId r) (regions state) of
                    Nothing -> return ()
                    Just regionHandle ->
                        let item = regionItem regionHandle
                            pos  = pair (Model.center r)
                            rad  = Model.radius r
                            dia  = rad*2
                        in defer view state $ do
                            Qt.setPos item pos
                            Qt.qsetRect item (Qt.IRect (-rad) (-rad) dia dia)
                return state
    process view state'

update :: Chan (IO ()) -> FeatureSpaceView -> FeatureSpaceView -> IO ()
update c _ _ = join (readChan c)

defer :: MonadIO m => FeatureSpaceView -> State -> IO () -> m ()
defer view state action = io $ do
    writeChan (guiChan state) action
    Qt.emitSignal view "update()" ()

newState :: Process.FeatureSpace -> IO State
newState fspace = do
    ug <- newMVar Nothing
    cs <- UI.defaultColorsFromFile
    pu <- newMVar False
    gc <- newChan
    return $ State fspace ug (cycle cs) Map.empty pu gc

new :: Process.FeatureSpace -> IO (FeatureSpaceView, Handle Input Output)
new fspace = do
    state <- newState fspace
    
    view <- featureSpaceView_
    Qt.setItemIndexMethod view Qt.eNoIndex
    Qt.setHandler view "keyPressEvent(QKeyEvent*)"   $ sceneKeyPressEvent state
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
    Qt.connectSlot view "update()" view "update()"   $ update (guiChan state)

    handle <- spawn $ process view state

    return (view, handle)
