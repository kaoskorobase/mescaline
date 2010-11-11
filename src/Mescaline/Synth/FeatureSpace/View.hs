{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , ExistentialQuantification #-}
module Mescaline.Synth.FeatureSpace.View (
    FeatureSpaceView
  , Input
  , Output
  , Highlight(..)
  , new
  , getRegionColors
) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Trans
import           Data.Bits
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import qualified Data.Map as Map
import qualified Data.Vector.Generic as V
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace.Model (FeatureSpace)
import qualified Mescaline.Synth.FeatureSpace.Model as Model
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import qualified Mescaline.Synth.FeatureSpace.Process as Process
import qualified Mescaline.Synth.Pattern.Event as Synth
import qualified Mescaline.Synth.Sampler.Process as Synth
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
import qualified Qtc.Core.QTimer                    as Qt
import qualified Qtc.Enums.Base                     as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import qualified Qtc.Enums.Gui.QGraphicsItem        as Qt
import qualified Qtc.Enums.Gui.QGraphicsScene       as Qt
import qualified Qtc.Gui.QAbstractGraphicsShapeItem as Qt
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QColor                     as Qt
import qualified Qtc.Gui.QCursor                    as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem       as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem_h     as Qt
import qualified Qtc.Gui.QGraphicsItem              as Qt
import qualified Qtc.Gui.QGraphicsRectItem          as Qt
import qualified Qtc.Gui.QGraphicsScene             as Qt
import qualified Qtc.Gui.QGraphicsSceneMouseEvent   as Qt
import qualified Qtc.Gui.QGraphicsScene_h           as Qt
import qualified Qtc.Gui.QKeyEvent                  as Qt
import qualified Qtc.Gui.QPen                       as Qt
import qualified Qtc.Gui.QWidget                    as Qt
import qualified Qth.ClassTypes.Core                as Qt
import qualified Qth.Core.Point                     as Qt
import qualified Qth.Core.Rect                      as Qt

type FeatureSpaceView = Qt.QGraphicsSceneSc (CFeatureSpaceView)
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
featureSpaceView_ = Qt.qSubClass (Qt.qGraphicsScene ())

data Highlight = HighlightOn Unit.Unit | HighlightOff Unit.Unit
type Input     = Either Process.Output Highlight
type Output    = ()

pair :: (V.Vector v a, Num a) => v a -> (a, a)
pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

unitRadius :: Double
unitRadius = 0.004

highlightRadius :: Double
highlightRadius = 2 * unitRadius

sceneKeyPressEvent :: State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyPressEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Alt
        then do
            _ <- swapMVar (playUnits state) True
            return ()
        else return ()

sceneKeyReleaseEvent :: State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyReleaseEvent state view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Alt
        then do
            _ <- swapMVar (playUnits state) False
            return ()
        else return ()

hoverHandler :: IO () -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneHoverEvent () -> IO ()
hoverHandler action item evt = action >> Qt.hoverEnterEvent_h item evt

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

data UnitHighlight  = UnitHighlight !(Qt.QGraphicsEllipseItem ()) !Int
type UnitHighlight' = Int

data HighlightState = HighlightState {
    highlightPen :: Qt.QPen ()
  , highlights   :: MVar (Unique.Map UnitHighlight')
  , highlightGroup :: MVar (Maybe (Qt.QGraphicsItem ()))
  , highlightUnits :: MVar (Unique.Map (Qt.QGraphicsEllipseItem ()))
  }

data State = State {
    featureSpace :: Process.Handle
  , synth        :: Synth.Handle
  , unitGroup    :: MVar (Maybe (Qt.QGraphicsItem ()))
  , units        :: MVar (Unique.Map (Qt.QGraphicsEllipseItem ()))
  , highlight    :: Maybe HighlightState
  , regionColors :: IntMap (Qt.QBrush ())
  , regions      :: MVar (IntMap Region)
  , playUnits    :: MVar Bool
  , guiChan      :: Chan Input
  }

regionMousePressHandler :: Process.Handle -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
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

regionMouseMoveHandler :: Process.Handle -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
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

regionMouseReleaseHandler :: Process.Handle -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseReleaseHandler _ region _ _ = do
    _ <- swapMVar (regionState region) RegionIdle
    return ()

addRegion :: FeatureSpaceView -> Model.Region -> State -> IO ()
addRegion view region state = do
    let Just b = IMap.lookup (Model.regionId region) (regionColors state)
        r      = Model.radius region
        x      = Model.center region V.! 0
        y      = Model.center region V.! 1
    item <- Qt.qGraphicsEllipseItem (Qt.rectF (-r) (-r) (r*2) (r*2))
    Qt.setBrush item b
    -- Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
    Qt.addItem view item
    Qt.setPos item (Qt.pointF x y)
    -- Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
    regionHandle <- Region (Model.regionId region) item `fmap` newMVar RegionIdle
    Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)"   $ regionMousePressHandler   (featureSpace state) regionHandle
    Qt.setHandler item "mouseMoveEvent(QGraphicsSceneMouseEvent*)"    $ regionMouseMoveHandler    (featureSpace state) regionHandle
    Qt.setHandler item "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ regionMouseReleaseHandler (featureSpace state) regionHandle
    modifyMVar_ (regions state) (return . IMap.insert (regionId regionHandle) regionHandle)

showUnits :: FeatureSpaceView -> State -> [Model.Unit] -> IO ()
showUnits view state us = do
    g <- takeMVar (unitGroup state)
    _ <- takeMVar (units state)
    maybe (return ()) (Qt.removeItem view) g

    g <- Qt.qGraphicsItem ()
    Qt.setZValue g (-1 :: Double)
    Qt.addItem view g

    putMVar (unitGroup state) (Just g)
    putMVar (units state) . Map.fromList =<< mapM (addUnit g state) us
    
    case highlight state of
        Nothing -> return ()
        Just hlState -> do
            g <- takeMVar (highlightGroup hlState)
            _ <- takeMVar (highlightUnits hlState)
            maybe (return ()) (Qt.removeItem view) g
            g <- Qt.qGraphicsItem ()
            Qt.setZValue g (1 :: Double)
            Qt.addItem view g
            putMVar (highlightGroup hlState) (Just g)
            putMVar (highlightUnits hlState) . Map.fromList =<< mapM (addUnit' highlightRadius (highlightPen hlState) g) us

addUnit' :: Double -> Qt.QPen () -> Qt.QGraphicsItem () -> Model.Unit -> IO (Unique.Id, Qt.QGraphicsEllipseItem ())
addUnit' radius pen parent unit = do
    let (x, y) = pair (Unit.value 0 unit)
        box    = Qt.rectF (-radius) (-radius) (radius*2) (radius*2)
    item <- Qt.qGraphicsEllipseItem_nf box
    Qt.setParentItem item parent
    Qt.setPos item (Qt.pointF x y)
    Qt.setPen item pen
    Qt.setVisible item False
    return (Unit.id unit, item)

addUnit :: Qt.QGraphicsItem () -> State -> Model.Unit -> IO (Unique.Id, Qt.QGraphicsEllipseItem ())
addUnit parent state unit = do
    let (x, y) = pair (Unit.value 0 unit)
        r      = unitRadius -- Model.minRadius
        box    = Qt.rectF (-r) (-r) (r*2) (r*2)
    item <- Qt.qGraphicsEllipseItem_nf box
    Qt.setParentItem item parent
    Qt.setPos item (Qt.pointF x y)

    -- Qt.setFlags item Qt.fItemIgnoresTransformations
    -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action

    Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler $
        readMVar (playUnits state) >>= flip when (sendTo (synth state) $ Synth.PlayUnit (-1) (Synth.defaultSynth unit))
    Qt.setAcceptsHoverEvents item True

    return (Unit.id unit, item)

defer :: MonadIO m => FeatureSpaceView -> State -> Input -> m ()
defer view state msg = io $ do
    writeChan (guiChan state) msg
    Qt.emitSignal view "update()" ()

process :: forall o m b .
           (Control.Monad.Trans.MonadIO m) =>
           FeatureSpaceView -> State -> ReceiverT Input o m b
process view state = recv >>= defer view state >> process view state

update :: State -> FeatureSpaceView -> IO ()
update state view = do
    e <- isEmptyChan (guiChan state)
    unless e $ do
        msg <- readChan (guiChan state)
        case msg of
            Left (Process.DatabaseLoaded us) -> do
                Qt.setItemIndexMethod view Qt.eNoIndex
                showUnits view state us
                Qt.setItemIndexMethod view Qt.eBspTreeIndex
            Left (Process.RegionAdded r) ->
                addRegion view r state
            Left (Process.RegionChanged r) -> do
                regionMap <- readMVar (regions state)
                case IMap.lookup (Model.regionId r) regionMap of
                    Nothing -> return ()
                    Just regionHandle ->
                        let item = regionItem regionHandle
                            pos  = Model.center2D r
                            rad  = Model.radius r
                            dia  = rad*2
                        in do
                            Qt.setPos item pos
                            Qt.qsetRect item (Qt.IRect (-rad) (-rad) dia dia)
            -- Right (HighlightOn unit) ->
            --     case highlight state of
            --         Nothing ->
            --             return ()
            --         Just hlState -> do
            --             us <- readMVar (units state)
            --             as <- takeMVar (highlights hlState)
            --             let uid = Unit.id unit
            --             case Map.lookup uid as of
            --                 Nothing -> do
            --                     -- Insert new highlight item
            --                     case Map.lookup uid us of
            --                         Nothing -> return ()
            --                         Just item -> do
            --                             Qt.IPoint x y <- Qt.scenePos item ()
            --                             let -- (x, y) = pair (Feature.value (Model.feature unit))
            --                                 r      = highlightRadius
            --                                 box    = Qt.rectF (-r) (-r) (r*2) (r*2)
            --                             -- item <- Qt.qGraphicsRectItem box
            --                             item <- Qt.qGraphicsEllipseItem box
            --                             Qt.setPos item (Qt.pointF x y)
            --                             Qt.setPen item (highlightPen hlState)
            --                             Qt.addItem view item
            --                             putMVar (highlights hlState) (Map.insert uid (UnitHighlight item 1) as)
            --                 Just (UnitHighlight item i) ->
            --                     -- Increase highlight count
            --                     putMVar (highlights hlState) (Map.insert uid (UnitHighlight item (i+1)) as)
            -- Right (HighlightOff unit) ->
            --     case highlight state of
            --         Nothing ->
            --             return ()
            --         Just hlState -> do
            --             as <- takeMVar (highlights hlState)
            --             case Map.lookup (Unit.id unit) as of
            --                 Nothing ->
            --                     putMVar (highlights hlState) as
            --                 Just (UnitHighlight item i) ->
            --                     if i <= 1
            --                         then do
            --                             -- Remove highlight item
            --                             Qt.removeItem view item
            --                             putMVar (highlights hlState) (Map.delete (Unit.id unit) as)
            --                         else do
            --                             -- Decrease highlight count
            --                             putMVar (highlights hlState) (Map.insert (Unit.id unit) (UnitHighlight item (i-1)) as)
            Right (HighlightOn unit) ->
                case highlight state of
                    Nothing ->
                        return ()
                    Just hlState -> do
                        us <- readMVar (highlightUnits hlState)
                        as <- takeMVar (highlights hlState)
                        let uid = Unit.id unit
                        case Map.lookup uid as of
                            Nothing -> do
                                -- Insert new highlight item
                                let Just item = Map.lookup uid us
                                Qt.setVisible item True
                                putMVar (highlights hlState) (Map.insert uid 1 as)
                            Just i ->
                                -- Increase highlight count
                                putMVar (highlights hlState) (Map.insert uid (i+1) as)
            Right (HighlightOff unit) ->
                case highlight state of
                    Nothing ->
                        return ()
                    Just hlState -> do
                        us <- readMVar (highlightUnits hlState)
                        as <- takeMVar (highlights hlState)
                        let uid = Unit.id unit
                        case Map.lookup (Unit.id unit) as of
                            Nothing ->
                                putMVar (highlights hlState) as
                            Just i -> do
                                let Just item = Map.lookup uid us
                                if i <= 1
                                    then do
                                        -- Remove highlight item
                                        Qt.setVisible item False
                                        putMVar (highlights hlState) (Map.delete (Unit.id unit) as)
                                    else do
                                        -- Decrease highlight count
                                        putMVar (highlights hlState) (Map.insert (Unit.id unit) (i-1) as)

getRegionColors :: Config.ConfigParser -> IO [Qt.QColor ()]
getRegionColors config = mapM (\i -> Config.getColor config "FeatureSpace" ("regionColor" ++ show i)) [1..n]
    where n = length Model.defaultRegions

newState :: Process.Handle -> Synth.Handle -> IO State
newState fspace synth = do
    config <- Config.getConfig

    ug <- newMVar Nothing
    us <- newMVar Map.empty

    hl <- Config.getIO config "FeatureSpace" "highlightUnits" :: IO Bool
    hlState <-
        if hl
            then do
                hls     <- newMVar Map.empty
                hlColor <- Config.getColor config "FeatureSpace" "highlightColor"
                hlBrush <- Qt.qBrush hlColor
                hlPen   <- Qt.qPen (hlBrush, 0::Double)
                hlGroup <- newMVar Nothing
                hlUnits <- newMVar Map.empty
                return $ Just $ HighlightState hlPen hls hlGroup hlUnits
            else return Nothing

    regionBrushes <- getRegionColors config >>= mapM Qt.qBrush

    rs <- newMVar IMap.empty
    pu <- newMVar False
    gc <- newChan
    
    return $ State fspace synth ug us hlState (IMap.fromList (zip [0..] regionBrushes)) rs pu gc

new :: Process.Handle -> Synth.Handle -> IO (FeatureSpaceView, Handle Input Output)
new fspace synth = do
    state <- newState fspace synth
    
    view <- featureSpaceView_
    Qt.setItemIndexMethod view Qt.eNoIndex
    Qt.setHandler view "keyPressEvent(QKeyEvent*)"   $ sceneKeyPressEvent state
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
    Qt.connectSlot view "update()" view "update()"   $ update state
    
    -- timer <- Qt.qTimer view
    -- Qt.connectSlot timer "timeout()" view "update()" ()
    -- Qt.start timer (50::Int)

    handle <- spawn $ process view state

    return (view, handle)
