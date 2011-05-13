{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           , ExistentialQuantification #-}
module Mescaline.FeatureSpace.View (
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
import           Data.HashTable (HashTable)
import qualified Data.HashTable as Hash
import           Data.Int (Int32, Int64)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IMap
import qualified Data.Vector.Generic as V
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Config.Qt as Config
import qualified Mescaline.FeatureSpace.Model as Model
import qualified Mescaline.FeatureSpace.Unit as Unit
import qualified Mescaline.FeatureSpace.Process as Process
import qualified Mescaline.Pattern.Event as Synth
import qualified Mescaline.Synth.Sampler.Process as Synth

import qualified Qtc.Classes.Gui                    as Qt
import qualified Qtc.Classes.Gui_h                  as Qt
import qualified Qtc.Classes.Object                 as Qt
import qualified Qtc.Classes.Qccs                   as Qt
import qualified Qtc.Classes.Qccs_h                 as Qt
import qualified Qtc.ClassTypes.Gui                 as Qt
import qualified Qtc.Core.Base                      as Qt
import           Qtc.Core.QEvent                    ()
import qualified Qtc.Enums.Base                     as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import           Qtc.Enums.Gui.QGraphicsItem        ()
import qualified Qtc.Enums.Gui.QGraphicsScene       as Qt
import           Qtc.Gui.QAbstractGraphicsShapeItem ()
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QGraphicsEllipseItem       as Qt
import           Qtc.Gui.QGraphicsEllipseItem_h     ()
import qualified Qtc.Gui.QGraphicsItem              as Qt
import qualified Qtc.Gui.QGraphicsScene             as Qt
import           Qtc.Gui.QGraphicsSceneMouseEvent   ()
import           Qtc.Gui.QGraphicsScene_h           ()
import           Qtc.Gui.QKeyEvent                  ()
import qualified Qtc.Gui.QPen                       as Qt
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
sceneKeyPressEvent state _ evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Alt
        then do
            _ <- swapMVar (playUnits state) True
            return ()
        else return ()

sceneKeyReleaseEvent :: State -> FeatureSpaceView -> Qt.QKeyEvent () -> IO ()
sceneKeyReleaseEvent state _ evt = do
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

data UnitHighlight  = UnitHighlight !(Qt.QGraphicsItem ()) !Int

data HighlightState = HighlightState {
    highlightPen :: Qt.QPen ()
  , highlights   :: MVar (HashTable Int64 UnitHighlight)
  }

data State = State {
    featureSpace :: Process.Handle
  , synth        :: Synth.Handle
  , unitGroup    :: MVar (Maybe (Qt.QGraphicsItem ()))
  , units        :: MVar (HashTable Int64 (Qt.QGraphicsItem ()))
  , highlight    :: Maybe HighlightState
  , regionColors :: IntMap (Qt.QBrush ())
  , regions      :: MVar (HashTable Int Region)
  , playUnits    :: MVar Bool
  , guiChan      :: Chan (IO ())
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
            sendTo fspace $ Process.UpdateRegion $ Model.mkRegion (regionId region) (ix, iy) (d/2)
            -- Qt.setPos item pos
            return ()
        RegionResize y0 -> do
            Qt.IRect _ _ d _ <- Qt.qboundingRect item ()
            let dy = y0 - ey
                d' = d + dy
                r' = max (d'/2) unitRadius
            -- putStrLn $ "Region " ++ show (regionId region) ++ " radius=" ++ show r'
            -- onChanged $ Update $ Model.updateRegionById (regionId region) (\r -> r { Model.radius = r' })
            Qt.IPoint ix iy <- Qt.scenePos item ()
            sendTo fspace $ Process.UpdateRegion $ Model.mkRegion (regionId region) (ix, iy) r'
            -- Qt.qsetRect item (Qt.IRect (-r') (-r') d' d')
            _ <- swapMVar (regionState region) (RegionResize ey)
            return ()

regionMouseReleaseHandler :: Process.Handle -> Region -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseReleaseHandler _ region _ _ = do
    _ <- swapMVar (regionState region) RegionIdle
    return ()

addRegion :: FeatureSpaceView -> State -> Model.Region -> IO ()
addRegion view state region = do
    let Just b = IMap.lookup (Model.regionId region) (regionColors state)
        r      = Model.radius region
        x      = Model.center region V.! 0
        y      = Model.center region V.! 1
    item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (-r) (-r) (r*2) (r*2))
    Qt.setBrush item b
    -- text <- Qt.qGraphicsTextItem (show (Model.regionId region + 1))
    -- Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
    Qt.addItem view item
    Qt.setPos item (Qt.pointF x y)
    -- Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
    regionHandle <- Region (Model.regionId region) item `fmap` newMVar RegionIdle
    Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)"   $ regionMousePressHandler   (featureSpace state) regionHandle
    Qt.setHandler item "mouseMoveEvent(QGraphicsSceneMouseEvent*)"    $ regionMouseMoveHandler    (featureSpace state) regionHandle
    Qt.setHandler item "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ regionMouseReleaseHandler (featureSpace state) regionHandle

    -- text <- Qt.qGraphicsTextItem item
    -- Qt.setPlainText text (show (Model.regionId region + 1))
    -- Qt.qscale text (0.0025::Double, 0.0025::Double)
    -- Qt.setPos text (Qt.pointF (0::Double) (0::Double))
    -- Qt.setParentItem text item
    -- Qt.adjustSize text ()

    withMVar (regions state) $ \rs ->
        Hash.insert rs (regionId regionHandle) regionHandle

showUnits :: FeatureSpaceView -> State -> [Model.Unit] -> IO ()
showUnits view state us = do
    modifyMVar_ (unitGroup state) $ \g -> do
        maybe (return ()) (Qt.removeItem view) g
        g' <- Qt.qGraphicsItem_nf ()
        Qt.setZValue g' (-1 :: Double)
        Qt.addItem view g'

        modifyMVar_ (units state) $ \_ -> do
            table <- Hash.new (==) hashInt64
            mapM_ (addUnit g' state table) us
            return table

        return (Just g')

-- unitToolTip :: Model.Unit -> String
-- unitToolTip unit = printf "%s - %s"
--                           -- (Unit.value 0 unit V.! 0) (Unit.value 0 unit V.! 1)
--                           (unwords (map (printf "%.4f") (concatMap V.toList (V.toList (Unit.featureVectors unit)))))
--                           (takeFileName (DB.sourceFileUrl (Unit.sourceFile unit)))

addUnit :: Qt.QGraphicsItem () -> State -> HashTable Int64 (Qt.QGraphicsItem ()) -> Model.Unit -> IO ()
addUnit parent state table unit = do
    let (x, y) = pair (Unit.value 0 unit)
        r      = unitRadius -- Model.minRadius
        box    = Qt.rectF (-r) (-r) (r*2) (r*2)
    item <- Qt.qGraphicsEllipseItem_nf box
    Qt.setParentItem item parent
    Qt.setPos item (Qt.pointF x y)
    -- Qt.setToolTip item (unitToolTip unit)

    -- Qt.setFlags item Qt.fItemIgnoresTransformations
    -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action

    Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler $
        readMVar (playUnits state) >>= flip when (sendTo (synth state) $ Synth.PlayUnit (-1) (Synth.defaultSynth unit))
    Qt.setAcceptsHoverEvents item True

    Hash.insert table (fromIntegral (Unit.id unit)) (Qt.objectCast item)

process :: forall o m b .
           (Control.Monad.Trans.MonadIO m) =>
           FeatureSpaceView -> State -> ReceiverT (Either Process.Output Highlight) o m b
process view state = do
    msg <- recv
    io $ case msg of
        Left (Process.DatabaseLoaded us) -> do
            defer view state $ do
                Qt.setItemIndexMethod view Qt.eNoIndex
                showUnits view state us
                Qt.setItemIndexMethod view Qt.eBspTreeIndex
        Left (Process.RegionChanged r) -> do
            withMVar (regions state) $ \rs -> do
                rh <- Hash.lookup rs (Model.regionId r)
                case rh of
                    Nothing -> return ()
                    Just regionHandle ->
                        let item = regionItem regionHandle
                            pos  = Model.center2D r
                            rad  = Model.radius r
                            dia  = rad*2
                        in defer view state $ do
                            Qt.setPos item pos
                            Qt.qsetRect item (Qt.IRect (-rad) (-rad) dia dia)
        Right (HighlightOn unit) ->
            case highlight state of
                Nothing ->
                    return ()
                Just hlState -> do
                    defer view state $ do
                        withMVar (units state) $ \us ->
                            withMVar (highlights hlState) $ \hls -> do
                                let uid = Unit.id unit
                                hl <- Hash.lookup hls uid
                                case hl of
                                    Nothing -> do
                                        -- Insert new highlight item
                                        Just item <- Hash.lookup us uid
                                        Qt.IPoint x y <- Qt.scenePos item ()
                                        let -- (x, y) = pair (Feature.value (Model.feature unit))
                                            r      = highlightRadius
                                            box    = Qt.rectF (-r) (-r) (r*2) (r*2)
                                        item <- Qt.qGraphicsEllipseItem box
                                        Qt.setPos item (Qt.pointF x y)
                                        Qt.setPen item (highlightPen hlState)
                                        Qt.addItem view item
                                        Hash.insert hls uid (UnitHighlight (Qt.objectCast item) 1)
                                    Just (UnitHighlight item i) -> do
                                        -- Increase highlight count
                                        Hash.update hls uid (UnitHighlight item (i+1))
                                        return ()
        Right (HighlightOff unit) ->
            case highlight state of
                Nothing ->
                    return ()
                Just hlState -> do
                    defer view state $ do
                        withMVar (highlights hlState) $ \hls -> do
                            let uid = Unit.id unit
                            hl <- Hash.lookup hls uid
                            case hl of
                                Nothing ->
                                    return ()
                                Just (UnitHighlight item i) ->
                                    if i <= 1
                                        then do
                                            -- Remove highlight item
                                            Qt.removeItem view item
                                            Hash.delete hls uid
                                        else do
                                            -- Decrease highlight count
                                            Hash.update hls uid (UnitHighlight item (i-1))
                                            return ()
    process view state

update :: Chan (IO ()) -> FeatureSpaceView -> IO ()
update c _ = join (readChan c)

defer :: MonadIO m => FeatureSpaceView -> State -> IO () -> m ()
defer view state action = io $ do
    writeChan (guiChan state) action
    Qt.emitSignal view "update()" ()

getRegionColors :: MonadIO m => AppT m [Qt.QColor ()]
getRegionColors = mapM (\i -> Config.getColor "FeatureSpace" ("regionColor" ++ show i) "white") [1..n]
    where n = length Model.defaultRegions

hashInt64 :: Int64 -> Int32
hashInt64 = fromIntegral

-- hashUnique :: Unique.Id -> Int32
-- hashUnique = Hash.hashString . Unique.toString

newState :: MonadIO m => Process.Handle -> Synth.Handle -> AppT m State
newState fspace synth = do
    hl <- App.config "FeatureSpace" "highlightUnits" True
    hlColor <- Config.getColor "FeatureSpace" "highlightColor" "black"
    regionColors <- getRegionColors

    liftIO $ do
        ug <- newMVar Nothing
        us <- newMVar =<< Hash.new (==) hashInt64

        hlState <-
            if hl
                then do
                    hls     <- newMVar =<< Hash.new (==) hashInt64
                    hlBrush <- Qt.qBrush hlColor
                    hlPen   <- Qt.qPen (hlBrush, 0::Double)
                    return $ Just $ HighlightState hlPen hls
                else return Nothing

        regionBrushes <- mapM Qt.qBrush regionColors

        rs <- newMVar =<< Hash.new (==) Hash.hashInt
        pu <- newMVar False
        gc <- newChan
    
        return $ State fspace synth ug us hlState (IMap.fromList (zip [0..] regionBrushes)) rs pu gc

new :: MonadIO m => Process.Handle -> Synth.Handle -> AppT m (FeatureSpaceView, Handle Input Output)
new fspace synth = do
    state <- newState fspace synth
    
    liftIO $ do
        view <- featureSpaceView_
        Qt.setItemIndexMethod view Qt.eNoIndex
        Qt.setHandler view "keyPressEvent(QKeyEvent*)"   $ sceneKeyPressEvent state
        Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
        Qt.connectSlot view "update()" view "update()"   $ update (guiChan state)

        model <- query fspace Process.GetModel
        mapM_ (addRegion view state) (Model.regions model)

        handle <- spawn $ process view state

        return (view, handle)
