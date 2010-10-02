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
import           Data.Bits
import qualified Data.IntMap as Map
import qualified Data.Vector.Generic as V
import qualified Mescaline.Application as App
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace (FeatureSpace)
import qualified Mescaline.Synth.FeatureSpace as FSpace
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

minRadius :: Double
minRadius = 0.005

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

regionMousePressHandler :: Region -> (Input -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMousePressHandler region onChanged item evt = do
    mods <- Qt.modifiers evt ()
    -- Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
    -- Qt.unsetFlag item Qt.eItemIsMovable
    if (Qt.qFlags_toInt mods) .&. (Qt.qFlags_toInt Qt.fControlModifier) /= 0
        then do
            Qt.IPoint _ ey <- Qt.scenePos evt ()
            swapMVar (regionState region) (RegionResize ey)
            return ()
        else when (Qt.qFlags_toInt mods == 0) $ do
            Qt.IPoint ix iy <- Qt.scenePos item ()
            Qt.IPoint ex ey <- Qt.scenePos evt ()
            swapMVar (regionState region) (RegionMove (ix-ex) (iy-ey))
            Qt.accept evt ()

regionMouseMoveHandler :: Region -> (Input -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseMoveHandler region onChanged item evt = do
    Qt.IPoint x y <- Qt.scenePos evt ()
    state <- readMVar (regionState region)
    case state of
        RegionIdle ->
            return ()
        RegionMove dx dy -> do
            let pos = (x+dx, y+dy)
            putStrLn $ "Region " ++ show (regionId region) ++ " pos=" ++ show pos
            onChanged $ Update $ FSpace.updateRegion (regionId region) (\r -> r { FSpace.center = V.fromList [fst pos, snd pos] })
            Qt.setPos item pos
            return ()
        RegionResize y0 -> do
            Qt.IRect _ _ d _ <- Qt.qboundingRect item ()
            let dy = y0 - y
                d' = max (minRadius*2) $ min 1 $ d + dy
                r' = d'/2
            putStrLn $ "Region " ++ show (regionId region) ++ " radius=" ++ show r'
            onChanged $ Update $ FSpace.updateRegion (regionId region) (\r -> r { FSpace.radius = r' })
            Qt.qsetRect item (Qt.IRect (-r') (-r') d' d')
            swapMVar (regionState region) (RegionResize y)
            return ()

regionMouseReleaseHandler :: Region -> (Input -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
regionMouseReleaseHandler region onChanged item evt = do
    swapMVar (regionState region) RegionIdle
    return ()

initScene :: FeatureSpaceView -> FeatureSpace -> MVar State -> (Input -> IO ()) -> (Unit.Unit -> IO ()) -> IO ()
initScene view model state onChanged playUnit = do
    -- Qt.setHandler view "mousePressEvent(QGraphicsSceneMouseEvent*)" $ sceneMousePressHandler state
    -- Qt.setHandler view "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ sceneMouseReleaseHandler state
    Qt.setSceneRect view (Qt.rectF 0 0 1 1)
    Qt.setHandler view "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent state
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
    mapM_ mkUnit (FSpace.units model)
    colors <- UI.defaultColorsFromFile
    mapM_ (uncurry $ mkRegion state) $ zip (Map.toList (FSpace.regions model)) colors
    where
        mkUnit u = do
            let (x, y) = pair (Feature.value (FSpace.feature u))
                r      = minRadius
                box    = Qt.rectF (-r) (-r) (r*2) (r*2)
            item <- Qt.qGraphicsEllipseItem_nf box
            Qt.setPos item (Qt.pointF x y)
            -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
            Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler $
                readMVar state >>= flip when (playUnit (FSpace.unit u)) . playUnits
            Qt.setAcceptsHoverEvents item True
            Qt.addItem view item
        mkRegion state (index, region) color = do
            let r = FSpace.radius region
                x = FSpace.center region V.! 0
                y = FSpace.center region V.! 1
            item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (-r) (-r) (r*2) (r*2))
            Qt.setBrush item =<< Qt.qBrush_nf color
            -- Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf? QGraphicsItem::ItemSendsGeometryChanges?
            Qt.addItem view item
            Qt.setPos item (Qt.pointF x y)
            -- Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
            region' <- Region index item `fmap` newMVar RegionIdle
            Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)"   $ regionMousePressHandler   region' onChanged
            Qt.setHandler item "mouseMoveEvent(QGraphicsSceneMouseEvent*)"    $ regionMouseMoveHandler    region' onChanged
            Qt.setHandler item "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ regionMouseReleaseHandler region' onChanged
            modifyMVar_ state (\s -> return $ s { regions = region' : regions s })

addRegions :: [FSpace.Region] -> FeatureSpace -> FeatureSpace
addRegions regions fspace = foldl (\fs (i, r) -> FSpace.insertRegion i r fs)
                          fspace
                          (zip [0..] regions)

inputLoop ichan ochan fspace = do
    x <- readChan ichan
    case x of
        Update f -> let fspace' = f fspace
                    in fspace' `seq` inputLoop ichan ochan fspace'
        ActivateRegion (t, i) -> do
            let (u, fspace') = FSpace.activateRegion i fspace
            maybe (return ()) (\u -> writeChan ochan (Activate (t, FSpace.unit u))) u
            inputLoop ichan ochan fspace'
        Deactivate _ ->
            inputLoop ichan ochan fspace

data RegionState = RegionIdle | RegionMove Double Double | RegionResize Double

data Region = Region {
    regionId    :: Int
  , regionItem  :: Qt.QGraphicsEllipseItem ()
  , regionState :: MVar RegionState
  }

data State = State {
    regions :: [Region]
  , playUnits :: Bool
  }

featureSpaceView :: FeatureSpace -> Chan Input -> IO (FeatureSpaceView, Chan Output)
featureSpaceView fspace0 ichan = do
    let fspace = addRegions (replicate 4 (FSpace.Region (V.fromList [0.5, 0.5]) 0.025)) fspace0
    this <- featureSpaceView_
    ochan <- newChan
    state <- newMVar (State [] False)

    initScene this fspace state (writeChan ichan) (writeChan ochan . Activate . (,) (-1))

    forkIO $ inputLoop ichan ochan fspace
    
    return (this, ochan)
