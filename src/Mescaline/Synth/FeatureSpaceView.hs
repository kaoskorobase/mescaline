{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Mescaline.Synth.FeatureSpaceView (
    FeatureSpaceView
  , featureSpaceView
  , Input(..)
  , Output(..)
  , regionsFromFile
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Data.Bits
import qualified Data.Vector.Generic as V
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace as FSpace
import qualified Qt as Qt

-- type FeatureSpaceView = Qt.QGraphicsSceneSc (CFeatureSpaceView)
type FeatureSpaceView = Qt.QGraphicsScene ()
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
-- featureSpaceView_ = Qt.qSubClass (Qt.qGraphicsScene ())
featureSpaceView_ = Qt.qGraphicsScene ()

data Input     = Model FeatureSpace | ActivateRegion (Double, Int) | Deactivate Unit.Unit
newtype Output = Activate (Double, Unit.Unit)

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

mouseHandler :: Unit.Unit -> (Unit.Unit -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler unit action this evt = action unit >> Qt.mousePressEvent_h this evt

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

hoverHandler :: MVar Bool -> Unit.Unit -> (Unit.Unit -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneHoverEvent () -> IO ()
hoverHandler state unit action this evt = do
    putStrLn "hoverHandler"
    b <- readMVar state
    if True -- b
        then action unit
        else return ()
    Qt.hoverEnterEvent_h this evt

-- createPoly :: DiagramType -> IO (QPolygonF ())
-- createPoly dt
--  | dt == StartEnd
--   = do
--     pth <- qPainterPath ()
--     let m200 = 200::Double
--         m50 = 50::Double
--         a0 = 0::Double
--         a50 = 50::Double
--         a150 = 150::Double
--         a90 = 90::Double
--         a180 = 180::Double
--         a270 = 270::Double
--         l200 = 200::Double
--         l25 = 25::Double
--     qmoveTo pth (m200, m50)
--     arcTo pth (a150, a0, a50, a50, a0, a90)
--     arcTo pth (a50, a0, a50, a50, a90, a90)
--     arcTo pth (a50, a50, a50, a50, a180, a90)
--     arcTo pth (a150, a50, a50, a50, a270, a90)
--     lineTo pth (l200, l25)
--     toFillPolygon pth ()
--  | dt == Conditional 
--   = qPolygonFL [pointF (-100) 0, pointF 0 100, pointF 100 0, pointF 0 (-100), pointF (-100) 0] 
--  | dt == Step 
--   = qPolygonFL [pointF (-100) (-100), pointF 100 (-100), pointF 100 100, pointF (-100) 100, pointF (-100) (-100)] 
--  | otherwise 
--   = qPolygonFL [pointF (-120) (-80), pointF (-70) 80, pointF 120 80, pointF 70 (-80), pointF (-120) (-80)] 

-- dio <- qGraphicsPolygonItem_nf ()
-- di_pf <- createPoly typ
-- setPolygon dio di_pf
-- setFlags dio $ fItemIsMovable + fItemIsSelectable + (qFlags_fromInt 2048)

-- data ClusterItem = ClusterItem {
--     di_o :: QGraphicsEllipseItem ()
--   }
-- 
-- data DiUms = ItemId | DeleteItem | GetItemAvna | AddArrow | RemoveArrow deriving Enum
-- 
-- um_ItemId = fromEnum ItemId
-- um_DeleteItem = fromEnum DeleteItem
-- um_GetItemAvna = fromEnum GetItemAvna
-- um_AddArrow = fromEnum AddArrow
-- um_RemoveArrow = fromEnum RemoveArrow
-- 
-- class QclusterItem x1 where
--     clusterItem :: x1 -> IO ClusterItem
-- 
-- instance QclusterItem (RectF) where
--     clusterItem x1 = do
--         dio <- qGraphicsEllipseItem_nf x1
--         clusterItem_s dio x1
-- 
-- diagramItemType :: Int
-- diagramItemType = (qEnum_toInt (eUserType::QGraphicsItem__)) + 15
-- 
-- diagramItemtype :: QGraphicsEllipseItem () -> IO Int
-- diagramItemtype itm = return diagramItemType
-- 
-- clusterMousePressHandler :: Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- clusterMousePressHandler this evt = do
--     putStrLn "mousePressEvent"
--     Qt.accept evt ()
--     p <- scenePos evt ()
--     Qt.setHandler this "mouseMoveEvent(QGraphicsSceneMouseEvent*)" $ clusterMouseMoveHandler p
--     -- Qt.mousePressEvent_h this evt
-- 
-- clusterMouseMoveHandler :: Qt.PointF -> Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- clusterMouseMoveHandler p0 this evt = do
--     p1 <- scenePos evt  ()
--     putStrLn $ "mouseMoveEvent " ++ show p0 ++ " " ++ show p1
--     -- Qt.scenePos this () >>= print
--     Qt.mouseMoveEvent_h this evt
-- 
-- clusterMouseReleaseHandler :: Qt.QGraphicsEllipseItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
-- clusterMouseReleaseHandler this evt = do
--     putStrLn "mouseReleaseEvent"
--     Qt.unSetHandler this "mouseMoveEvent(QGraphicsSceneMouseEvent*)"
--     return ()
--     -- Qt.mouseReleaseEvent_h this evt
-- 
-- clusterItem_s :: QGraphicsEllipseItem () -> RectF -> IO ClusterItem
-- clusterItem_s dio bounds = do
--     -- setRect dio bounds
--     -- setFlags dio fItemIsMovable
--     -- tnmo <- qCast_QMenu mnu
--     -- di_ar <- newIORef []
--     -- di_asa <- newIORef []
--     -- di_mca <- newIORef (0::Int)
--     -- di_aca <- newIORef (0::Int)
--     let tdi = ClusterItem dio
--     -- setHandler dio "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ diItemChange tdi
--     -- Qt.setHandler dio "mousePressEvent(QGraphicsSceneMouseEvent*)" $ clusterMousePressHandler
--     -- Qt.setHandler dio "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ clusterMouseReleaseHandler
--     -- setHandler dio "contextMenuEvent(QGraphicsSceneContextMenuEvent*)" $ diContextMenuEvent tnmo
--     setHandler dio "(int)type()" diagramItemtype
--     return tdi
-- 
-- diagramItem_delete :: ClusterItem -> IO ()
-- diagramItem_delete di = qGraphicsEllipseItem_delete (di_o di)
-- 
-- diItemChange :: ClusterItem -> QGraphicsEllipseItem () -> GraphicsItemChange -> QVariant () -> IO (QVariant ())
-- diItemChange di i ic vr
--   = do
--     if (ic == eItemPositionChange)
--      then
--       do
--       p <- scenePos i ()
--       putStrLn $ "eItemPositionChange " ++ show p
--       -- mapM_ (\(s, a) -> updatePosition a) =<< diArrows di
--      else return ()
--     return vr

-- umItemId :: Int -> QGraphicsEllipseItem () -> QVariant () -> IO (QVariant ())
-- umItemId id item p1 = qVariant id 
-- 
-- itemId :: QGraphicsEllipseItem () -> IO Int
-- itemId item = qVariantValue_Int =<< userMethod item um_ItemId =<< qCast_QVariant objectNull

-- diContextMenuEvent :: QMenu () -> QGraphicsPolygonItem () -> QGraphicsSceneContextMenuEvent () -> IO ()
-- diContextMenuEvent mnu pi ce
--   = do
--     s <- scene pi ()
--     clearSelection s ()
--     setSelected pi True
--     sp <- screenPos ce ()
--     ta <- exec mnu sp
--     return ()

-- clusterChange :: Qt.QGraphicsEllipseItem () -> Qt.GraphicsItemChange -> Qt.QVariant () -> IO (Qt.QVariant ())
-- clusterChange i ic vr = do
--     putStrLn $ "itemChange " ++ show ic
--     if (ic == Qt.eItemPositionChange)
--         then Qt.scenePos i () >>= print
--         else return ()
--     return vr

initScene :: FeatureSpaceView -> FeatureSpace -> MVar Bool -> (Unit.Unit -> IO ()) -> IO ()
initScene view model state action = do
    -- Qt.setHandler view "mousePressEvent(QGraphicsSceneMouseEvent*)" $ sceneMousePressHandler state
    -- Qt.setHandler view "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ sceneMouseReleaseHandler state
    mapM_ mkUnit (units model)
    colors <- fmap (fmap snd) $ regionsFromFile "regions.txt"
    mapM_ (uncurry mkRegion) (zip (regionList model) colors)
    where
        mkUnit u = do
            let (x, y) = pair (Feature.value (feature u))
                box = Qt.rectF x y 0.01 0.01
            item <- Qt.qGraphicsRectItem_nf box
            -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
            Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler state (unit u) action
            Qt.setAcceptsHoverEvents item True
            Qt.addItem view item
        mkRegion region color = do
            let r = radius region
                x = center region V.! 0
                y = center region V.! 1
            item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (x-r) (y-r) (r*2) (r*2))
            Qt.setBrush item =<< Qt.qBrush color
            -- Qt.setFlags cluster $ Qt.fItemIsMovable
            -- Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange
            Qt.addItem view item

-- regions :: [((Double, Double), Double)]
-- regions = [((0,0), 0.25), ((0.4,0.4), 0.1), ((0.2,0.8), 0.03), ((0.1,0.3),0.001)]

addRegions :: [Region] -> FeatureSpace -> FeatureSpace
addRegions regions fspace = foldl (\fs (i, r) -> insertRegion i r fs)
                          fspace
                          (zip [0..] regions)

inputLoop ichan ochan fspace = do
    x <- readChan ichan
    case x of
        Model _ -> inputLoop ichan ochan fspace
        ActivateRegion (t, i) -> do
            let (u, fspace') = activateRegion i fspace
            maybe (return ()) (\u -> writeChan ochan (Activate (t, unit u))) u
            inputLoop ichan ochan fspace'
        Deactivate _ ->
            inputLoop ichan ochan fspace

qColorFromRgbaF :: (Double, Double, Double, Double) -> IO (Qt.QColor ())
qColorFromRgbaF (r, g, b, a) = Qt.qColorFromRgba
                                ( (shiftL (round (a*255)) 24)
                              .|. (shiftL (round (r*255)) 16)
                              .|. (shiftL (round (g*255)) 8)
                              .|. (shiftL (round (b*255)) 0) )

regionsFromFile :: FilePath -> IO [(Region, Qt.QColor ())]
regionsFromFile path = do
    s <- readFile path
    mapM (f.words) (lines s)
    where
        f [x, y, r, cr, cg, cb, ca] = do
            c <- qColorFromRgbaF (read cr, read cg, read cb, read ca)
            -- c' <- Qt.qColorFromRgba (read c :: Int)
            return (Region (V.fromList [read x, read y]) (read r), c)
        f _ = error "regionsFromFile: parse error"
                    
featureSpaceView :: FeatureSpace -> Chan Input -> IO (FeatureSpaceView, Chan Output)
featureSpaceView fspace0 ichan = do
    regions <- regionsFromFile "regions.txt"
    let fspace = addRegions (map fst regions) fspace0
    this <- featureSpaceView_
    ochan <- newChan
    state <- newMVar False

    initScene this fspace state (writeChan ochan . Activate . (,) (-1))

    forkIO $ inputLoop ichan ochan fspace
    
    return (this, ochan)
