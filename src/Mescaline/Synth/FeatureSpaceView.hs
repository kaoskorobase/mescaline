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
import qualified Data.IntMap as Map
import qualified Data.Vector.Generic as V
import qualified Mescaline.Application as App
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

data Input     = Update (FeatureSpace -> FeatureSpace) | ActivateRegion (Double, Int) | Deactivate Unit.Unit
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

hoverHandler :: MVar State -> Unit.Unit -> (Unit.Unit -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneHoverEvent () -> IO ()
hoverHandler state unit action this evt = do
    -- putStrLn "hoverHandler"
    s <- readMVar state
    if playUnits s
        then action unit
        else return ()
    Qt.hoverEnterEvent_h this evt

clusterChange :: Int -> ((FeatureSpace -> FeatureSpace) -> IO ()) -> Qt.QGraphicsEllipseItem () -> Qt.GraphicsItemChange -> Qt.QVariant () -> IO (Qt.QVariant ())
clusterChange regionId onChanged item itemChange variant = do
    if (itemChange == Qt.eItemPositionChange)
        then do
            Qt.IPoint x y <- Qt.scenePos item ()
            putStrLn $ "eItemPositionChange " ++ show (x, y)
        else return ()
    return variant

initScene :: FeatureSpaceView -> FeatureSpace -> MVar State -> ((FeatureSpace -> FeatureSpace) -> IO ()) -> (Unit.Unit -> IO ()) -> IO ()
initScene view model state changed playUnit = do
    -- Qt.setHandler view "mousePressEvent(QGraphicsSceneMouseEvent*)" $ sceneMousePressHandler state
    -- Qt.setHandler view "mouseReleaseEvent(QGraphicsSceneMouseEvent*)" $ sceneMouseReleaseHandler state
    Qt.setHandler view "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent state
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" $ sceneKeyReleaseEvent state
    mapM_ mkUnit (units model)
    colors <- fmap (fmap snd) $ regionsFromFile =<< App.getResourcePath "regions.txt"
    mapM_ (uncurry $ mkRegion state) $ zip (Map.toList (FSpace.regions model)) colors
    where
        mkUnit u = do
            let (x, y) = pair (Feature.value (feature u))
                box = Qt.rectF x y 0.01 0.01
            item <- Qt.qGraphicsRectItem_nf box
            -- Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
            Qt.setHandler item "hoverEnterEvent(QGraphicsSceneHoverEvent*)" $ hoverHandler state (unit u) playUnit
            Qt.setAcceptsHoverEvents item True
            Qt.addItem view item
        mkRegion state (index, region) color = do
            let r = radius region
                x = center region V.! 0
                y = center region V.! 1
            item <- Qt.qGraphicsEllipseItem_nf (Qt.rectF (x-r) (y-r) (r*2) (r*2))
            Qt.setBrush item =<< Qt.qBrush color
            Qt.setFlags item $ Qt.fItemIsMovable + (Qt.qFlags_fromInt 2048) -- FIXME: Huh? Wtf?
            Qt.setHandler item "(QVariant)itemChange(QGraphicsItem::GraphicsItemChange,const QVariant&)" $ clusterChange index changed
            Qt.addItem view item
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

data State = State {
    regionItems :: [Qt.QGraphicsEllipseItem ()]
  , playUnits :: Bool
  }

featureSpaceView :: FeatureSpace -> Chan Input -> IO (FeatureSpaceView, Chan Output)
featureSpaceView fspace0 ichan = do
    regions <- regionsFromFile =<< App.getResourcePath "regions.txt"
    let fspace = addRegions (map fst regions) fspace0
    this <- featureSpaceView_
    ochan <- newChan
    state <- newMVar (State [] False)

    initScene this fspace state (writeChan ichan . Update) (writeChan ochan . Activate . (,) (-1))

    forkIO $ inputLoop ichan ochan fspace
    
    return (this, ochan)
