module Mescaline.Synth.FeatureSpaceView (
    FeatureSpaceView
  , featureSpaceView
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import qualified Data.Vector.Generic as V
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import qualified Qt as Q

type FeatureSpaceView = Q.QGraphicsSceneSc (CFeatureSpaceView)
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
featureSpaceView_ = Q.qSubClass (Q.qGraphicsScene ())

type Params = [(Unit.Unit, Feature.Feature)]
type Output = [Unit.Unit]

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

mouseHandler :: Unit.Unit -> (Unit.Unit -> IO ()) -> Q.QGraphicsRectItem () -> Q.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler unit action this evt = action unit >> Q.mousePressEvent_h this evt

initScene :: FeatureSpaceView -> Params -> (Unit.Unit -> IO ()) -> IO ()
initScene view p action = mapM_ (uncurry unit) p
    where
        unit u f = do
            let (x, y) = pair (Feature.value f)
                box = Q.rectF x y 0.01 0.01
            item <- Q.qGraphicsRectItem_nf box
            Q.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler u action
            Q.addItem view item

featureSpaceView :: Params -> Chan Params -> Chan Output -> IO FeatureSpaceView
featureSpaceView units ichan ochan = do
    -- ref <- newMVar (Params boxSize padding s (mkBboxes boxSize padding s))
    ref <- newMVar units
    this <- featureSpaceView_
    initScene this units $ \unit -> writeChan ochan [unit]

    -- forkIO $ fix $ \loop -> do
    --     s <- readChan ichan
    --     modifyMVar_ ref (const $ return s)
    --     postGUIAsync $ widgetQueueDraw w
    --     loop

    return this
