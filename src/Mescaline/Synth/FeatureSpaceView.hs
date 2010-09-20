module Mescaline.Synth.FeatureSpaceView (
    FeatureSpaceView
  , featureSpaceView
  , Input(..)
  , Output(..)
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Chunked
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Fix (fix)
import qualified Data.Vector.Generic as V
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace
import qualified Qt as Q

type FeatureSpaceView = Q.QGraphicsSceneSc (CFeatureSpaceView)
data CFeatureSpaceView = CFeatureSpaceView

featureSpaceView_ :: IO (FeatureSpaceView)
featureSpaceView_ = Q.qSubClass (Q.qGraphicsScene ())

data Input     = Model FeatureSpace | Deactivate Unit.Unit
newtype Output = Activate Unit.Unit

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

mouseHandler :: Unit.Unit -> (Unit.Unit -> IO ()) -> Q.QGraphicsRectItem () -> Q.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler unit action this evt = action unit >> Q.mousePressEvent_h this evt

initScene :: FeatureSpaceView -> FeatureSpace -> (Unit.Unit -> IO ()) -> IO ()
initScene view model action = mapM_ mkUnit (units model)
    where
        mkUnit u = do
            let (x, y) = pair (Feature.value (feature u))
                box = Q.rectF x y 0.01 0.01
            item <- Q.qGraphicsRectItem_nf box
            Q.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler (unit u) action
            Q.addItem view item

featureSpaceView :: FeatureSpace -> Chan Input -> Chan Output -> IO FeatureSpaceView
featureSpaceView fspace ichan ochan = do
    -- ref <- newMVar (Params boxSize padding s (mkBboxes boxSize padding s))
    ref <- newMVar units
    this <- featureSpaceView_
    initScene this fspace (writeChan ochan . Activate)

    -- forkIO $ fix $ \loop -> do
    --     s <- readChan ichan
    --     modifyMVar_ ref (const $ return s)
    --     postGUIAsync $ widgetQueueDraw w
    --     loop

    return this
