module Mescaline.Synth.FeatureSpaceView where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Colour.SRGB.Linear (rgb)
import qualified Data.Vector.Generic as V
import Data.IORef
import Graphics.Rendering.Cairo (Render)
import Graphics.UI.Gtk hiding (Image, Point, get)
-- import Graphics.UI.Gtk.Diagram
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import qualified System.Glib.MainLoop as MainLoop

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render)
import Control.Monad.State (liftIO)
import Data.List (find)
import Data.Maybe
import Graphics.UI.Gtk hiding (Point)
import Graphics.UI.Gtk.Gdk.EventM

import Debug.Trace

type FeatureSpaceView = DrawingArea

-- type Rect = (Double, Double, Double, Double)
-- 
-- data Params a = Params {
--     boxSize   :: Double
--   , padding   :: Double
--   , sequencer :: Sequencer a
--   , bboxes    :: [[((Int,Int), Rect)]] }

type Params = [(Unit.Unit, Feature.Feature)]
type Output = [Unit.Unit]

viewSizeRequest :: Params -> IO Requisition
viewSizeRequest _ = return $ Requisition 0 300

-- withState :: Render a -> Render a
-- withState action = do
--     C.save
--     a <- action
--     C.restore
--     return a
--     
-- sequencerRender :: Params a -> Render ()
-- sequencerRender p = do
--     withState $ do
--         C.translate (padding p) (padding p)
--         forM_ [0..cols (sequencer p) - 1] $ \r -> do
--             withState $ do
--                 C.translate 0 (fromIntegral r * (boxSize p + padding p))
--                 forM_ [0..rows (sequencer p) - 1] $ \c -> do
--                     withState $ do
--                         C.translate (fromIntegral c * (boxSize p + padding p)) 0
--                         C.rectangle 0 0 (boxSize p) (boxSize p)
--                         let i = (r, c)
--                         if s `isElemAtIndex` i
--                             then C.setSourceRGB 0.3 0.3 0.3 >> C.fillPreserve
--                             else if s `isCursorAtIndex` i
--                                 then C.setSourceRGB 0.3 0 0 >> C.fillPreserve
--                                 else return ()
--                         C.setSourceRGB 0 0 0
--                         C.stroke
--     where
--         s = sequencer p

pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

viewRender :: Params -> Double -> Double -> Render ()
viewRender p w h = mapM_ (render.pair.Feature.value.snd) p -- >> C.rectangle 0 0 w h >> C.stroke
    where
        render (x, y) = C.rectangle (x*w-1) (y*h-1) 2 2 >> C.fill

viewExpose :: Params -> EventM EExpose Bool
viewExpose p = do
    win <- eventWindow
    liftIO $ do
        (width, height) <- drawableGetSize win
        renderWithDrawable win (viewRender p (fromIntegral width) (fromIntegral height))
    return True
-- 
-- translateRect :: (Double, Double) -> Rect -> Rect
-- translateRect (dx, dy) (x, y, w, h) = (x+dx, y+dy, w, h)
-- 
-- isWithin :: (Double, Double) -> Rect -> Bool
-- (px, py) `isWithin` (x, y, w, h) = px >= x && px <= x+w && py >= y && py <= y+h 
-- 
-- mkBboxes :: Double -> Double -> Sequencer a -> [[((Int,Int), Rect)]]
-- mkBboxes boxSize padding s = map fr [0..rows s - 1]
--     where
--         fr r = map (fc r) [0..cols s - 1]
--         fc r c = ((r, c), (padding + (boxSize+padding) * fromIntegral c, padding + (boxSize+padding) * fromIntegral r, boxSize, boxSize))
-- 
-- pointToIndex :: Params a -> (Double, Double) -> Maybe (Int, Int)
-- pointToIndex params p = fmap fst $ find (isWithin p . snd) $ concat $ bboxes params
-- 
viewMouseEvent :: Chan Output -> Params -> EventM EButton Bool
viewMouseEvent ochan p = do
    (x, y) <- eventCoordinates
    win <- eventWindow
    (w, h) <- liftIO $ drawableGetSize win
    b <- eventButton
    if b == LeftButton
        then liftIO . writeChan ochan . map fst . filter (f x y w h.pair.Feature.value.snd) $ p
        else return ()
    return True
    where
        f x y w h (ux, uy) = let (ux0, ux1) = (ux * fromIntegral w - 3, ux * fromIntegral w + 3)
                                 (uy0, uy1) = (uy * fromIntegral h - 3, uy * fromIntegral h + 3)
                             in x >= ux0 && x <= ux1 && y >= uy0 && y <= uy1

-- -- redraw :: Double -> Double -> Sequencer a -> Diagram
-- -- redraw boxSize padding s = grid $ [ [ pad padding padding $ box r c $ s `isCursorAtIndex` (r, c) | c <- [0..cols s - 1] ] |  r <- [0..rows s - 1] ]
-- --     where
-- --         box r c b = D.Region (r * cols s + c) $ fillColor (fc b) $ rect boxSize boxSize
-- --         fc True  = rgb 0.2 0.2 0.2
-- --         fc False = rgb 1.0 1.0 1.0

newFeatureSpaceView :: Params -> Chan Params -> Chan Output -> IO FeatureSpaceView
newFeatureSpaceView units ichan ochan = do
    -- ref <- newMVar (Params boxSize padding s (mkBboxes boxSize padding s))
    ref <- newMVar units
    w <- drawingAreaNew
    w `on` sizeRequest $ readMVar ref >>= viewSizeRequest
    w `on` exposeEvent $ liftIO (readMVar ref) >>= viewExpose
    w `on` buttonPressEvent $ liftIO (readMVar ref) >>= viewMouseEvent ochan
    -- w <- diagramNew $ \p -> readIORef ref >>= \s -> return (Auto, redraw boxSize padding s)
    -- flip MainLoop.timeoutAdd 5 $ do
    --     e <- isEmptyChan ichan
    --     when (not e) $ do
    --         readChan ichan >>= \s -> modifyMVar_ ref (\p -> let p' = p { sequencer = s } in p' `seq` return p')
    --         widgetQueueDraw w
    --     return True
    forkIO $ fix $ \loop -> do
        s <- readChan ichan
        modifyMVar_ ref (const $ return s)
        postGUIAsync $ widgetQueueDraw w
        loop
    return w
