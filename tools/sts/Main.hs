{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Accessor
import           Data.Char ( toLower )
import           Data.Colour
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.IORef
import qualified Data.List as List
import qualified Data.List.Zipper as Z
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word16)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Graphics.Rendering.Chart as C
import           Graphics.Rendering.Chart.Grid as C
import           Graphics.Rendering.Chart.Gtk
import           Graphics.UI.Gtk as G
import           Graphics.UI.Gtk.Glade
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.ColourMap
import qualified Mescaline.Database as DB
import qualified Paths_mescaline_sts as Paths
import           Reactive.Banana hiding (filter)
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Model as RM
import           Statistics.Function (indexed)
import           Statistics.KernelDensity (Points, epanechnikovPDF, fromPoints)
import           Statistics.Types (Sample)
import           System.FilePath

data SoundFile = SoundFile { fileId :: DB.SourceFileId, file :: DB.SourceFile, marked :: Bool }
                 deriving (Typeable)

fileColour :: ColourMap DB.SourceFileId a -> SoundFile -> Colour a
fileColour m s = m Map.! fileId s

colourToGdk :: (Floating a, Fractional a, RealFrac a) => Colour a -> Color
colourToGdk c = Color (r2w (channelRed rgb)) (r2w (channelGreen rgb)) (r2w (channelBlue rgb))
    where
        rgb = toRGBUsingSpace sRGBSpace c
        r2w r = truncate (r * fromIntegral (maxBound :: Word16))

data ScatterPlotAxis = ScatterPlotAxis {
    scatterPlotDescriptorId :: DB.DescriptorId
  , scatterPlotDescriptor :: DB.Descriptor
  , scatterPlotIndex :: Int
  } deriving (Show, Typeable)

scatterPlotAxisTitle :: ScatterPlotAxis -> String
scatterPlotAxisTitle x = descriptorShortName (scatterPlotDescriptor x) ++ " (" ++ show (scatterPlotIndex x) ++ ")"

data ScatterPlot = ScatterPlot {
    scatterPlotX :: ScatterPlotAxis
  , scatterPlotY :: ScatterPlotAxis
  } deriving (Show, Typeable)

descriptorShortName :: DB.Descriptor -> String
descriptorShortName = takeFileName . DB.descriptorName

scatterPlotAxisMenu :: [(String, [ScatterPlotAxis])] -> (ScatterPlotAxis -> IO ()) -> IO Menu
scatterPlotAxisMenu descr action = do
    menu <- menuNew
    mapM_ (createMenuItem0 menu) descr
    widgetShowAll menu
    return menu
    where
        createMenuItem0 menu (name, axes) = do
            item <- menuItemNewWithLabel name
            menuShellAppend menu item
            case axes of
                [] -> return ()
                _ -> do
                    subMenu <- menuNew
                    menuItemSetSubmenu item subMenu
                    mapM_ (createMenuItem1 subMenu) axes
        createMenuItem1 menu axis = do
            item <- menuItemNewWithLabel (show (scatterPlotIndex axis))
            menuShellAppend menu item
            onActivateLeaf item (action axis)

getFeature :: DB.DescriptorId -> [DB.Feature] -> DB.Feature
getFeature d = fromJust . List.find ((== d) . DB.featureDescriptor)

renderScatterPlot :: ColourMap DB.SourceFileId Double -> DB.UnitMap -> ScatterPlot -> Renderable (Layout1Pick Double Double)
renderScatterPlot sfMap units sp = layout1ToRenderable layout
    where
        -- bars = plot_errbars_values ^= [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
        --      $ plot_errbars_title ^="test"
        --      $ defaultPlotErrBars

        points sf colour =
            let us = Map.filter (\(u, _) -> sf == DB.unitSourceFile u) units
                value axis = flip (V.!) (scatterPlotIndex axis)
                           . DB.featureValue
                           . getFeature (scatterPlotDescriptorId axis)
                vs = map (\(_, fs) -> (value (scatterPlotX sp) fs, value (scatterPlotY sp) fs)) (Map.elems us)
            in plot_points_style ^= filledCircles 2 (opaque colour)
               $ plot_points_values ^= vs
               -- $ plot_points_title ^= "Test data"
               $ defaultPlotPoints

        layout = -- layout1_title ^= "Units"
                 layout1_plots ^= map (\(sf, c) -> Left (toPlot (points sf c))) (Map.toList sfMap)
               -- $ layout1_left_axis   ^= (laxis_title ^= scatterPlotAxisTitle (scatterPlotY sp) $ defaultLayoutAxis)
               -- $ layout1_bottom_axis ^= (laxis_title ^= scatterPlotAxisTitle (scatterPlotX sp) $ defaultLayoutAxis)
               $ defaultLayout1

type Histogram = (Points, U.Vector Double)

featurePDFs :: Map DB.DescriptorId DB.Descriptor -> DB.UnitMap -> Map DB.DescriptorId [Histogram]
featurePDFs ds us = Map.mapWithKey (\di -> fmap (kde di) . indices) ds
    where
        indices d = [0..DB.descriptorDegree d - 1]
        kde di i = estimate . V.fromList . map (flip (V.!) i . DB.featureValue . getFeature di . snd) . Map.elems $ us
        estimate :: U.Vector Double -> Histogram
        estimate = epanechnikovPDF 100

renderKDE :: Bool -> ScatterPlotAxis -> Map DB.DescriptorId [Histogram] -> Renderable (Layout1Pick Double Double)
renderKDE flipped axis pdfs = layout1ToRenderable layout
    where
        descr = scatterPlotAxisTitle axis
        (points, pdf) = (pdfs Map.! scatterPlotDescriptorId axis) !! scatterPlotIndex axis

        layout = -- layout1_title ^= "Densities for \"" ++ descr ++ "\""
                 layout1_plots ^= [ Left (toPlot info) ]
               $ layout1_left_axis ^= (if flipped then featureAxis else pdfAxis)
               $ layout1_bottom_axis ^= (if flipped then pdfAxis else featureAxis)
               $ defaultLayout1 :: Layout1 Double Double

        pdfAxis = -- laxis_title ^= "estimate of probability density"
                  defaultLayoutAxis
        pdfValues = V.toList (fromPoints points)

        featureAxis = -- laxis_generate ^= semiAutoScaledAxis secAxis
                      laxis_title ^= descr
                    $ defaultLayoutAxis
        featureValues = V.toList (V.map (/ V.sum pdf) pdf)

        -- semiAutoScaledAxis opts ps = autoScaledAxis opts (extremities ++ ps)
        -- extremities = maybe [] (\(lo, hi) -> [lo, hi]) exs

        info = plot_lines_values ^= [ zip (if flipped then featureValues else pdfValues)
                                          (if flipped then pdfValues else featureValues) ]
             $ defaultPlotLines

renderChart :: Renderable (Layout1Pick Double Double)
            -> Renderable (Layout1Pick Double Double)
            -> Renderable (Layout1Pick Double Double)
            -> Renderable (Layout1Pick Double Double)
renderChart a b c = gridToRenderable $ weights (1,1) $ g
    where
        g = aboveN
            [ f c     `beside` tval a
            , C.empty `beside` f b
            ]
        f = tval . setPickFn (const Nothing)
        e = tval $ spacer (20,20)

{-----------------------------------------------------------------------------
Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you 
data EventSource a = EventSource {
    setHandler :: (a -> IO ()) -> IO ()
  , getHandler :: IO (a -> IO ())
  }

newEventSource :: IO (EventSource a)
newEventSource = do
    ref <- newIORef (const $ return ())
    return EventSource { setHandler = writeIORef ref
                       , getHandler = readIORef ref }

addHandler :: EventSource a -> AddHandler a
addHandler es k = do
    handler <- getHandler es
    setHandler es (\x -> handler x >> k x)

fire :: EventSource a -> a -> IO ()
fire es x = getHandler es >>= ($x)

-- | Pick function wrapper type.
data PickFunction a = NoPick | Pick (PickFn a) deriving (Typeable)

instance Show (PickFunction a) where
    show NoPick   = "NoPick"
    show (Pick _) = "Pick"

instance Functor PickFunction where
    fmap _ NoPick    = NoPick
    fmap f (Pick pf) = Pick (fmap f . pf)

pick :: PickFunction a -> C.Point -> Maybe a
pick NoPick    = const Nothing
pick (Pick pf) = pf

-- | A version opf updateCanvas that passes the pick function to a callback.
updateCanvas' :: G.DrawingArea -> (PickFunction a -> IO ()) -> Renderable a -> IO Bool
updateCanvas' canvas pickFnSrc chart = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    let sz = (fromIntegral width,fromIntegral height)
    pickf <- G.renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
    pickFnSrc (Pick pickf)
    return True

newtype L1P x y = L1P { unL1P :: Layout1Pick x y } deriving (Show, Typeable)

-- | User interface event abstraction.
data UIEvent =
    MouseEvent {
        uiEventTime :: TimeStamp
      , uiEventCoordinates :: (Double,Double)
      , uiEventButton :: Int
      }
  | MotionEvent {
        uiEventTime :: TimeStamp
      , uiEventCoordinates :: (Double,Double)
      }
    deriving (Show, Typeable)

mouseEventHandler :: (UIEvent -> IO ()) -> EventM EButton Bool
mouseEventHandler a = do
    MouseEvent <$> eventTime <*> eventCoordinates <*> liftM fromEnum eventButton >>= liftIO . a
    return True

motionEventHandler :: (UIEvent -> IO ()) -> EventM EMotion Bool
motionEventHandler a = do
    MotionEvent <$> eventTime <*> eventCoordinates >>= liftIO . a
    return True

newColumn :: ( CellRendererClass cell
             , TreeModelClass (model row)
             , TypedTreeModelClass model ) =>
             model row
          -> String
          -> IO cell
          -> (row -> [AttrOp cell])
          -> IO (TreeViewColumn, cell)
newColumn model label newRenderer attrs = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col label
    renderer <- newRenderer
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model attrs
    return (col, renderer)

appMain :: AppT IO ()
appMain = do
    [gladeFile, dbFile] <- App.getArgs
    (descriptors, (sfs, us)) <- DB.withDatabase dbFile $ do
        a <- DB.descriptorMap
        b <- DB.queryFeatures (const True) (Map.keys a)
        return (a, b)
    let colourMap = hsv (Map.keys sfs)
        scatterPlotAxes = map (\(di, d) -> (descriptorShortName d, map (ScatterPlotAxis di d) [0..DB.descriptorDegree d - 1]))
                              (Map.toList descriptors)
    liftIO $ do
        initGUI
        unsafeInitGUIForThreadedRTS

        Just xml <- xmlNew gladeFile
        win <- xmlGetWidget xml castToWindow "window"
        onDestroy win mainQuit

        -- create a new list model
        model <- listStoreNew $ fmap (\(si, s) -> SoundFile si s True) $ Map.toList sfs
        view <- xmlGetWidget xml castToTreeView "soundFiles"
        treeViewSetModel view model

        treeViewSetHeadersVisible view True

        -- add a couple columns
        (col1, _) <- newColumn model "File" cellRendererTextNew $ \row -> [ cellText := takeFileName . DB.sourceFileUrl . file $ row
                                                                          , cellTextBackgroundColor := colourToGdk . fileColour colourMap $ row ]
        treeViewColumnSetResizable col1 True
        treeViewAppendColumn view col1

        (col2, _) <- newColumn model "Channels" cellRendererTextNew $ \row -> [ cellText := show . DB.sourceFileNumChannels . file $ row ]
        treeViewAppendColumn view col2

        (col3, _) <- newColumn model "Sample rate" cellRendererTextNew $ \row -> [ cellText := show . DB.sourceFileSampleRate . file $ row ]
        treeViewAppendColumn view col3

        (col4, renderer4) <- newColumn model "Active" cellRendererToggleNew $ \row -> [ cellToggleActive := marked row ]
        treeViewAppendColumn view col4

        -- Update the model when the toggle buttons are activated
        on renderer4 cellToggled $ \pathStr -> do
            let (i:_) = stringToTreePath pathStr
            val <- listStoreGetValue model i
            listStoreSetValue model i val { marked = not (marked val) }

        -- enable interactive search
        treeViewSetEnableSearch view True
        treeViewSetSearchEqualFunc view $ Just $ \str iter -> do
            (i:_) <- treeModelGetPath model iter
            row <- listStoreGetValue model i
            return (map toLower str `List.isPrefixOf` map toLower (DB.sourceFileUrl . file $ row))

        -- Drawing area for plots
        canvas <- xmlGetWidget xml castToDrawingArea "plot"
        set canvas [ widgetWidthRequest := 800, widgetHeightRequest := 800 ]
        widgetAddEvents canvas [Button1MotionMask]

        -- Event sources
        pickFnSrc <- newEventSource
        scatterPlotSrc <- newEventSource

        -- Set up event network
        prepareEvents $ do
            -- Event fired as sound files are marked or unmarked
            soundFiles <- fromAddHandler $ \a -> void $ on renderer4 cellToggled $ const (listStoreToList model >>= a)
            soundFiles0 <- liftIO $ listStoreToList model
            -- Expose event for canvas
            expose <- fromAddHandler $ \a -> void $ on canvas exposeEvent $ liftIO (a ()) >> return True
            -- Canvas button events
            buttonPress <- fromAddHandler $ void . on canvas buttonPressEvent . mouseEventHandler
            buttonMove <- fromAddHandler $ void . on canvas motionNotifyEvent . motionEventHandler
            -- The picks from the plot area ((Double,Double) -> Maybe Layout1Pick)
            picks <- liftM (fmap ((uncurry Point >>>) . pick) . stepper NoPick) $ fromAddHandler $ addHandler pickFnSrc
            let -- Unfortunately we don't get to know the axis when picking a label!
                -- How to do more automatized Typeable wrapping/unwrapping?
                buttonPressInAxis = flip mapFilter (((\f e -> (e, f (uiEventCoordinates e))) <$> picks) `apply` buttonPress)
                                            $ \(e, p) -> case p of
                                                Just (L1P (L1P_BottomAxis x)) -> Just (e, Left x)
                                                Just (L1P (L1P_LeftAxis x)) -> Just (e, Right x)
                                                _ -> Nothing
                -- left' z = let z' = if Z.beginp z then Z.left (Z.end z) else Z.left z
                --           in (Z.cursor z', z')
                -- right' z = let z' = Z.right z
                --                z'' = if Z.endp z' then Z.start z else z'
                --            in (Z.cursor z'', z'')
                -- updateScatterPlotAxis (button, click) (sp, (xAxes, yAxes)) =
                --     let inc = if button == 1
                --               then left'
                --               else if button == 3
                --                    then right'
                --                    else \z -> (Z.cursor z, z)
                --     in case click of
                --         Left _  -> let (a, z) = inc xAxes in (sp { scatterPlotX = a }, (z, yAxes))
                --         Right _ -> let (a, z) = inc yAxes in (sp { scatterPlotY = a }, (xAxes, z))
                -- scatterPlot = fst <$> accumB ( ScatterPlot (head scatterPlotAxes) (head scatterPlotAxes)
                --                              , (Z.fromList scatterPlotAxes, Z.fromList scatterPlotAxes) )
                --                              (updateScatterPlotAxis <$> buttonPressInAxis)
            scatterPlotAxis <- fromAddHandler $ addHandler scatterPlotSrc
            let units = flip fmap (stepper soundFiles0 soundFiles) $ \sfs ->
                            -- Filter units by IDs of marked sound files
                            let ids = Set.fromList (map fileId (filter marked sfs))
                            in Map.filter (flip Set.member ids . DB.unitSourceFile . fst) us
                pdfs = featurePDFs descriptors <$> units
                scatterPlotState = accumB (ScatterPlot (head . snd . head $ scatterPlotAxes) (head  . snd . head $ scatterPlotAxes))
                                     ((\e sp -> either (\x -> sp { scatterPlotX = x })
                                                       (\y -> sp { scatterPlotY = y }) e) <$> scatterPlotAxis)
                scatterPlot = renderScatterPlot colourMap <$> units <*> scatterPlotState
                kdePlotX = ((renderKDE False . scatterPlotX) <$> scatterPlotState <*> pdfs)
                kdePlotY = ((renderKDE True  . scatterPlotY) <$> scatterPlotState <*> pdfs)
                chart = renderChart <$> scatterPlot <*> kdePlotX <*> kdePlotY
                popupAxisMenu e p = scatterPlotAxisMenu scatterPlotAxes (fire scatterPlotSrc . either (const Left) (const Right) p) >>=
                                    flip menuPopup (Just (toEnum (uiEventButton e), uiEventTime e))
                redraw = ignore soundFiles `union` ignore scatterPlotAxis
            -- Update canvas with current plot and feed back new pick function
            reactimate $ ((const . void . updateCanvas' canvas (fire pickFnSrc . fmap L1P)) <$> chart) `apply` expose
            -- Display the axis popup menu if needed
            reactimate $ uncurry popupAxisMenu <$> (R.filter ((==3) . uiEventButton . fst) buttonPressInAxis)
            -- Schedule redraw
            reactimate $ widgetQueueDraw canvas <$ redraw
            -- Print the picks when a button is pressed in the plot area
            reactimate $ print <$> (picks `apply` fmap uiEventCoordinates (buttonPress `union` buttonMove))
            reactimate $ print <$> buttonPressInAxis

        widgetShowAll win
        mainGUI

main :: IO ()
main = App.runAppT appMain =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles

-- Reactive combinators
mapFilter :: FRP f => (a -> Maybe b) -> RM.Event f a -> RM.Event f b
mapFilter f = fmap fromJust . R.filter isJust . fmap f

ignore :: Functor f => f a -> f ()
ignore = (<$) ()
