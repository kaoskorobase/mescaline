{-# LANGUAGE BangPatterns
           , DeriveDataTypeable
           , FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Accessor
import           Data.Char ( toLower )
import           Data.Colour
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.IORef
import qualified Data.KDTree as KD
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word16)
import qualified Data.Vector as B
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import           Debug.Trace
import           Graphics.Rendering.Chart as C
import           Graphics.Rendering.Chart.Grid as C
import           Graphics.UI.Gtk as G
import           Graphics.UI.Gtk.Glade
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.ColourMap
import qualified Mescaline.Database as DB
import qualified Mescaline.Synth.Sampler.Model as Sampler
import qualified Mescaline.Pattern.Event as Event
import qualified Paths_mescaline_sts as Paths
import           Reactive.Banana hiding (filter)
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Model as RM
import           Sound.OpenSoundControl (utcr)
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Process.Monad
import           Statistics.Function (indexed)
import           Statistics.KernelDensity (Points, epanechnikovPDF, fromPoints)
import           Statistics.Sample
import           Statistics.Types (Sample)
import           System.FilePath

data SoundFile = SoundFile { fileId :: DB.SourceFileId, file :: DB.SourceFile, marked :: Bool }
                 deriving (Show, Typeable)

type UnitMap = Map DB.UnitId (DB.Unit, Map DB.DescriptorId DB.Feature)

fileColour :: ColourMap DB.SourceFileId a -> DB.SourceFileId -> Colour a
fileColour m s = m Map.! s

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

scatterPlotAxisValue :: V.Vector v a => ScatterPlotAxis -> Map DB.DescriptorId (v a) -> a
scatterPlotAxisValue a m = (m Map.! scatterPlotDescriptorId a) V.! scatterPlotIndex a

data AxisType = XAxis | YAxis
                deriving (Eq, Show, Typeable)

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

getFeature :: DB.DescriptorId -> Map DB.DescriptorId DB.Feature -> DB.Feature
getFeature = flip (Map.!)

meanPlot :: AxisType -> ScatterPlotAxis -> FeatureStatMap -> Plot Double Double
meanPlot at a stats =
    let label = "mean"
        ls = line_color ^= opaque black $ defaultPlotLineStyle
        m = featureMean (scatterPlotAxisValue a stats)
        f = case at of
                XAxis -> vlinePlot
                YAxis -> hlinePlot
    in f label ls m

layoutScatterPlot :: ColourMap DB.SourceFileId Double -> PlotData -> ScatterPlot -> Layout1 Double Double
layoutScatterPlot colourMap plotData sp = layout
    where
        xAxis = scatterPlotX sp
        yAxis = scatterPlotY sp

        points sf =
            let colour = fileColour colourMap sf
                us = unitMap plotData Map.! sf
                value axis = flip (V.!) (scatterPlotIndex axis)
                           . DB.featureValue
                           . getFeature (scatterPlotDescriptorId axis)
                vs = map (\(_, fs) -> (value xAxis fs, value yAxis fs)) (Map.elems us)
            in plot_points_style ^= filledCircles 2 (opaque colour)
             $ plot_points_values ^= vs
             -- $ plot_points_title ^= "Test data"
             $ defaultPlotPoints

        bottomAxis = -- laxis_title ^= scatterPlotAxisTitle xAxis $
                     laxis_generate ^= axisFn plotData xAxis $
                     defaultLayoutAxis
        leftAxis = -- laxis_title ^= scatterPlotAxisTitle yAxis $
                   -- laxis_generate ^= scaledAxis yAxis $
                   laxis_generate ^= axisFn plotData yAxis $
                   defaultLayoutAxis

        layout = -- layout1_title ^= "Units"
                 layout1_plots ^= map (Left . toPlot . points) (Set.toList (markedSoundFiles plotData))
                                    ++ [ Left $ meanPlot XAxis xAxis (featureStats plotData)
                                       , Left $ meanPlot YAxis yAxis (featureStats plotData) ]
               $ layout1_left_axis ^= leftAxis
               $ layout1_bottom_axis ^= bottomAxis
               $ defaultLayout1

layoutKDE :: ColourMap DB.SourceFileId Double
          -> Bool
          -> ScatterPlotAxis
          -> DB.SourceFileId
          -> PlotData
          -> Layout1 Double Double
layoutKDE colourMap flipped axis sf plotData = layout
    where
        descr = scatterPlotAxisTitle axis

        layout = -- layout1_title ^= "Densities for \"" ++ descr ++ "\""
                 layout1_plots ^= [ Left $ mkPlot featureLineStyle (featureStats plotData)
                                  , Left $ mkPlot fileLineStyle (fileStats plotData Map.! sf)
                                  , Left $ meanPlot (if flipped then YAxis else XAxis) axis (featureStats plotData) ]
               $ layout1_bottom_axis ^= (if flipped then pdfAxis else featureAxis)
               $ layout1_left_axis ^= (if flipped then featureAxis else pdfAxis)
               $ defaultLayout1 :: Layout1 Double Double

        pdfAxis = laxis_title ^= "estimate of probability density"
                $ defaultLayoutAxis

        featureAxis = laxis_title ^= descr
                    $ laxis_generate ^= axisFn plotData axis
                    $ defaultLayoutAxis

        featureLineStyle = line_color ^= opaque black $ defaultPlotLineStyle

        fileLineStyle = line_color ^= opaque (fileColour colourMap sf) $ defaultPlotLineStyle

        flipValues xs = if flipped then map (\(a, b) -> (b, a)) xs else xs

        mkPlot lineStyle stats = pdfPlot
             where
                 (points, pdf) = featureHist (scatterPlotAxisValue axis stats)
                 xValues = V.toList (fromPoints points)
                 yValues = V.toList (V.map (/ V.sum pdf) pdf)
                 pdfPlot = toPlot $ plot_lines_values ^= map flipValues [ zip xValues yValues ]
                            $ plot_lines_style ^= lineStyle
                            $ defaultPlotLines

data PickType =
    ScatterPlotPick
  | XHistogramPick
  | YHistogramPick
  deriving (Eq, Show, Typeable)

data Pick = Pick {
    pickType :: PickType
  , pickData :: (Layout1Pick Double Double)
  } deriving (Show, Typeable)

renderChart :: Layout1 Double Double
            -> Layout1 Double Double
            -> Layout1 Double Double
            -> Renderable Pick
renderChart scatterPlot xHist yHist = mapMaybePickFn p r
    where
        r = renderLayout1Matrix scatterPlot [ [ Just yHist, Just scatterPlot ]
                                            , [ Nothing,    Just xHist       ] ]
        p ((0, 1), l) = Just (Pick ScatterPlotPick l)
        p ((1, 1), l) = Just (Pick XHistogramPick l)
        p ((0, 0), l) = Just (Pick YHistogramPick l)
        p _          = Nothing

type Histogram = (Points, U.Vector Double)

data FeatureStatistics = FeatureStatistics {
    featureMin  :: Double
  , featureMax  :: Double
  , featureMean :: Double
  , featureVar  :: Double
  , featureHist :: Histogram
  } deriving (Show)

type FeatureStatMap = Map DB.DescriptorId (B.Vector FeatureStatistics)

data PlotData = PlotData {
    soundFiles       :: [SoundFile]
  , markedSoundFiles :: Set.Set DB.SourceFileId
  , unitMap          :: Map DB.SourceFileId UnitMap
  , fileStats        :: Map DB.SourceFileId FeatureStatMap
  , featureStats     :: FeatureStatMap
  } deriving (Show)

axisFn :: PlotData -> ScatterPlotAxis -> AxisFn Double
axisFn pd a = const $ autoAxis [fMin - fPad, fMax + fPad]
    where
        s = scatterPlotAxisValue a (featureStats pd)
        fMin = featureMin s
        fMax = featureMax s
        fPad = (fMax - fMin) * 5e-3

mkFeatureStats :: DB.DescriptorMap -> UnitMap -> Map DB.DescriptorId (B.Vector FeatureStatistics)
mkFeatureStats ds us = Map.mapWithKey (\di -> V.fromList . fmap (mkStats di) . indices) ds
    where
        indices d = [0..DB.descriptorDegree d - 1]
        mkStats di i = stats . V.fromList . map (flip (V.!) i . DB.featureValue . getFeature di . snd) . Map.elems $ us
        estimatePDF :: U.Vector Double -> Histogram
        estimatePDF = epanechnikovPDF 100
        stats :: U.Vector Double -> FeatureStatistics
        stats v = let (μ, σ) = meanVariance v
                  in FeatureStatistics {
                        featureMin  = V.minimum v
                      , featureMax  = V.maximum v
                      , featureMean = μ
                      , featureVar  = σ
                      , featureHist = estimatePDF v
                      }

mkPlotData :: DB.DescriptorMap -> DB.UnitMap -> [SoundFile] -> PlotData
mkPlotData ds us sfs = PlotData sfs ids um fileStats featStats
    where
        mkFeatureMap = Map.fromList . map (\f -> (DB.featureDescriptor f, f))
        um = Map.foldrWithKey (\ui (u, fs) ->
                                Map.alter (Just . maybe (Map.singleton ui (u, mkFeatureMap fs))
                                                        (Map.insert ui (u, mkFeatureMap fs)))
                                          (DB.unitSourceFile u))
                              Map.empty us
        ids = Set.fromList (map fileId (filter marked sfs))
        featStats = mkFeatureStats ds (Map.fold Map.union Map.empty (Map.filterWithKey (\k _ -> Set.member k ids) um))
        fileStats = fmap (mkFeatureStats ds) um

-- ====================================================================
-- kd-Tree for unit lookup

type UnitTree = KD.Tree U.Vector DB.Unit

mkUnitTree :: ScatterPlot -> PlotData -> UnitTree
mkUnitTree sp = KD.fromList . map (uncurry point) . units
    where
        units :: PlotData -> [(DB.Unit, Map DB.DescriptorId DB.Feature)]
        units = concat . Map.elems . fmap Map.elems . unitMap
        coord fs = let fs' = fmap DB.featureValue fs
                   in V.fromList [ scatterPlotAxisValue (scatterPlotX sp) fs'
                                 , scatterPlotAxisValue (scatterPlotY sp) fs' ]
        point u fs = (coord fs, u)

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

fromEventSource :: Typeable a => EventSource a -> Prepare (Event a)
fromEventSource = fromAddHandler . addHandler

fire :: EventSource a -> a -> IO ()
fire es x = getHandler es >>= ($x)

-- | Pick function wrapper type.
newtype PickFunction a = PickFunction (PickFn a)
                         deriving (Typeable)

-- | A pick function that never returns a pick.
noPick :: PickFunction a
noPick = PickFunction (const Nothing)

pick :: PickFunction a -> (Double, Double) -> Maybe a
pick (PickFunction f) (x, y) = f (Point x y)

-- | A version opf updateCanvas that passes the pick function to a callback.
updateCanvas' :: G.DrawingArea -> (PickFunction a -> IO ()) -> Renderable a -> IO Bool
updateCanvas' canvas pickFnSrc chart = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    let sz = (fromIntegral width,fromIntegral height)
    pickf <- G.renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
    pickFnSrc (PickFunction pickf)
    return True

-- | User interface event abstraction.
data UIEvent =
    ButtonEvent {
        uiEventTime :: TimeStamp
      , uiEventCoordinates :: (Double,Double)
      , uiEventModifiers :: [Modifier]
      , uiEventButton :: Int
      }
  | MotionEvent {
        uiEventTime :: TimeStamp
      , uiEventCoordinates :: (Double,Double)
      , uiEventModifiers :: [Modifier]
      }
    deriving (Show, Typeable)

buttonEventHandler :: EventM EButton UIEvent
buttonEventHandler = ButtonEvent <$> eventTime <*> eventCoordinates <*> eventModifier <*> liftM fromEnum eventButton

motionEventHandler :: EventM EMotion UIEvent
motionEventHandler = MotionEvent <$> eventTime <*> eventCoordinates <*> eventModifier

fromSignal :: Typeable a => self -> Signal self handler -> ((a -> IO ()) -> handler) -> Prepare (Event a)
fromSignal self signal handler = fromAddHandler (void . on self signal . handler)

fromEvent :: (WidgetClass self, Typeable a) => self -> Signal self (EventM t Bool) -> EventM t a -> Prepare (Event a)
fromEvent self signal handler = fromAddHandler $ void . on self signal . \a -> handler >>= liftIO . a >> return True

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
                                                                          , cellTextBackgroundColor := colourToGdk . fileColour colourMap . fileId $ row ]
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
        set canvas [ widgetWidthRequest := 640, widgetHeightRequest := 640 ]
        widgetAddEvents canvas [Button1MotionMask]

        -- Synthesis processes
        synthInput <- newTChanIO
        forkIO $ withDefaultInternal $ do
            synth <- Sampler.new (Sampler.Options "" Nothing Nothing True)
            forever $ do
                (t, e) <- liftIO $ atomically $ readTChan synthInput
                fork $ Sampler.playUnit synth t e

        playbackInput <- newTChanIO
        let playUnit = atomically . writeTChan playbackInput
        forkIO $ forever $ do
            u <- atomically $ readTChan playbackInput
            t <- utcr
            let e = Event.defaultSynth u
            atomically $ writeTChan synthInput (t + 0.02, e)

        -- Event sources
        pickFnSrc <- newEventSource
        scatterPlotSrc <- newEventSource

        -- Set up event network
        prepareEvents $ do
            -- Soundfile list model events
            soundFiles <- fromSignal renderer4 cellToggled $ \a -> const (listStoreToList model >>= a)
            soundFiles0 <- liftIO $ listStoreToList model
            currentSoundFile <- fromSignal view cursorChanged $ \a -> do
                p <- treeViewGetCursor view
                case p of
                    ([i], _) -> listStoreGetValue model i >>= a
                    _ -> return ()
            -- Soundfile list model behaviors
            let selectedSoundFile = stepper (head soundFiles0) currentSoundFile
                selectedSoundFileId = fileId <$> selectedSoundFile
            -- Canvas events
            expose        <- fromEvent canvas exposeEvent        (return ())
            buttonPress   <- fromEvent canvas buttonPressEvent   buttonEventHandler
            buttonRelease <- fromEvent canvas buttonReleaseEvent buttonEventHandler
            buttonMove    <- fromEvent canvas motionNotifyEvent  motionEventHandler
            -- The picks from the plot area ((Double,Double) -> Maybe Pick)
            pickFunction <- liftM (fmap pick . stepper noPick) $ fromEventSource pickFnSrc
            let applyPickFunction es = ((\f e -> (e, f (uiEventCoordinates e))) <$> pickFunction) `apply` es
                -- Button press events occurring on the relevant axes
                buttonPressInAxisTitle = flip mapMaybe (applyPickFunction buttonPress)
                                            $ \(e, p) -> case p of
                                                Just (Pick XHistogramPick (L1P_BottomAxisTitle _)) -> Just (XAxis, e)
                                                Just (Pick YHistogramPick (L1P_LeftAxisTitle _))   -> Just (YAxis, e)
                                                _ -> Nothing
            scatterPlotAxis <- fromEventSource scatterPlotSrc
            let plotData = fmap (mkPlotData descriptors us) (stepper soundFiles0 soundFiles)
                scatterPlotState = accumB (ScatterPlot (head . snd . head $ scatterPlotAxes) (head  . snd . head $ scatterPlotAxes))
                                          ((\(at, a) sp ->
                                            case at of
                                                XAxis -> sp { scatterPlotX = a }
                                                YAxis -> sp { scatterPlotY = a }) <$> scatterPlotAxis)
                -- Display
                scatterPlot = layoutScatterPlot colourMap <$> plotData <*> scatterPlotState
                kdePlotX = (layoutKDE colourMap False . scatterPlotX) <$> scatterPlotState <*> selectedSoundFileId <*> plotData
                kdePlotY = (layoutKDE colourMap True  . scatterPlotY) <$> scatterPlotState <*> selectedSoundFileId <*> plotData
                chart = renderChart <$> scatterPlot <*> kdePlotX <*> kdePlotY
                redraw = ignore soundFiles `union` ignore currentSoundFile `union` ignore scatterPlotAxis
                -- Synthesis
                unitTree = mkUnitTree <$> scatterPlotState <*> plotData
                scatterPlotCoord e = flip mapMaybe (applyPickFunction e) $ \(_, p) ->
                                        case p of
                                            Just (Pick ScatterPlotPick (L1P_PlotArea x y _)) -> Just (V.fromList [x, y])
                                            _ -> Nothing
                unit = mapMaybe (fmap $ \((_, u), _) -> u) $
                        ((\t v -> KD.closest KD.sqrEuclidianDistance v t) <$> unitTree)
                            `apply` scatterPlotCoord (R.filter ((== [Control]) . uiEventModifiers) buttonMove)
            -- Update canvas with current plot and feed back new pick function
            reactimate $ ((const . void . updateCanvas' canvas (fire pickFnSrc)) <$> chart) `apply` expose
            -- Display the axis popup menu if needed
            let popupAxisMenu a e = scatterPlotAxisMenu scatterPlotAxes (fire scatterPlotSrc . ((,) a)) >>=
                                        flip menuPopup (Just (toEnum (uiEventButton e), uiEventTime e))
            reactimate $ uncurry popupAxisMenu <$> (R.filter ((==3) . uiEventButton . snd) buttonPressInAxisTitle)
            -- Schedule redraw
            reactimate $ widgetQueueDraw canvas <$ redraw
            -- Print the picks when a button is pressed in the plot area
            -- reactimate $ print <$> (applyPickFunction (buttonPress `union` buttonMove))
            reactimate $ print <$> unit
            -- (R.filter (\(e, p) -> (uiEventModifiers p == [Control]) . uiEventModifiers)) 

        widgetShowAll win
        mainGUI

-- type SynthProcess s = Time -> s -> (Synth, (Time, s))

main :: IO ()
main = App.runAppT appMain =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles

-- Reactive combinators
mapMaybe :: FRP f => (a -> Maybe b) -> RM.Event f a -> RM.Event f b
mapMaybe f = fmap fromJust . R.filter isJust . fmap f

ignore :: Functor f => f a -> f ()
ignore = (<$) ()

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF f = (fmap fst f, fmap snd f)

zipA :: Applicative f => f a -> f b -> f (a, b)
zipA a b = (,) <$> a <*> b
