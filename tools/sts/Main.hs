{-# LANGUAGE BangPatterns
           , DeriveDataTypeable
           , FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Accessor
import           Data.Char ( toLower )
import           Data.Colour
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Colour.Names (white)
import qualified Data.Foldable as Fold
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
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as M
import           Graphics.Rendering.Chart as C
import           Graphics.Rendering.Chart.Grid as C
import           Graphics.UI.Gtk as G
import           Graphics.UI.Gtk.Glade
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.ColourMap
import qualified Mescaline.Database as DB
import qualified Mescaline.Synth.Sampler.Model as Sampler
import qualified Mescaline.Synth.Sampler.Params as Sampler
import qualified Paths_mescaline_sts as Paths
import           Reactive.Banana hiding (filter)
import qualified Reactive.Banana as R
import           Reactive.Banana.EventSource
import qualified Reactive.Banana.Model as RM
import           Sound.OpenSoundControl (pauseThread, utcr)
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Process.Monad
import           Statistics.KernelDensity (Points, epanechnikovPDF, fromPoints)
import           Statistics.Sample
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

data ScatterPlotLayer = ScatterPlotForeground | ScatterPlotBackground deriving (Show)

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

layoutScatterPlot :: ScatterPlotLayer -> ColourMap DB.SourceFileId Double -> PlotData -> ScatterPlot -> Layout1 Double Double
layoutScatterPlot layer colourMap plotData sp = layout
    where
        xAxis = scatterPlotX sp
        yAxis = scatterPlotY sp

        points sf =
            let colour = fileColour colourMap sf
                us = unitMap plotData Map.! sf
                value axis = flip (V.!) (scatterPlotIndex axis)
                           . DB.featureValue
                           . getFeature (scatterPlotDescriptorId axis)
                (xs, as) = List.foldl' (\(!vs, !as) (ui, (_, fs)) ->
                                        let p = (value xAxis fs, value yAxis fs)
                                        in (p:vs, if Set.member ui (activeUnits plotData) then p:as else as))
                                       ([], [])
                                       (Map.assocs us)
            in case layer of
                ScatterPlotForeground ->
                    [ -- Highlight active units
                      plot_points_style ^= hollowCircles 4 2 (opaque black)
                    $ plot_points_values ^= as
                    -- $ plot_points_title ^= "Test data"
                    $ defaultPlotPoints ]
                ScatterPlotBackground ->
                    [ -- Plot units
                      plot_points_style ^= filledCircles 2 (opaque colour)
                    $ plot_points_values ^= xs
                    -- $ plot_points_title ^= "Test data"
                    $ defaultPlotPoints ]

        bottomAxis = -- laxis_title ^= scatterPlotAxisTitle xAxis $
                     laxis_generate ^= axisFn plotData xAxis $
                     defaultLayoutAxis
        leftAxis = -- laxis_title ^= scatterPlotAxisTitle yAxis $
                   -- laxis_generate ^= scaledAxis yAxis $
                   laxis_generate ^= axisFn plotData yAxis $
                   defaultLayoutAxis

        background = case layer of
                        ScatterPlotForeground -> transparent
                        ScatterPlotBackground -> opaque white

        layout = -- layout1_title ^= "Units"
                 layout1_plots ^= concatMap (map (Left . toPlot) . points) (Set.toList (markedSoundFiles plotData))
                                    ++ [ Left $ meanPlot XAxis xAxis (featureStats plotData)
                                       , Left $ meanPlot YAxis yAxis (featureStats plotData) ]
               $ layout1_left_axis ^= leftAxis
               $ layout1_bottom_axis ^= bottomAxis
               $ layout1_background ^= solidFillStyle background
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

        plots = [ Left $ mkPlot featureLineStyle (featureStats plotData)
                , Left $ mkPlot fileLineStyle (fileStats plotData Map.! sf)
                , Left $ meanPlot (if flipped then YAxis else XAxis) axis (featureStats plotData) ]

        layout = -- layout1_title ^= "Densities for \"" ++ descr ++ "\""
                 layout1_plots ^= plots
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

mkChart :: Layout1 Double Double
        -> Layout1 Double Double
        -> Layout1 Double Double
        -> Renderable Pick
mkChart scatterPlot xHist yHist = mapMaybePickFn p r
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
  , activeUnits      :: Set.Set DB.UnitId
  , fileStats        :: Map DB.SourceFileId FeatureStatMap
  , featureStats     :: FeatureStatMap
  } deriving (Show)

getSoundFile :: DB.SourceFileId -> PlotData -> SoundFile
getSoundFile sf = fromJust . List.find ((== sf) . fileId) . soundFiles

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
mkPlotData ds us sfs = PlotData sfs ids um Set.empty fileStats featStats
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

setActiveUnits :: Set.Set DB.UnitId -> PlotData -> PlotData
setActiveUnits s p = p { activeUnits = s }

-- ====================================================================
-- kd-Tree for unit lookup

type UnitTree = KD.Tree U.Vector (DB.UnitId, (DB.SourceFile, DB.Unit))

mkUnitTree :: ScatterPlot -> PlotData -> UnitTree
mkUnitTree sp pd = KD.fromList $ map point units
    where
        units :: [(DB.SourceFile, (DB.UnitId, (DB.Unit, Map DB.DescriptorId DB.Feature)))]
        units = concat
              $ fmap (\sf -> let f = file (getSoundFile sf pd) in map ((,)f) (Map.assocs (unitMap pd Map.! sf)))
              $ Set.toList (markedSoundFiles pd)
        -- units :: PlotData -> [((DB.SourceFile, DB.Unit), Map DB.DescriptorId DB.Feature)]
        -- units pd = concat . Map.elems . fmap Map.elems . unitMap . 
        coord fs = let fs' = fmap DB.featureValue fs
                   in V.fromList [ scatterPlotAxisValue (scatterPlotX sp) fs'
                                 , scatterPlotAxisValue (scatterPlotY sp) fs' ]
        point (sf, (ui, (u, fs))) = (coord fs, (ui, (sf, u)))

-- | Pick function wrapper type.
newtype PickFunction a = PickFunction (PickFn a)
                         deriving (Typeable)

-- | A pick function that never returns a pick.
noPick :: PickFunction a
noPick = PickFunction (const Nothing)

pick :: PickFunction a -> (Double, Double) -> Maybe a
pick (PickFunction f) (x, y) = f (Point x y)

-- | A version opf updateCanvas that passes the pick function to a callback.
updateCanvas' :: G.DrawingArea -> (PickFunction a -> IO ()) -> C.Render (PickFunction a) -> IO Bool
updateCanvas' canvas pickFnSrc render = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    G.renderWithDrawable win render >>= pickFnSrc
    return True

renderChart :: Renderable a -> (Int, Int) -> C.Render (PickFunction a)
renderChart r (w,h) = liftM PickFunction $ runCRender (render r (fromIntegral w, fromIntegral h)) bitmapEnv 

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

hasModifiers :: [Modifier] -> UIEvent -> Bool
hasModifiers ms = (== ms) . uiEventModifiers

hasButton :: Int -> UIEvent -> Bool
hasButton b = (== b) . uiEventButton

satisfies :: [(a -> Bool)] -> a -> Bool
satisfies ps a = all ($a) ps

fromSignal :: Typeable a => self -> Signal self handler -> ((a -> IO ()) -> handler) -> Prepare (Event a)
fromSignal self signal handler = fromAddHandler (void . on self signal . handler)

fromEvent :: (WidgetClass self, Typeable a) => self -> Signal self (EventM t Bool) -> EventM t (a, Bool) -> Prepare (Event a)
fromEvent self signal handler = fromAddHandler $ void . on self signal . \action -> do { (a, b) <- handler ; liftIO (action a) ; return b }

fromEvent_ :: (WidgetClass self, Typeable a) => self -> Signal self (EventM t Bool) -> EventM t a -> Bool -> Prepare (Event a)
fromEvent_ self signal handler override = fromEvent self signal (fmap (flip (,) override) handler)

buttonEventHandler :: EventM EButton UIEvent
buttonEventHandler = ButtonEvent <$> eventTime <*> eventCoordinates <*> eventModifier <*> liftM fromEnum eventButton

buttonPress :: (WidgetClass self) => self -> Bool -> Prepare (Event UIEvent)
buttonPress self = fromEvent_ self buttonPressEvent buttonEventHandler

buttonRelease :: (WidgetClass self) => self -> Bool -> Prepare (Event UIEvent)
buttonRelease self = fromEvent_ self buttonReleaseEvent buttonEventHandler

motionEventHandler :: EventM EMotion UIEvent
motionEventHandler = MotionEvent <$> eventTime <*> eventCoordinates <*> eventModifier

motion :: (WidgetClass self) => self -> Bool -> Prepare (Event UIEvent)
motion self = fromEvent_ self motionNotifyEvent motionEventHandler

resizeEvent :: (WidgetClass self) => self -> Bool -> Prepare (Event (Int, Int))
resizeEvent self = fromEvent_ self configureEvent eventSize

-- buttonMotion :: (WidgetClass self) => Int -> [Modifier] -> self -> Prepare (Behavior (Maybe (Double, Double)))
-- buttonMotion b ms self = do
--     p <- liftM (R.filter f) $ buttonPress self
--     m <-                      motion self
--     r <- liftM (R.filter f) $ buttonRelease self
--     stepper Nothing ((Just . uiEventCoordinates) <$> (p `union` m) `union` Nothing <$ r)
--     where f = satisfies [hasButton b, hasModifiers ms]

newtype UnitIdT = UnitIdT { unUnitIdT :: DB.UnitId } deriving (Show, Typeable)

data BackgroundSurface = BackgroundSurface {
    backgroundSurfaceSize :: (Int,Int)
  , backgroundSurface :: C.Surface
  } deriving (Typeable)

mkBackgroundSurface :: MonadIO m => Int -> Int -> m BackgroundSurface
mkBackgroundSurface w h = liftIO (liftM (BackgroundSurface (w,h)) (C.createImageSurface C.FormatRGB24 w h))

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

        (col4, renderer4) <- newColumn model "Active" cellRendererToggleNew $ \row -> [ cellToggleActive := marked row ]
        treeViewAppendColumn view col4

        (col2, _) <- newColumn model "Channels" cellRendererTextNew $ \row -> [ cellText := show . DB.sourceFileNumChannels . file $ row ]
        treeViewAppendColumn view col2

        (col3, _) <- newColumn model "Sample rate" cellRendererTextNew $ \row -> [ cellText := show . DB.sourceFileSampleRate . file $ row ]
        treeViewAppendColumn view col3

        (col5, _) <- newColumn model "Duration" cellRendererTextNew $ \row -> [ cellText := show . DB.sourceFileDuration . file $ row ]
        treeViewAppendColumn view col5

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
                (t, e, a1, a2) <- liftIO $ atomically $ readTChan synthInput
                fork $ a1 >> Sampler.playUnit synth t e >> a2

        playbackControl <- newTChanIO
        playbackData <- newTVarIO Nothing
        let setUnit x = atomically $ writeTVar playbackData x
            setPlayback b = atomically $ writeTChan playbackControl b
        forkIO $ forever $ do
            b <- atomically $ readTChan playbackControl
            when b $ do
                fix $ \loop -> do
                    e <- atomically ((Left <$> readTChan playbackControl) `orElse` (Right <$> readTVar playbackData))
                    case e of
                        Left b -> if b then loop else return ()
                        Right (Just ((sf, u), (whenStarting, whenFinished))) -> do
                            t <- utcr
                            let e = Sampler.defaultParams sf u
                            atomically $ writeTChan synthInput (t + 0.2, e, liftIO whenStarting, liftIO whenFinished)
                            pauseThread $ DB.unitDuration u
                            loop
                        _ -> loop

        -- Event sources
        pickFnSrc <- newEventSource
        scatterPlotSrc <- newEventSource
        unitPlayingSrc <- newEventSource
        bgSurfaceSrc <- newEventSource

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
            canvasResized <- resizeEvent canvas False
            bgSurface     <- liftM2 stepper 
                                    (mkBackgroundSurface 0 0)
                                    (fromEventSource bgSurfaceSrc)
            expose        <- fromEvent_ canvas exposeEvent        (return ())        False
            buttonPress   <- fromEvent_ canvas buttonPressEvent   buttonEventHandler False
            buttonRelease <- fromEvent_ canvas buttonReleaseEvent buttonEventHandler False
            buttonMove    <- fromEvent_ canvas motionNotifyEvent  motionEventHandler False
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
            unitPlaying <- fromEventSource unitPlayingSrc
            let plotData = setActiveUnits <$> activeUnits <*> (mkPlotData descriptors us <$> stepper soundFiles0 soundFiles)
                scatterPlotState = accumB (ScatterPlot (head . snd . head $ scatterPlotAxes) (head  . snd . head $ scatterPlotAxes))
                                          ((\(at, a) sp ->
                                            case at of
                                                XAxis -> sp { scatterPlotX = a }
                                                YAxis -> sp { scatterPlotY = a }) <$> scatterPlotAxis)
                -- Display
                scatterPlotFG = layoutScatterPlot ScatterPlotForeground colourMap <$> plotData <*> scatterPlotState
                scatterPlotBG = layoutScatterPlot ScatterPlotBackground colourMap <$> plotData <*> scatterPlotState
                kdePlotX = (layoutKDE colourMap False . scatterPlotX) <$> scatterPlotState <*> selectedSoundFileId <*> plotData
                kdePlotY = (layoutKDE colourMap True  . scatterPlotY) <$> scatterPlotState <*> selectedSoundFileId <*> plotData
                chartFG = mkChart <$> scatterPlotFG <*> kdePlotX <*> kdePlotY
                chartBG = mkChart <$> scatterPlotBG <*> kdePlotX <*> kdePlotY
                fgRender = renderChart <$> chartFG <*> (backgroundSurfaceSize <$> bgSurface)
                render = (\fg bg -> do
                            C.save
                            C.setSourceSurface (backgroundSurface bg) 0 0
                            C.paint
                            C.restore
                            fg)
                            <$> fgRender
                            <*> bgSurface
                redraw = ignore soundFiles `union` ignore currentSoundFile `union` ignore scatterPlotAxis `union` ignore unitPlaying
                -- Synthesis
                unitTree = mkUnitTree <$> scatterPlotState <*> plotData
                filterPlayback = R.filter (satisfies [hasButton 1])
                playbackStart = filterPlayback buttonPress
                playbackPos = uiEventCoordinates <$> playbackStart `union` buttonMove
                playback = (True <$ playbackStart) `union` (False <$ filterPlayback buttonRelease)
                scatterPlotCoord = flip mapMaybe (pickFunction `apply` playbackPos) $ \p ->
                                    case p of
                                        Just (Pick ScatterPlotPick (L1P_PlotArea x y _)) -> Just (V.fromList [x, y])
                                        _ -> Nothing
                closest = filterMaybe $ ((\t v -> KD.closest KD.sqrEuclidianDistance v t) <$> unitTree)
                                            `apply` scatterPlotCoord
                unit = (\((_, u), _) -> u) <$> closest
                activeUnits = accumB Set.empty (either (Set.insert . unUnitIdT) (Set.delete . unUnitIdT) <$> unitPlaying)
            -- Update canvas with current plot and feed back new pick function
            reactimate $ ((\chart (w,h) -> do
                            bg <- mkBackgroundSurface w h
                            pf <- C.renderWith (backgroundSurface bg) (renderChart chart (backgroundSurfaceSize bg))
                            fire bgSurfaceSrc bg
                            fire pickFnSrc pf)
                         <$> chartBG) `apply` canvasResized
            reactimate $ ((const . void . updateCanvas' canvas ({- fire pickFnSrc -} const (return ()))) <$> render) `apply` expose
            -- Display the axis popup menu if needed
            let popupAxisMenu a e = scatterPlotAxisMenu scatterPlotAxes (fire scatterPlotSrc . ((,) a)) >>=
                                        flip menuPopup (Just (toEnum (uiEventButton e), uiEventTime e))
            reactimate $ uncurry popupAxisMenu <$> (R.filter ((==3) . uiEventButton . snd) buttonPressInAxisTitle)
            -- Schedule redraw
            reactimate $ widgetQueueDraw canvas <$ redraw
            -- Print the picks when a button is pressed in the plot area
            -- reactimate $ print <$> (applyPickFunction (buttonPress `union` buttonMove))
            -- reactimate $ print <$> activeUnits
            reactimate $ (\(ui, u) -> setUnit (Just (u, (fire unitPlayingSrc (Left (UnitIdT ui)), fire unitPlayingSrc (Right (UnitIdT ui)))))) <$> unit
            reactimate $ setPlayback <$> playback
            reactimate $ (const . print <$> activeUnits) `apply` unitPlaying
            -- (R.filter (\(e, p) -> (uiEventModifiers p == [Control]) . uiEventModifiers)) 

        widgetShowAll win
        mainGUI

-- type SynthProcess s = Time -> s -> (Synth, (Time, s))

main :: IO ()
main = App.runAppT appMain =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles

-- Reactive combinators
merge :: FRP f => RM.Event f a -> RM.Event f b -> RM.Event f (Either a b)
merge a b = union (Left <$> a) (Right <$> b)

mapMaybe :: FRP f => (a -> Maybe b) -> RM.Event f a -> RM.Event f b
mapMaybe f = fmap fromJust . R.filter isJust . fmap f

filterMaybe :: FRP f => RM.Event f (Maybe a) -> RM.Event f a
filterMaybe = mapMaybe id

ignore :: Functor f => f a -> f ()
ignore = (<$) ()

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF f = (fmap fst f, fmap snd f)

zipA :: Applicative f => f a -> f b -> f (a, b)
zipA a b = (,) <$> a <*> b
