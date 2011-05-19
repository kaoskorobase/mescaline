{-# LANGUAGE DeriveDataTypeable #-}
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
import           Data.Word (Word16)
import           Data.List ( isPrefixOf )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Typeable
import qualified Data.Vector.Generic as V
import           Graphics.Rendering.Chart as C
import           Graphics.Rendering.Chart.Gtk
import           Graphics.UI.Gtk as G
import           Graphics.UI.Gtk.Glade
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.ColourMap
import qualified Mescaline.Database as DB
import qualified Paths_mescaline_sts as Paths
import           Reactive.Banana hiding (filter)
import           System.FilePath

newTextColumn f label model = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col label
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model $ \row -> [ cellText := f row ]
    return col

data SoundFile = SoundFile { fileId :: DB.SourceFileId, file :: DB.SourceFile, marked :: Bool }
                 deriving (Typeable)

fileColour :: ColourMap DB.SourceFileId a -> SoundFile -> Colour a
fileColour m s = m Map.! fileId s

colourToGdk :: (Floating a, Fractional a, RealFrac a) => Colour a -> Color
colourToGdk c = Color (r2w (channelRed rgb)) (r2w (channelGreen rgb)) (r2w (channelBlue rgb))
    where
        rgb = toRGBUsingSpace sRGBSpace c
        r2w r = truncate (r * fromIntegral (maxBound :: Word16))

chart sfMap units = layout1ToRenderable layout
    where
        -- bars = plot_errbars_values ^= [symErrPoint x y dx dy | (x,y,dx,dy) <- vals]
        --      $ plot_errbars_title ^="test"
        --      $ defaultPlotErrBars

        points sf colour =
            let us = Map.filter (\(u, _) -> sf == DB.unitSourceFile u) units
                vs = map (\(_, f) -> DB.featureValue (head f)) (Map.elems us)
            in plot_points_style ^= filledCircles 2 (opaque colour)
               $ plot_points_values ^= [ (v V.! 0, v V.! 1) | v <- vs ]
               $ plot_points_title ^= "Test data"
               $ defaultPlotPoints

        layout = layout1_title ^= "Units"
               $ layout1_plots ^= map (\(sf, c) -> Left (toPlot (points sf c))) (Map.toList sfMap)
               $ defaultLayout1

{-----------------------------------------------------------------------------
Event sources
------------------------------------------------------------------------------}
-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you
data EventSource a = EventSource {
        setHandler :: (a -> IO ()) -> IO (),
        getHandler :: IO (a -> IO ())
        }

newEventSource :: IO (EventSource a)
newEventSource = do
    ref <- newIORef (const $ return ())
    return $
        EventSource { setHandler = writeIORef ref, getHandler = readIORef ref}

addHandler :: EventSource a -> AddHandler a
addHandler es k = do
    handler <- getHandler es
    setHandler es (\x -> handler x >> k x)

fire :: EventSource a -> (a -> IO ())
fire es x = getHandler es >>= ($ x)

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

updateCanvas' :: G.DrawingArea -> (PickFunction a -> IO ()) -> Renderable a -> IO Bool
updateCanvas' canvas pickFnSrc chart = do
    win <- G.widgetGetDrawWindow canvas
    (width, height) <- G.widgetGetSize canvas
    let sz = (fromIntegral width,fromIntegral height)
    pickf <- G.renderWithDrawable win $ runCRender (render chart sz) bitmapEnv
    pickFnSrc (Pick pickf)
    return True

newtype L1P x y = L1P { unL1P :: Layout1Pick x y } deriving (Show, Typeable)

appMain :: AppT IO ()
appMain = do
    [gladeFile, dbFile] <- App.getArgs
    (sfs, us) <- DB.withDatabase dbFile $ DB.query ".*" ["es.globero.mescaline.spectral"]
    let cmap = hsv (Map.keys sfs)

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
        col1 <- treeViewColumnNew
        col2 <- treeViewColumnNew
        col3 <- treeViewColumnNew
        col4 <- treeViewColumnNew

        treeViewColumnSetTitle col1 "File"
        treeViewColumnSetResizable col1 True
        treeViewColumnSetTitle col2 "Channels"
        treeViewColumnSetTitle col3 "Sample rate"
        treeViewColumnSetTitle col4 "Active"

        renderer1 <- cellRendererTextNew
        renderer2 <- cellRendererTextNew
        renderer3 <- cellRendererTextNew
        renderer4 <- cellRendererToggleNew

        cellLayoutPackStart col1 renderer1 True
        cellLayoutPackStart col2 renderer2 True
        cellLayoutPackStart col3 renderer3 True
        cellLayoutPackStart col4 renderer4 True

        cellLayoutSetAttributes col1 renderer1 model $ \row -> [ cellText := takeFileName . DB.sourceFileUrl . file $ row
                                                               , cellTextBackgroundColor := colourToGdk . fileColour cmap $ row ]
        cellLayoutSetAttributes col2 renderer2 model $ \row -> [ cellText := show . DB.sourceFileNumChannels . file $ row ]
        cellLayoutSetAttributes col3 renderer3 model $ \row -> [ cellText := show . DB.sourceFileSampleRate . file $ row ]
        cellLayoutSetAttributes col4 renderer4 model $ \row -> [ cellToggleActive := marked row ]

        treeViewAppendColumn view col1
        treeViewAppendColumn view col2
        treeViewAppendColumn view col3
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
            return (map toLower str `isPrefixOf` map toLower (DB.sourceFileUrl . file $ row))

        -- Drawing area for plots
        canvas <- xmlGetWidget xml castToDrawingArea "plot"
        widgetAddEvents canvas [Button1MotionMask]
        pickFnSrc <- newEventSource

        -- Set up event network
        prepareEvents $ do
            -- Event fired as sound files are marked or unmarked
            soundFiles <- fromAddHandler $ \a -> void $ on renderer4 cellToggled $ const (listStoreToList model >>= a)
            -- Expose event for canvas
            expose <- fromAddHandler $ \a -> void $ on canvas exposeEvent $ liftIO (a ()) >> return True
            -- Canvas button events
            buttonPress <- fromAddHandler $ \a -> void $ on canvas buttonPressEvent $ eventCoordinates >>= liftIO . a >> return True
            buttonMove <- fromAddHandler $ \a -> void $ on canvas motionNotifyEvent $ eventCoordinates >>= liftIO . a >> return True
            -- The picks from the plot area ((Double,Double) -> Maybe Layout1Pick)
            picks <- liftM (fmap ((uncurry Point >>>) . pick) . stepper NoPick) $ fromAddHandler $ addHandler pickFnSrc
            let plots = stepper (chart cmap us) $ flip fmap soundFiles $ \sfs ->
                            -- Filter units by IDs of marked sound files
                            let ids = Set.fromList (map fileId (filter marked sfs))
                            in chart cmap (Map.filter (flip Set.member ids . DB.unitSourceFile . fst) us)
            -- Schedule redraw when sound files change
            reactimate $ widgetQueueDraw canvas <$ soundFiles
            -- Update canvas with current plot and feed back new pick function
            reactimate (((const . void . updateCanvas' canvas (fire pickFnSrc . fmap L1P)) <$> plots) `apply` expose)
            -- Print the picks when a button is pressed in the plot area
            reactimate $ print <$> (picks `apply` (buttonPress `union` buttonMove))

        widgetShowAll win
        mainGUI

main :: IO ()
main = App.runAppT appMain =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles
