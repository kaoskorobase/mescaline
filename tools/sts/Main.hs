module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Accessor
import           Data.Char ( toLower )
import           Data.Colour
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Colour.Names
import           Data.Word (Word16)
import           Data.List ( isPrefixOf )
import qualified Data.Map as Map
import qualified Data.Vector.Generic as V
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Gtk
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Glade
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.ColourMap
import qualified Mescaline.Database as DB
import qualified Paths_mescaline_sts as Paths
import           System.FilePath
import           System.Glib.Signals (on)

newTextColumn f label model = do
    col <- treeViewColumnNew
    treeViewColumnSetTitle col label
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer True
    cellLayoutSetAttributes col renderer model $ \row -> [ cellText := f row ]
    return col

data SoundFile = SoundFile { fileId :: DB.SourceFileId, file :: DB.SourceFile, marked :: Bool }

fileColour :: ColourMap DB.SourceFileId a -> SoundFile -> Colour a
fileColour m s = m Map.! fileId s

colourToGdk :: (Floating a, Fractional a, RealFrac a) => Colour a -> Color
colourToGdk c = Color (r2w (channelRed rgb)) (r2w (channelGreen rgb)) (r2w (channelBlue rgb))
    where
        rgb = toRGBUsingSpace sRGBSpace c
        r2w r = truncate (r * fromIntegral (maxBound :: Word16))

chart sfMap units = toRenderable layout
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
        treeViewColumnSetTitle col3 "Active"

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

        -- update the model when the toggle buttons are activated
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

        canvas <- xmlGetWidget xml castToDrawingArea "plot"
        canvas `on` exposeEvent $ liftIO $ updateCanvas (chart cmap us) canvas

        widgetShowAll win
        mainGUI

main :: IO ()
main = App.runAppT appMain =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles
