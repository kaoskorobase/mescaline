{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad (unless)
import           Data.Accessor
import           Data.Char (ord)
import           Data.Function (fix)
import           Data.Maybe
import           Database.HDBC (quickQuery')
import qualified Mescaline.Application as App
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Concat as Synth
import qualified Mescaline.Synth.Pattern as P
import           Mescaline.Synth.Sequencer as Sequencer
import           Mescaline.Synth.SequencerView
import qualified Mescaline.Synth.FeatureSpace as FeatureSpace
import qualified Mescaline.Synth.FeatureSpaceView as FeatureSpaceView
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Server
import qualified Sound.SC3.Server.Process.CommandLine as Server
import qualified System.Environment as Env
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import qualified System.Random as Random

import qualified Qtc.Classes.Gui                as Qt
import qualified Qtc.Classes.Gui_h              as Qt
import qualified Qtc.Classes.Object             as Qt
import qualified Qtc.Classes.Qccs               as Qt
import qualified Qtc.Classes.Qccs_h             as Qt
import qualified Qtc.ClassTypes.Gui             as Qt
import qualified Qtc.ClassTypes.Tools           as Qt
import qualified Qtc.Core.Base                  as Qt
import qualified Qtc.Core.QFile                 as Qt
import qualified Qtc.Enums.Base                 as Qt
import qualified Qtc.Enums.Classes.Core         as Qt
import qualified Qtc.Enums.Core.QIODevice       as Qt
import qualified Qtc.Enums.Core.Qt              as Qt
import qualified Qtc.Enums.Gui.QGraphicsView    as Qt
import qualified Qtc.Enums.Gui.QPainter         as Qt
import qualified Qtc.Gui.Base                   as Qt
import qualified Qtc.Gui.QAction                as Qt
import qualified Qtc.Gui.QApplication           as Qt
import qualified Qtc.Gui.QGraphicsView          as Qt
import qualified Qtc.Gui.QKeySequence           as Qt
import qualified Qtc.Gui.QMainWindow            as Qt
import qualified Qtc.Gui.QMenu                  as Qt
import qualified Qtc.Gui.QMenuBar               as Qt
import qualified Qtc.Gui.QMessageBox            as Qt
import qualified Qtc.Gui.QWidget                as Qt
import qualified Qtc.Gui.QWidget_h              as Qt
import qualified Qtc.Tools.QUiLoader            as Qt
import qualified Qtc.Tools.QUiLoader_h          as Qt

numRegions :: Int
numRegions = 4

sequencer0 :: Sequencer ()
sequencer0 = Sequencer.cons 16 16 0.125 (Bar (-1))

setEnv = setVal (P.synth.>P.attackTime) 0.01 . setVal (P.synth.>P.releaseTime) 0.02

getUnits dbFile pattern features = do
    (units, sfMap) <- DB.withDatabase dbFile $ \c -> do
        Sql.unitQuery (quickQuery' c)
              ((Sql.url Sql.sourceFile `Sql.like` pattern) `Sql.and` (Sql.segmentation Sql.unit `Sql.eq` Unit.Onset))
              features
    case units of
        Left e -> fail e
        Right us -> return us

-- sceneKeyPressEvent :: MVar Bool -> Chan (Sequencer a -> Sequencer a) -> Qt.QWidget () -> Qt.QKeyEvent () -> IO ()
-- sceneKeyPressEvent mute chan _ qkev = do
--     key <- Qt.key qkev ()
--     if key == Qt.qEnum_toInt Qt.eKey_C
--         then writeChan chan Sequencer.deleteAll
--         else if key == Qt.qEnum_toInt Qt.eKey_M
--             then modifyMVar_ mute (return . not)
--             else return ()

type MainWindow = Qt.QMainWindowSc (CMainWindow)
data CMainWindow = CMainWindow

mainWindow :: Qt.QWidget () -> IO (MainWindow)
mainWindow mw = Qt.qSubClass $ Qt.qCast_QMainWindow mw

-- windowKeyPressEvent :: MainWindow -> Qt.QKeyEvent () -> IO ()
-- windowKeyPressEvent this evt = putStrLn "yeah!" >> Qt.keyPressEvent_h this evt

loadUI :: FilePath -> IO (MainWindow)
loadUI path = do
    uiLoader <- Qt.qUiLoader ()
    uiFile <- Qt.qFile path
    Qt.open uiFile Qt.fReadOnly
    w <- Qt.load uiLoader uiFile
    Qt.close uiFile ()
    mainWindow w

about :: Qt.QMainWindow () -> IO ()
about mw
  = Qt.qMessageBoxAbout (
        mw
      , "About Mescaline"
      , "<h3>About Mescaline</h3><a href=\"http://mescaline.globero.es\">Mescaline</a> is a data-driven sequencer and synthesizer.")

clearSequencer :: Chan (Sequencer a -> Sequencer a) -> Qt.QMainWindow () -> IO ()
clearSequencer chan _ = writeChan chan Sequencer.deleteAll

muteSequencer :: MVar Bool -> Qt.QMainWindow () -> IO ()
muteSequencer mute _ = modifyMVar_ mute (return . not)

scrubble :: Qt.QMainWindow () -> IO ()
scrubble _ = putStrLn "Scrubble dat shit!"

pipe :: (a -> IO b) -> Chan a -> Chan b -> IO ()
pipe f ichan ochan = do
    a <- readChan ichan
    b <- f a
    writeChan ochan b
    pipe f ichan ochan

startSynth :: IO (Chan (Either FeatureSpaceView.Output ()), Chan ())
startSynth = do
    ichan <- newChan
    ochan <- newChan

    forkIO $ do
        scsynth <- App.getResourcePath "supercollider/scsynth"
        plugins <- App.getResourcePath "supercollider/plugins"
        let
            serverOptions = Server.defaultServerOptions {
                Server.loadSynthDefs  = False
              , Server.serverProgram  = scsynth
              , Server.ugenPluginPath = Just [plugins]
              }
            rtOptions = Server.defaultRTOptions { Server.udpPortNumber = 2278 }
        putStrLn $ unwords $ Server.rtCommandLine serverOptions rtOptions
        Server.withSynth
            serverOptions
            rtOptions
            Server.defaultOutputHandler
            $ \(t :: OSC.UDP) -> do
                synth <- Synth.newSamplerWithTransport t serverOptions
                fix $ \loop -> do
                    e <- readChan ichan
                    case e of
                        Left (FeatureSpaceView.Activate (t, u)) -> do
                            -- b <- readMVar mute
                            -- unless b $ do
                            -- print u
                            Synth.playEvent synth (setEnv (P.fromUnit t u))
                            -- return ()
                            loop
                        Right _ -> writeChan ochan ()
    return (ichan, ochan)

stopSynth :: (Chan (Either FeatureSpaceView.Output ()), Chan ()) -> IO ()
stopSynth (ichan, ochan) = do
    writeChan ichan (Right ())
    readChan ochan

main :: IO ()
main = do
    app <- Qt.qApplication  ()

    -- Parse arguments
    args <- App.getArgs
    dbFile <- case args of
                (f:_) -> return f
                _     -> App.getUserDataPath "mescaline.db"
    let pattern = case args of
                    (_:p:_) -> p
                    _       -> "%"
        
    -- FIXME: .ui file is not found in the resource for some reason
    -- rcc <- Qt.registerResource "app/mescaline.rcc"
    -- engine <- qScriptEngine ()
    -- scriptFile <- qFile ":/calculator.js"
    -- open scriptFile fReadOnly
    -- ss <- qTextStream scriptFile
    -- ra <- readAll ss ()
    -- dv <- evaluate engine ra
    -- close scriptFile ()

    mainWindow <- loadUI =<< App.getResourcePath "mescaline.ui"

    units <- getUnits dbFile pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]

    seq_ichan <- newChan
    (seqView, seq_ochan) <- sequencerView 30 2 sequencer0 seq_ichan    
    mute <- newMVar False
    
    -- Global key event handler
    -- mainWindow <- Qt.findChild ui ("<QMainWindow*>", "mainWindow") :: IO (Qt.QMainWindow ())
    -- print window
    -- Qt.setHandler mainWindow "keyPressEvent(QKeyEvent*)" $ windowKeyPressEvent
    
    -- Set up actions
    aboutAction <- Qt.qAction ("About Mescaline", mainWindow)
    Qt.setStatusTip aboutAction "Show about message box"
    Qt.connectSlot  aboutAction "triggered()" mainWindow "about()" about

    clearAction <- Qt.qAction ("Clear Sequencer", mainWindow)
    Qt.setShortcut  clearAction =<< Qt.qKeySequence "c"
    Qt.setStatusTip clearAction "Clear sequencer"
    Qt.connectSlot  clearAction "triggered()" mainWindow "clearSequencer()" $ clearSequencer seq_ichan

    muteAction <- Qt.qAction ("Mute Sequencer", mainWindow)
    Qt.setShortcut  muteAction =<< Qt.qKeySequence "m"
    Qt.setStatusTip muteAction "Mute sequencer"
    Qt.connectSlot  muteAction "triggered()" mainWindow "muteSequencer()" $ muteSequencer mute
    
    scrubbleAction <- Qt.qAction ("Scrubble", mainWindow)
    Qt.setShortcut  scrubbleAction =<< Qt.qKeySequence "s"
    Qt.setStatusTip scrubbleAction "Scrubble"
    Qt.connectSlot  scrubbleAction "triggered()" mainWindow "scrubble()" $ scrubble
    
    -- Set up menus
    menuBar <- Qt.menuBar mainWindow ()
    aboutMenu <- Qt.addMenu menuBar "about.Mescaline"
    Qt.addAction aboutMenu aboutAction

    sequencerMenu <- Qt.addMenu menuBar "Sequencer"
    Qt.addAction sequencerMenu muteAction
    Qt.addAction sequencerMenu clearAction
    Qt.addAction sequencerMenu scrubbleAction

    graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "sequencerView")
    Qt.setScene graphicsView seqView

    -- matrixBox <- G.xmlGetWidget xml G.castToContainer "matrix"
    -- G.containerAdd matrixBox canvas
    -- tempo <- G.xmlGetWidget xml G.castToSpinButton "tempo"
    -- G.onValueSpinned tempo $ do
    --     t <- G.spinButtonGetValue tempo
    --     let t' = 60/t/4
    --     writeChan ichan (setVal tick t')
    
    fspace_ichan <- newChan
    let fspace = FeatureSpace.fromList (map (second head) units) (Random.mkStdGen 0)
    (fspaceView, fspace_ochan) <- FeatureSpaceView.featureSpaceView numRegions fspace fspace_ichan

    graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "featureSpaceView")
    Qt.setRenderHints graphicsView (Qt.fAntialiasing :: Qt.RenderHints)
    Qt.setScene graphicsView fspaceView
    Qt.setDragMode graphicsView Qt.eScrollHandDrag
    -- Qt.fitInView graphicsView (Qt.rectF 0 0 1 1)
    Qt.qscale graphicsView (600::Double, 600::Double)

    -- Pipe sequencer output to feature space
    forkIO $ fix $ \loop -> do
        (t, s) <- readChan seq_ochan
        let is = map (flip div numRegions . fst) $ indicesAtCursor s
        mapM_ (writeChan fspace_ichan . FeatureSpaceView.ActivateRegion . (,) t) is
        loop
    
    -- Fork synth process
    (synth_ichan, synth_ochan) <- startSynth
    
    -- Pipe feature space view output to synth
    forkIO $ pipe (return . Left) fspace_ochan synth_ichan

    -- ctor <- evaluate engine "Calculator"
    -- scriptUi <- newQObject engine ui
    -- calc <- construct ctor [scriptUi]
    Qt.qshow mainWindow ()
    Qt.activateWindow mainWindow ()
    
    ok <- Qt.qApplicationExec ()
    
    -- Signal synth thread and wait for it to exit.
    -- Otherwise stale scsynth processes will be lingering around.
    stopSynth (synth_ichan, synth_ochan)

    putStrLn "Bye sucker."
    Qt.returnGC
