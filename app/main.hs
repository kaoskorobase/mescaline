{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Exception
import           Control.Monad (unless)
import           Data.Accessor
import           Data.Char (ord)
import           Data.Function (fix)
import           Data.Maybe
import           Database.HDBC (quickQuery')
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Sampler as Synth
import qualified Mescaline.Synth.Pattern as P
import qualified Mescaline.Synth.Sequencer.Model as Sequencer
import qualified Mescaline.Synth.Sequencer.Process as SequencerP
import qualified Mescaline.Synth.Sequencer.View as SequencerView
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import qualified Mescaline.Synth.FeatureSpace.View as FeatureSpaceView
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
import qualified Qtc.Gui.QMainWindow_h          as Qt
import qualified Qtc.Gui.QMenu                  as Qt
import qualified Qtc.Gui.QMenuBar               as Qt
import qualified Qtc.Gui.QMessageBox            as Qt
import qualified Qtc.Gui.QWidget                as Qt
import qualified Qtc.Gui.QWidget_h              as Qt
import qualified Qtc.Tools.QUiLoader            as Qt
import qualified Qtc.Tools.QUiLoader_h          as Qt

numRegions :: Int
numRegions = 4

sequencer0 :: Sequencer.Sequencer ()
sequencer0 = Sequencer.cons 16 16 0.125 (Sequencer.Bar (-1))

setEnv = setVal (P.attackTime) 0.01 . setVal (P.releaseTime) 0.02

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
mainWindow mw = do
    -- Qt.setHandler mw "keyPressEvent(QKeyEvent*)" $ windowKeyPressEvent
    -- Qt.setFocus mw Qt.eOtherFocusReason
    -- Qt.grabKeyboard mw ()
    Qt.qSubClass $ Qt.qCast_QMainWindow mw

windowKeyPressEvent :: Qt.QWidget () -> Qt.QKeyEvent () -> IO ()
windowKeyPressEvent this evt = putStrLn "yeah!" -- >> Qt.keyPressEvent_h this evt

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

clearSequencer :: SequencerP.Sequencer () -> Qt.QMainWindow () -> IO ()
clearSequencer h _ = sendTo h $ SequencerP.ClearAll

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

startSynth :: IO (Chan (Either (Time, Unit.Unit) ()), Chan ())
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
        (Server.withSynth
            serverOptions
            rtOptions
            Server.defaultOutputHandler
        -- (Server.withTransport
        --     serverOptions
        --     rtOptions
            $ \(t :: OSC.UDP) -> do
                synth <- Synth.new t serverOptions
                fix $ \loop -> do
                    e <- readChan ichan
                    case e of
                        Left (t, u) -> do
                            -- b <- readMVar mute
                            -- unless b $ do
                            -- print u
                            sendTo synth $ Synth.PlayUnit t u (setEnv P.defaultSynth)
                            -- return ()
                            loop
                        Right _ -> return ()) `finally` writeChan ochan ()
    return (ichan, ochan)

stopSynth :: (Chan (Either (Time, Unit.Unit) ()), Chan ()) -> IO ()
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
    -- Qt.setHandler mainWindow "keyPressEvent(QKeyEvent*)" $ windowKeyPressEvent

    -- units <- getUnits dbFile pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]

    seqP <- SequencerP.new sequencer0
    (seqView, seqViewP) <- SequencerView.new 30 2 seqP
    seqViewP `listenTo` seqP
    mute <- newMVar False
    
    -- Global key event handler
    -- mainWindow <- Qt.findChild ui ("<QMainWindow*>", "mainWindow") :: IO (Qt.QMainWindow ())
    -- print window
    
    -- Set up actions
    aboutAction <- Qt.qAction ("About Mescaline", mainWindow)
    Qt.setStatusTip aboutAction "Show about message box"
    Qt.connectSlot  aboutAction "triggered()" mainWindow "about()" about

    clearAction <- Qt.qAction ("Clear Sequencer", mainWindow)
    Qt.setShortcut  clearAction =<< Qt.qKeySequence "c"
    Qt.setStatusTip clearAction "Clear sequencer"
    Qt.connectSlot  clearAction "triggered()" mainWindow "clearSequencer()" $ clearSequencer seqP

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
    
    -- Feature space process
    -- let fspace = FeatureSpace.fromList (map (second head) units) (Random.mkStdGen 0)
    fspaceP <- FeatureSpaceP.new
    -- Pipe sequencer output to feature space
    fspaceSeqP <- spawn $ fix $ \loop -> do
        x <- recv
        case x of
            SequencerP.Changed t s ->
                let is = map (flip div numRegions . fst) $ Sequencer.indicesAtCursor s
                in mapM_ (sendTo fspaceP . FeatureSpaceP.ActivateRegion t) is
        loop
    fspaceSeqP `listenTo` seqP
    
    -- Feature space view
    (fspaceView, fspaceViewP) <- FeatureSpaceView.new fspaceP
    fspaceViewP `listenTo` fspaceP
    
    graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "featureSpaceView")
    Qt.setRenderHints graphicsView (Qt.fAntialiasing :: Qt.RenderHints)
    Qt.setScene graphicsView fspaceView
    Qt.setDragMode graphicsView Qt.eScrollHandDrag
    -- Qt.fitInView graphicsView (Qt.rectF 0 0 1 1)
    Qt.qscale graphicsView (600::Double, 600::Double)

    mapM_ (\i -> sendTo fspaceP $ FeatureSpaceP.AddRegion 0.5 0.5 0.025) [0..numRegions-1]
    sendTo fspaceP $ FeatureSpaceP.LoadDatabase dbFile pattern
    
    -- Fork synth process
    (synth_ichan, synth_ochan) <- startSynth
    
    -- Pipe feature space view output to synth
    toSynthP <- spawn $ fix $ \loop -> do
        x <- recv
        case x of
            FeatureSpaceP.UnitActivated t u -> io $ writeChan synth_ichan (Left (t, u))
            _                               -> return ()
        loop
    toSynthP `listenTo` fspaceP

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
