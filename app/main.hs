{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Exception
import           Control.Monad
import           Data.Accessor
import           Data.Bits
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
import qualified Qtc.Gui.QGraphicsView_h        as Qt
import qualified Qtc.Gui.QKeySequence           as Qt
import qualified Qtc.Gui.QMainWindow            as Qt
import qualified Qtc.Gui.QMainWindow_h          as Qt
import qualified Qtc.Gui.QMenu                  as Qt
import qualified Qtc.Gui.QMenuBar               as Qt
import qualified Qtc.Gui.QMessageBox            as Qt
import qualified Qtc.Gui.QWheelEvent            as Qt
import qualified Qtc.Gui.QWidget                as Qt
import qualified Qtc.Gui.QWidget_h              as Qt
import qualified Qtc.Tools.QUiLoader            as Qt
import qualified Qtc.Tools.QUiLoader_h          as Qt

numRegions :: Int
numRegions = 4

sequencer0 :: Sequencer.Sequencer ()
sequencer0 = Sequencer.cons 16 16 0.125 (Sequencer.Bar 0)

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

featureSpaceViewKeyPressEvent :: Qt.QGraphicsView () -> Qt.QKeyEvent () -> IO ()
featureSpaceViewKeyPressEvent view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Control
        then Qt.setDragMode view Qt.eScrollHandDrag
        else Qt.keyPressEvent_h view evt

featureSpaceViewKeyReleaseEvent :: Qt.QGraphicsView () -> Qt.QKeyEvent () -> IO ()
featureSpaceViewKeyReleaseEvent view evt = do
    key <- Qt.key evt ()
    if key == Qt.qEnum_toInt Qt.eKey_Control
        then Qt.setDragMode view Qt.eNoDrag
        else Qt.keyPressEvent_h view evt

-- featureSpaceViewWheelEvent :: Qt.QGraphicsView () -> Qt.QWheelEvent () -> IO ()
-- featureSpaceViewWheelEvent view evt = do
--     mods <- Qt.modifiers evt ()
--     when (Qt.qFlags_toInt mods .&. Qt.qEnum_toInt Qt.eControlModifier == 0) $ do
--         units <- Qt.delta evt ()
--         let deg = fromIntegral units / 8
--             scale0 = deg * 1.5
--             scale = if scale0 == 0 then 1 else if scale0 < 0 then 1/negate scale0 else scale0
--         scaleFeatureSpace scale view

mkFeatureSpaceView :: Qt.QWidget a -> IO (Qt.QGraphicsView ())
mkFeatureSpaceView parent = do
    view <- Qt.qGraphicsView parent
    Qt.setRenderHints view (Qt.fAntialiasing :: Qt.RenderHints)
    Qt.setHandler view "keyPressEvent(QKeyEvent*)" featureSpaceViewKeyPressEvent
    Qt.setHandler view "keyReleaseEvent(QKeyEvent*)" featureSpaceViewKeyReleaseEvent
    -- Qt.fitInView graphicsView (Qt.rectF 0 0 1 1)
    Qt.setTransformationAnchor view Qt.eAnchorUnderMouse
    return view

-- | Custom createWidget handler.
--
-- This is needed because it is not possible to set event handlers on objects return from the UI loader (??).
createWidget :: Qt.QUiLoader () -> String -> Qt.QWidget a -> String -> IO (Qt.QWidget ())
createWidget this className parent name = do
    if (className == "FeatureSpaceView")
        then do
            view <- mkFeatureSpaceView parent
            Qt.setObjectName view name
            return (Qt.objectCast view)
        else do
            Qt.createWidget_h this (className, parent, name)

loadUI :: FilePath -> IO (MainWindow)
loadUI path = do
    uiLoader <- Qt.qUiLoader ()
    Qt.setHandler uiLoader "(QWidget*)createWidget(const QString&,QWidget*,const QString&)" createWidget
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

playPauseSequencer :: Qt.QAction () -> SequencerP.Sequencer () -> Qt.QMainWindow () -> Bool -> IO ()
playPauseSequencer a h _ _ = do
    b <- Qt.isChecked a ()
    sendTo h $ SequencerP.Transport $ if b then SequencerP.Start else SequencerP.Pause

resetSequencer :: SequencerP.Sequencer () -> Qt.QMainWindow () -> Bool -> IO ()
resetSequencer h _ _ = sendTo h $ SequencerP.Transport SequencerP.Reset

scaleFeatureSpace :: Double -> Qt.QGraphicsView () -> IO ()
scaleFeatureSpace s v = Qt.qscale v (s, s)

setScaleFeatureSpace :: Double -> Qt.QGraphicsView () -> IO ()
setScaleFeatureSpace s v = do
    Qt.resetTransform v ()
    Qt.qscale v (s, s)

zoomInFeatureSpace :: Qt.QGraphicsView () -> Qt.QMainWindow () -> Bool -> IO ()
zoomInFeatureSpace v _ _ = scaleFeatureSpace 2.0 v

zoomOutFeatureSpace :: Qt.QGraphicsView () -> Qt.QMainWindow () -> Bool -> IO ()
zoomOutFeatureSpace v _ _ = scaleFeatureSpace 0.5 v

zoomResetFeatureSpace :: Qt.QGraphicsView () -> Qt.QMainWindow () -> Bool -> IO ()
zoomResetFeatureSpace v _ _ = setScaleFeatureSpace 600 v

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

    -- Sequencer process
    seqP <- SequencerP.new sequencer0
    (seqView, seqViewP) <- SequencerView.new 30 2 seqP
    seqViewP `listenTo` seqP
    mute <- newMVar False
    
    -- Sequencer view
    seq_graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "sequencerView")
    Qt.setScene seq_graphicsView seqView

    -- matrixBox <- G.xmlGetWidget xml G.castToContainer "matrix"
    -- G.containerAdd matrixBox canvas
    -- tempo <- G.xmlGetWidget xml G.castToSpinButton "tempo"
    -- G.onValueSpinned tempo $ do
    --     t <- G.spinButtonGetValue tempo
    --     let t' = 60/t/4
    --     writeChan ichan (setVal tick t')
    
    -- Feature space process
    fspaceP <- FeatureSpaceP.new
    -- Pipe sequencer output to feature space
    fspaceSeqP <- spawn $ fix $ \loop -> do
        x <- recv
        case x of
            SequencerP.Changed t s transport ->
                case transport of
                    SequencerP.Running -> let is = map (flip div numRegions . fst) $ Sequencer.indicesAtCursor s
                                          in mapM_ (sendTo fspaceP . FeatureSpaceP.ActivateRegion t) is
                    _                 -> return ()
        loop
    fspaceSeqP `listenTo` seqP
    
    -- Feature space view
    (fspaceView, fspaceViewP) <- FeatureSpaceView.new fspaceP
    fspaceViewP `listenTo` fspaceP
    
    fspace_graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "featureSpaceView")
    Qt.setScene fspace_graphicsView fspaceView

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

    -- Set up actions and menus
    menuBar <- Qt.menuBar mainWindow ()

    -- "About" menu
    aboutAction <- Qt.qAction ("About Mescaline", mainWindow)
    Qt.setStatusTip aboutAction "Show about message box"
    Qt.connectSlot  aboutAction "triggered()" mainWindow "about()" about

    aboutMenu <- Qt.addMenu menuBar "about.Mescaline"
    Qt.addAction aboutMenu aboutAction

    -- "Sequencer" menu
    sequencerMenu <- Qt.addMenu menuBar "Sequencer"

    playAction <- Qt.qAction ("Play", mainWindow)
    Qt.setCheckable playAction True
    Qt.setShortcut  playAction =<< Qt.qKeySequence "SPACE"
    Qt.setStatusTip playAction "Start or pause the sequencer"
    Qt.connectSlot  playAction "triggered()" mainWindow "playPauseSequencer()" $ playPauseSequencer playAction seqP
    Qt.addAction sequencerMenu playAction

    resetAction <- Qt.qAction ("Reset", mainWindow)
    Qt.setShortcut  resetAction =<< Qt.qKeySequence "RETURN"
    Qt.setStatusTip resetAction "Reset the sequencer"
    Qt.connectSlot  resetAction "triggered()" mainWindow "resetSequencer()" $ resetSequencer seqP
    Qt.addAction sequencerMenu resetAction

    clearAction <- Qt.qAction ("Clear Sequencer", mainWindow)
    Qt.setShortcut  clearAction =<< Qt.qKeySequence "Ctrl+k"
    Qt.setStatusTip clearAction "Clear sequencer"
    Qt.connectSlot  clearAction "triggered()" mainWindow "clearSequencer()" $ clearSequencer seqP
    Qt.addAction sequencerMenu clearAction

    -- muteAction <- Qt.qAction ("Mute Sequencer", mainWindow)
    -- Qt.setShortcut  muteAction =<< Qt.qKeySequence "m"
    -- Qt.setStatusTip muteAction "Mute sequencer"
    -- Qt.connectSlot  muteAction "triggered()" mainWindow "muteSequencer()" $ muteSequencer mute
    -- Qt.addAction sequencerMenu muteAction
      
    -- "FeatureSpace" menu
    fspaceMenu <- Qt.addMenu menuBar "FeatureSpace"

    zoomMenu <- Qt.addMenu fspaceMenu "Zoom"

    zoomInAction <- Qt.qAction ("Zoom In", mainWindow)
    Qt.setShortcut  zoomInAction =<< Qt.qKeySequence "Ctrl++"
    Qt.setStatusTip zoomInAction "Zoom into feature space"
    Qt.connectSlot  zoomInAction "triggered()" mainWindow "zoomInFeatureSpace()" $ zoomInFeatureSpace fspace_graphicsView
    Qt.addAction zoomMenu zoomInAction

    zoomOutAction <- Qt.qAction ("Zoom Out", mainWindow)
    Qt.setShortcut  zoomOutAction =<< Qt.qKeySequence "Ctrl+-"
    Qt.setStatusTip zoomOutAction "Zoom out of feature space"
    Qt.connectSlot  zoomOutAction "triggered()" mainWindow "zoomOutFeatureSpace()" $ zoomOutFeatureSpace fspace_graphicsView
    Qt.addAction zoomMenu zoomOutAction
    
    zoomResetAction <- Qt.qAction ("Reset", mainWindow)
    Qt.setShortcut  zoomResetAction =<< Qt.qKeySequence "Ctrl+0"
    Qt.setStatusTip zoomResetAction "Reset feature space zoom"
    Qt.connectSlot  zoomResetAction "triggered()" mainWindow "zoomResetFeatureSpace()" $ zoomResetFeatureSpace fspace_graphicsView
    Qt.trigger zoomResetAction ()
    Qt.addAction zoomMenu zoomResetAction
    
    -- Start the application

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
