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
import qualified Data.List as List
import           Data.Maybe
import           Database.HDBC (quickQuery')
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Database.Process as DatabaseP
import qualified Mescaline.Synth.Pattern as P
import qualified Mescaline.Synth.Sampler as Synth
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
import           System.Directory
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
import qualified Qtc.Enums.Gui.QFileDialog      as Qt
import qualified Qtc.Enums.Gui.QGraphicsView    as Qt
import qualified Qtc.Enums.Gui.QPainter         as Qt
import qualified Qtc.Gui.Base                   as Qt
import qualified Qtc.Gui.QAction                as Qt
import qualified Qtc.Gui.QApplication           as Qt
import qualified Qtc.Gui.QDialog                as Qt
import qualified Qtc.Gui.QFileDialog            as Qt
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

-- ====================================================================
-- Menu definition utilities
-- TODO: Factor this into a separate module.

data ActionType = Trigger | Checkable deriving (Eq, Show)

data MenuDefinition =
    Menu String String [MenuDefinition]
  | Action String String String ActionType (Maybe String) (Qt.QWidget () -> Qt.QAction () -> IO ())

actionCallback :: Qt.QAction () -> (Qt.QWidget () -> Qt.QAction () -> IO ()) -> Qt.QWidget () -> Bool -> IO ()
actionCallback a cb w _ = cb w a

defineMenu :: [MenuDefinition] -> Qt.QMenuBar () -> Qt.QWidget () -> IO [(String, Qt.QAction ())]
defineMenu defs menuBar widget = mapM (f (Left menuBar) []) defs >>= return . concat
    where
        f menu path (Menu name display defs) = do
            menu' <- either (flip Qt.addMenu display) (flip Qt.addMenu display) menu
            mapM (f (Right menu') (path ++ [name])) defs >>= return . concat
        f menu path (Action name display statusTip actionType shortcut callback) = do
            case menu of
                Left _ -> return []
                Right menu -> do
                    action <- Qt.qAction (display, widget)
                    Qt.setStatusTip action statusTip
                    when (actionType == Checkable) (Qt.setCheckable action True)
                    maybe (return ()) (\s -> Qt.setShortcut action =<< Qt.qKeySequence s) shortcut
                    let path' = path ++ [name]
                        slot  = List.intercalate "_" path' ++ "()"
                        key   = "/" ++ List.intercalate "/" path'
                    Qt.connectSlot action "triggered()" widget slot (actionCallback action callback)
                    Qt.addAction menu action
                    return [(key, action)]

trigger :: String -> [(String, Qt.QAction ())] -> IO ()
trigger k = maybe (return ()) (flip Qt.trigger ()) . lookup k

-- ====================================================================
-- Actions

action_about :: Qt.QWidget () -> Qt.QAction () -> IO ()
action_about mw _ = Qt.qMessageBoxAbout (
        mw
      , "About Mescaline"
      , "<h3>About Mescaline</h3>"
        ++ "<a href=\"http://mescaline.globero.es\">Mescaline</a> "
        ++ "is a data-driven sequencer and synthesizer." )

importDialog :: Qt.FileMode -> Qt.QWidget () -> IO [FilePath]
importDialog fileMode w = do
    d <- Qt.qFileDialog w
    Qt.setFileMode d fileMode
    Qt.setViewMode d (Qt.eList :: Qt.QFileDialogViewMode)
    b <- Qt.exec d ()
    if b > 0
        then Qt.selectedFiles d ()
        else return []

action_file_importFile :: DatabaseP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_file_importFile db w _ = do
    ps <- importDialog Qt.eExistingFile w
    putStrLn $ "Import file: " ++ show ps
    sendTo db $ DatabaseP.Import ps

action_file_importDirectory :: DatabaseP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_file_importDirectory db w _ = do
    ps <- importDialog Qt.eDirectory w
    putStrLn $ "Import directory: " ++ show ps
    sendTo db $ DatabaseP.Import ps

action_sequencer_clear :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_clear h _ _ = sendTo h $ SequencerP.ClearAll

action_sequencer_mute :: MVar Bool -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_mute mute _ _ = modifyMVar_ mute (return . not)

action_sequencer_playPause :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_playPause h _ a = do
    b <- Qt.isChecked a ()
    sendTo h $ SequencerP.Transport $ if b then SequencerP.Start else SequencerP.Pause

action_sequencer_reset :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_reset h _ _ = sendTo h $ SequencerP.Transport SequencerP.Reset

scaleFeatureSpace :: Double -> Qt.QGraphicsView () -> IO ()
scaleFeatureSpace s v = Qt.qscale v (s, s)

setScaleFeatureSpace :: Double -> Qt.QGraphicsView () -> IO ()
setScaleFeatureSpace s v = do
    Qt.resetTransform v ()
    Qt.qscale v (s, s)

action_featureSpace_zoom_zoomIn :: Qt.QGraphicsView () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_featureSpace_zoom_zoomIn v _ _ = scaleFeatureSpace 2.0 v

action_featureSpace_zoom_zoomOut :: Qt.QGraphicsView () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_featureSpace_zoom_zoomOut v _ _ = scaleFeatureSpace 0.5 v

action_featureSpace_zoom_reset :: Qt.QGraphicsView () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_featureSpace_zoom_reset v _ _ = setScaleFeatureSpace 600 v

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
    
    -- Database process
    dbP <- DatabaseP.new
    addListenerWith (\(DatabaseP.Changed path pattern) -> FeatureSpaceP.LoadDatabase path pattern) dbP fspaceP
    sendTo dbP $ DatabaseP.Load dbFile pattern
    
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
    let menuDef = [
              Menu "about" "about.Mescaline"
                [ Action "about" "About Mescaline" "Show about message box" Trigger Nothing action_about ]
            , Menu "file" "File"
              [ Action "importFile" "Import File..." "Import a file" Trigger (Just "Ctrl+i") (action_file_importFile dbP)
              , Action "importDirectory" "Import Directory..." "Import a directory" Trigger (Just "Ctrl+Shift+I") (action_file_importDirectory dbP) ]
            , Menu "sequencer" "Sequencer"
              [ Action "play" "Play" "Start or pause the sequencer" Checkable (Just "SPACE") (action_sequencer_playPause seqP)
              , Action "reset" "Reset" "Reset the sequencer" Trigger (Just "RETURN") (action_sequencer_reset seqP)
              , Action "clear" "Clear" "Clear sequencer" Trigger (Just "Ctrl+k") (action_sequencer_clear seqP)
              -- , Action "mute" "Mute" "Mute sequencer" Trigger (Just "m") (action_sequencer_mute mute)
              ]
            , Menu "featureSpace" "FeatureSpace"
              [ Menu "zoom" "Zoom"
                [ Action "zoomIn" "Zoom In" "Zoom into feature space" Trigger (Just "Ctrl++") (action_featureSpace_zoom_zoomIn fspace_graphicsView)
                , Action "zoomOut" "Zoom Out" "Zoom out of feature space" Trigger (Just "Ctrl+-") (action_featureSpace_zoom_zoomOut fspace_graphicsView)
                , Action "reset" "Reset" "Reset feature space zoom" Trigger (Just "Ctrl+0") (action_featureSpace_zoom_reset fspace_graphicsView) ] ] ]

    menuBar <- Qt.menuBar mainWindow ()
    actions <- defineMenu menuDef menuBar (Qt.objectCast mainWindow)
    trigger "/featureSpace/zoom/reset" actions

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
