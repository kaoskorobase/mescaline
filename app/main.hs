{-# LANGUAGE CPP, ScopedTypeVariables #-}
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
import           Data.Version (showVersion)
import           Database.HDBC (quickQuery')
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Desktop as App
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Database.Process as DatabaseP
import qualified Mescaline.Synth.Sampler.Process as SynthP
#if USE_OLD_SEQUENCER == 1
import qualified Mescaline.Synth.Sequencer.Model as Sequencer
import qualified Mescaline.Synth.Sequencer.Process as SequencerP
import qualified Mescaline.Synth.Sequencer.View as SequencerView
#else
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
#endif -- USE_OLD_SEQUENCER
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import qualified Mescaline.Synth.FeatureSpace.View as FeatureSpaceView
import qualified Mescaline.Synth.Pattern as Pattern
import qualified Mescaline.Synth.Pattern.Environment as Pattern
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Patch as Patch
import qualified Mescaline.Synth.Pattern.Process as PatternP
import qualified Mescaline.Synth.Pattern.View as PatternView
import           Mescaline.Util (findFiles)
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
import qualified Qtc.Enums.Gui.QPainter         as Qt
import qualified Qtc.Gui.Base                   as Qt

import qualified Qtc.Gui.QApplication           as Qt

import qualified Qtc.Gui.QDialog                as Qt
import qualified Qtc.Gui.QFileDialog            as Qt
import qualified Qtc.Gui.QMessageBox            as Qt

import qualified Qtc.Enums.Gui.QGraphicsView    as Qt
import qualified Qtc.Gui.QGraphicsView          as Qt
import qualified Qtc.Gui.QGraphicsView          as Qt
import qualified Qtc.Gui.QGraphicsView_h        as Qt

import qualified Qtc.Gui.QKeySequence           as Qt
import qualified Qtc.Gui.QMainWindow            as Qt
import qualified Qtc.Gui.QMainWindow_h          as Qt

import qualified Qtc.Gui.QAction                as Qt
import qualified Qtc.Gui.QMenu                  as Qt
import qualified Qtc.Gui.QMenuBar               as Qt

import qualified Qtc.Enums.Gui.QTextCursor      as Qt
import qualified Qtc.Gui.QTextCursor            as Qt
import qualified Qtc.Gui.QTextEdit              as Qt
import qualified Qtc.Gui.QFontMetrics           as Qt

import qualified Qtc.Gui.QWheelEvent            as Qt
import qualified Qtc.Gui.QWidget                as Qt
import qualified Qtc.Gui.QWidget_h              as Qt
import qualified Qtc.Tools.QUiLoader            as Qt
import qualified Qtc.Tools.QUiLoader_h          as Qt

#if USE_OLD_SEQUENCER
numRegions :: Int
numRegions = length FeatureSpace.defaultRegions

sequencer0 :: Sequencer.Sequencer ()
sequencer0 = Sequencer.cons 16 16 0.125 (Sequencer.Bar 0)
#endif -- USE_OLD_SEQUENCER

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

-- ====================================================================
-- Menu definition utilities
-- TODO: Factor this into a separate module.

data ActionType = Trigger | Checkable deriving (Eq, Show)

data MenuDefinition =
    Menu String String [MenuDefinition]
  | Action String String String ActionType (Maybe String) (Qt.QWidget () -> Qt.QAction () -> IO ())
  | Separator

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
        f menu path Separator = do
            case menu of
                Left _ -> return ()
                Right menu -> Qt.addSeparator menu () >> return ()
            return []

defineWindowMenu :: [MenuDefinition] -> Qt.QMainWindow () -> IO [(String, Qt.QAction ())]
defineWindowMenu defs window = do
    menuBar <- Qt.menuBar window ()
    defineMenu defs menuBar (Qt.objectCast window)

trigger :: String -> [(String, Qt.QAction ())] -> IO ()
trigger k = maybe (return ()) (flip Qt.trigger ()) . lookup k

directoryMenu :: (FilePath -> Qt.QWidget () -> Qt.QAction () -> IO ()) -> [String] -> FilePath -> IO [MenuDefinition]
directoryMenu callback exts dir = liftM (map mkAction) (findFiles exts [dir])
    where
        mkAction path =
            Action
                (tr "/" '.' path)
                (makeRelative dir path)
                "" Trigger Nothing (callback path)
        tr s c = map (\x -> if elem x s then c else x)

-- ====================================================================
-- Logging to text view

chanLogger :: Log.Priority -> String -> Chan String -> IO () -> Log.GenericHandler (Chan String)
chanLogger prio fmt chan action =
    Log.GenericHandler
        prio
        (Log.simpleLogFormatter fmt)
        chan
        (\chan msg -> writeChan chan msg >> action)
        (const (return ()))

createLoggers :: MainWindow -> IO ()
createLoggers logWindow = do
    textEdit <- Qt.findChild logWindow ("<QTextEdit*>", "textEdit") :: IO (Qt.QTextEdit ())
    chan <- newChan
    Qt.connectSlot logWindow "logMessage()" logWindow "logMessage()" $ logMessage chan textEdit
    let fmt = "[$prio][$loggername] $msg\n"
        action = Qt.emitSignal logWindow "logMessage()" ()
    components <- Log.getComponents
    -- FIXME: The log levels have to be initialized first down in main, why?
    mapM_ (\(logger, prio) -> do
        Log.updateGlobalLogger
            logger
            (Log.setHandlers [chanLogger prio fmt chan action]))
            components
    -- Disable stderr logger
    Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers ([] :: [Log.GenericHandler ()]))
    where
        logMessage :: Chan String -> Qt.QTextEdit () -> MainWindow -> IO ()
        logMessage chan edit _ = do
            msg <- readChan chan
            c <- Qt.textCursor edit ()
            Qt.insertText c msg
            _ <- Qt.movePosition c (Qt.eEnd :: Qt.MoveOperation)
            Qt.setTextCursor edit c

clearLog :: MainWindow -> IO ()
clearLog logWindow = do
    edit <- Qt.findChild logWindow ("<QTextEdit*>", "textEdit") :: IO (Qt.QTextEdit ())
    Qt.setPlainText edit ""

-- ====================================================================
-- Actions

action_about :: Qt.QWidget () -> Qt.QAction () -> IO ()
action_about mw _ = Qt.qMessageBoxAbout (
        mw
      , "About Mescaline"
      , unwords [
            "<center><h2>Mescaline</h2></center>"
          , "<center><h4>Version " ++ showVersion App.version ++ "</h4></center>"
          , "<a href=\"http://mescaline.globero.es\">Mescaline</a>"
          , "is a data-driven sequencer and synthesizer." ] )

importDialog :: Qt.FileMode -> Qt.QWidget () -> IO [FilePath]
importDialog fileMode w = do
    d <- Qt.qFileDialog w
    Qt.setFileMode d fileMode
    Qt.setViewMode d (Qt.eList :: Qt.QFileDialogViewMode)
    b <- Qt.exec d ()
    if b > 0
        then Qt.selectedFiles d ()
        else return []

action_file_openFile :: PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_file_openFile h w _ = do
    ps <- importDialog Qt.eExistingFile w
    case ps of
        [] -> return ()
        (p:_) -> sendTo h $ PatternP.LoadPatch p

action_file_saveFile :: PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_file_saveFile h w _ = do
    ps <- importDialog Qt.eAnyFile w
    case ps of
        [] -> return ()
        (p:_) -> sendTo h $ PatternP.StorePatch p

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

#if USE_OLD_SEQUENCER
action_sequencer_playPause :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_playPause h _ a = do
    b <- Qt.isChecked a ()
    sendTo h $ SequencerP.Transport $ if b then SequencerP.Start else SequencerP.Pause

action_sequencer_reset :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_reset h _ _ = sendTo h $ SequencerP.Transport SequencerP.Reset

action_sequencer_clear :: SequencerP.Sequencer () -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_clear h _ _ = sendTo h $ SequencerP.ClearAll

action_sequencer_mute :: MVar Bool -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_sequencer_mute mute _ _ = modifyMVar_ mute (return . not)
#else
action_pattern_playPause :: PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_pattern_playPause h _ a = do
    b <- Qt.isChecked a ()
    sendTo h $ PatternP.Transport $ if b then PatternP.Start else PatternP.Pause

action_pattern_reset :: PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_pattern_reset h _ _ = sendTo h $ PatternP.Transport PatternP.Reset

action_pattern_run :: PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_pattern_run h _ _ = sendTo h PatternP.RunPatch

action_pattern_mute :: Int -> PatternP.Handle -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_pattern_mute i h _ a = do
    b <- Qt.isChecked a ()
    sendTo h $ PatternP.Mute i b
#endif -- USE_OLD_SEQUENCER

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

action_closeActiveWindow :: Qt.QWidget () -> Qt.QAction () -> IO ()
action_closeActiveWindow _ _ = Qt.qApplicationActiveWindow () >>= flip Qt.close () >> return ()

action_showWindow :: MainWindow -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_showWindow w _ _ = Qt.qshow w () >> Qt.activateWindow w ()

action_help_openUrl :: String -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_help_openUrl url _ _ = App.openUrl url >> return ()

action_help_manual :: Qt.QWidget () -> Qt.QAction () -> IO ()
action_help_manual = action_help_openUrl "http://mescaline.globero.es/documentation/manual"

action_help_patternRef :: Qt.QWidget () -> Qt.QAction () -> IO ()
action_help_patternRef = action_help_openUrl "http://mescaline.globero.es/doc/html/mescaline/Mescaline-Synth-Pattern-ASTLib.html"

action_help_openExample :: PatternP.Handle -> FilePath -> Qt.QWidget () -> Qt.QAction () -> IO ()
action_help_openExample process path _ _ = sendTo process (PatternP.LoadPatch path)

main :: IO ()
main = do
    mapM_ (\(l,p) -> Log.updateGlobalLogger l (Log.setLevel p)) =<< Log.getComponents

    app <- Qt.qApplication  ()

    -- Parse arguments
    args <- App.getArgs
    dbFile <- case args of
                (f:_) -> return f
                _     -> App.getUserDataPath "mescaline.db"
    let pattern = case args of
                    (_:p:_) -> p
                    _       -> "%"

    mainWindow <- loadUI =<< App.getResourcePath "mescaline.ui"
    editorWindow <- loadUI =<< App.getResourcePath "editor.ui"
    logWindow <- loadUI =<< App.getResourcePath "messages.ui"

    -- Qt.setHandler mainWindow "keyPressEvent(QKeyEvent*)" $ windowKeyPressEvent

    -- Synth process
    (synthP, synthQuit) <- SynthP.new

    -- Feature space process
    fspaceP <- FeatureSpaceP.new

    -- Feature space view
    (fspaceView, fspaceViewP) <- FeatureSpaceView.new fspaceP synthP
    connect Left fspaceP fspaceViewP
    connect (\x -> case x of
                SynthP.UnitStarted _ u -> Right $ FeatureSpaceView.HighlightOn u
                SynthP.UnitStopped _ u -> Right $ FeatureSpaceView.HighlightOff u)
            synthP fspaceViewP
    
    fspace_graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "featureSpaceView")
    Qt.setScene fspace_graphicsView fspaceView

    -- Sequencer process
#if USE_OLD_SEQUENCER == 1
    seqP <- SequencerP.new sequencer0
    (seqView, seqViewP) <- SequencerView.new 30 2 seqP
    seqViewP `listenTo` seqP
    mute <- newMVar False
#endif

    defaultPatch <- Patch.defaultPatch
    patternP <- PatternP.new defaultPatch fspaceP
    (patternView, patternViewP) <- PatternView.new patternP (Qt.objectCast editorWindow)
    patternViewP `listenTo` patternP

    -- Sequencer view
    seq_graphicsView <- Qt.findChild mainWindow ("<QGraphicsView*>", "sequencerView")
#if USE_OLD_SEQUENCER == 1
    Qt.setScene seq_graphicsView seqView
#else
    Qt.setScene seq_graphicsView patternView
#endif

    -- Pipe sequencer output to feature space
#if USE_OLD_SEQUENCER == 1
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
#endif

    -- Database process
    dbP <- DatabaseP.new
    connect (\(DatabaseP.Changed path pattern) -> FeatureSpaceP.LoadDatabase path pattern) dbP fspaceP
    sendTo dbP $ DatabaseP.Load dbFile pattern
    
    -- Pattern process
    patternToFSpaceP <- spawn $ fix $ \loop -> do
        x <- recv
        case x of
            PatternP.Event time event -> do
                -- io $ putStrLn $ "PatternP.Event " ++ show time ++ " " ++ show event
                Event.withSynth (return ()) (sendTo synthP . SynthP.PlayUnit time) event
            _ -> return ()
        loop
    patternToFSpaceP `listenTo` patternP
    fspaceToPatternP <- spawn $ fix $ \loop -> do
        x <- recv
        case x of
            FeatureSpaceP.RegionChanged _ -> do
                fspace <- query fspaceP FeatureSpaceP.GetModel
                sendTo patternP $ PatternP.SetFeatureSpace fspace
            _ -> return ()
        loop
    fspaceToPatternP `listenTo` fspaceP
    
    -- Pipe feature space view output to synth
    -- toSynthP <- spawn $ fix $ \loop -> do
    --     x <- recv
    --     case x of
    --         FeatureSpaceP.UnitActivated t u -> io $ writeChan synth_ichan (Left (t, (FeatureSpace.unit u)))
    --         _                               -> return ()
    --     loop
    -- toSynthP `listenTo` fspaceP

    -- Set up actions and menus
    examplesMenu <- directoryMenu (action_help_openExample patternP) ["msc"] =<< App.getResourcePath "patches/examples"
    
    let aboutAction = Action "about" "About Mescaline" "Show about message box" Trigger Nothing action_about
        darwinMenuDef = [ Menu "about" "about.Mescaline" [ aboutAction ] ]
        helpMenuDef   = [ Menu "help" "Help" [ aboutAction ] ]
        menuDef =
            (if App.buildOS == App.OSX then darwinMenuDef else [])
            ++
            [ Menu "file" "File"
              [ Action "openFile" "Open..." "Open file" Trigger (Just "Ctrl+o") (action_file_openFile patternP)
              , Action "saveFile" "Save" "Save file" Trigger (Just "Ctrl+s") (action_file_saveFile patternP)
              , Separator
              , Action "importFile" "Import File..." "Import a file" Trigger (Just "Ctrl+i") (action_file_importFile dbP)
              , Action "importDirectory" "Import Directory..." "Import a directory" Trigger (Just "Ctrl+Shift+I") (action_file_importDirectory dbP) ]
#if USE_OLD_SEQUENCER == 1
            , Menu "sequencer" "Sequencer"
              [ Action "play" "Play" "Start or pause the sequencer" Checkable (Just "SPACE") (action_sequencer_playPause seqP)
              , Action "reset" "Reset" "Reset the sequencer" Trigger (Just "RETURN") (action_sequencer_reset seqP)
              , Action "clear" "Clear" "Clear sequencer" Trigger (Just "Ctrl+k") (action_sequencer_clear seqP)
              -- , Action "mute" "Mute" "Mute sequencer" Trigger (Just "m") (action_sequencer_mute mute)
              ]
#else
            , Menu "sequencer" "Sequencer"
              [ Action "play" "Play" "Start or pause the sequencer" Checkable (Just "SPACE") (action_pattern_playPause patternP)
              , Action "reset" "Reset" "Reset the sequencer" Trigger (Just "Ctrl+RETURN") (action_pattern_reset patternP)
              , Action "run" "Run Patch" "Run the current patch" Trigger (Just "Ctrl+r") (action_pattern_run patternP)
              , Menu "mute" "Mute"
                (flip map [0..7] $ \i -> Action ("mute" ++ show i)
                                                ("Mute track " ++ show (i + 1))
                                                "" Checkable (Just ("Ctrl+" ++ show (i + 1)))
                                                (action_pattern_mute i patternP)) ]
#endif
            , Menu "featureSpace" "FeatureSpace"
              [ Menu "zoom" "Zoom"
                [ Action "zoomIn" "Zoom In" "Zoom into feature space" Trigger (Just "Ctrl++") (action_featureSpace_zoom_zoomIn fspace_graphicsView)
                , Action "zoomOut" "Zoom Out" "Zoom out of feature space" Trigger (Just "Ctrl+-") (action_featureSpace_zoom_zoomOut fspace_graphicsView)
                , Action "reset" "Reset" "Reset feature space zoom" Trigger (Just "Ctrl+0") (action_featureSpace_zoom_reset fspace_graphicsView) ] ]
            , Menu "window" "Window"
              [ Action "closeWindow" "Close" "Close window" Trigger (Just "Ctrl+w") action_closeActiveWindow
              , Separator
              , Action "mainWindow" "Main" "Show main window" Trigger (Just "Ctrl+Shift+w") (action_showWindow mainWindow)
              , Action "editorWindow" "Editor" "Show editor window" Trigger (Just "Ctrl+Shift+e") (action_showWindow editorWindow)
              , Separator
              , Action "logWindow" "Messages" "Show message window" Trigger (Just "Ctrl+Shift+m") (action_showWindow logWindow)
              , Action "clearLog" "Clear Messages" "Clear message window" Trigger (Just "Ctrl+Shift+c") (const (const (clearLog logWindow))) ]
            , Menu "help" "Help"
              [ Action "help" "Mescaline Help" "Open Mescaline manual in browser" Trigger Nothing action_help_manual
              , Action "help_patternRef" "Pattern Reference" "Open pattern reference in browser" Trigger Nothing action_help_patternRef
              , Separator
              , Menu "help_examples" "Examples" examplesMenu
              ]
            ]
            ++
            (if App.buildOS /= App.OSX then helpMenuDef else [])

    actions <- defineWindowMenu menuDef (Qt.objectCast mainWindow)
    trigger "/featureSpace/zoom/reset" actions
    defineWindowMenu menuDef (Qt.objectCast editorWindow)
    defineWindowMenu menuDef (Qt.objectCast logWindow)
    createLoggers logWindow -- =<< Qt.findChild logWindow ("<QTextEdit*>", "textEdit")

    -- Start the application
    Qt.qshow mainWindow ()
    Qt.activateWindow mainWindow ()

    ok <- Qt.qApplicationExec ()
    
    -- Signal synth thread and wait for it to exit.
    -- Otherwise stale scsynth processes will be lingering around.
    synthQuit

    putStrLn "Bye sucker."
    Qt.returnGC
