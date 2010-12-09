module Mescaline.Pattern.View (
    View
  , Input
  , Output
  , new
) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Trans (MonadIO)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Mescaline.Application.Config as Config
import           Mescaline.Synth.FeatureSpace.View (getRegionColors)
import qualified Mescaline.Pattern.Sequencer as Model
import qualified Mescaline.Pattern.Process as Process
import qualified Mescaline.Pattern.Patch as Patch

import qualified Qtc.Classes.Gui                    as Qt
import qualified Qtc.Classes.Object                 as Qt
import qualified Qtc.Classes.Qccs_h                 as Qt
import qualified Qtc.ClassTypes.Gui                 as Qt
import qualified Qtc.Core.Base                      as Qt
import qualified Qtc.Enums.Core.Qt                  as Qt
import qualified Qtc.Gui.QBrush                     as Qt
import qualified Qtc.Gui.QFontMetrics               as Qt
import qualified Qtc.Gui.QGraphicsRectItem          as Qt
import           Qtc.Gui.QGraphicsRectItem_h        ()
import qualified Qtc.Gui.QGraphicsScene             as Qt
import qualified Qtc.Gui.QPen                       as Qt
import qualified Qtc.Gui.QTextEdit                  as Qt
import qualified Qtc.Gui.QWidget                    as Qt
import qualified Qth.Core.Point                     as Qt
import qualified Qth.Core.Rect                      as Qt

type View  = Qt.QGraphicsSceneSc (CView)
data CView = CView

newView :: IO (View)
newView = Qt.qSubClass (Qt.qGraphicsScene ())

data Params = Params {
    boxSize       :: Double
  , boxPadding    :: Double
  , cursorPadding :: Double
  } deriving (Show)

type FieldMap  = Map.Map (Int,Int) (Qt.QAbstractGraphicsShapeItem ())
type CursorMap = IntMap.IntMap (Qt.QAbstractGraphicsShapeItem ())

data CursorStyle = CursorStyle {
    cs_inactive :: IntMap.IntMap (Qt.QPen ())
  , cs_active   :: IntMap.IntMap (Qt.QPen ())
  }

data FieldStyle = FieldStyle {
    fs_empty  :: (Qt.QPen (), Qt.QBrush ())
  , fs_filled :: (Qt.QPen (), Qt.QBrush ())
  }

data State = State {
    params       :: Params
  , messageQueue :: Chan (Input)
  , sequencer    :: MVar (Maybe Model.Sequencer)
  , fields       :: FieldMap
  , cursors      :: CursorMap
  , fieldStyle   :: FieldStyle
  , cursorStyle  :: CursorStyle
  , editorWindow :: Qt.QWidget ()
  , editor       :: Qt.QTextEdit ()
  }

type Input  = Process.Output
type Output = ()

mouseHandler :: (Int, Int) -> ((Int, Int) -> Double -> IO ()) -> Qt.QGraphicsRectItem () -> Qt.QGraphicsSceneMouseEvent () -> IO ()
mouseHandler coord action this evt = action coord 1 >> Qt.mousePressEvent_h this evt

fieldPos :: Params -> Int -> Int -> (Double, Double)
fieldPos p r c =
    let y = boxPadding p + fromIntegral r * (boxSize p + boxPadding p)
        x = boxPadding p + fromIntegral c * (boxSize p + boxPadding p)
    in (x, y)

cursorPos :: Params -> Int -> Int -> (Double, Double)
cursorPos p r c =
    let (x, y) = fieldPos p r c
    in (x - cursorPadding p, y - cursorPadding p)

initScene :: Params -> View -> Model.Sequencer -> ((Int, Int) -> Double -> IO ()) -> IO (FieldMap, CursorMap)
initScene params view model action = do
    fs <- forM [0..Model.rows model - 1] $ \r -> do
            forM [0..Model.cols model - 1] $ \c -> do
                let (x, y) = fieldPos params r c
                    box = Qt.rectF 0 0 (boxSize params) (boxSize params)
                    coord = (r, c)
                item <- Qt.qGraphicsRectItem_nf box
                Qt.setPos item (Qt.pointF x y)
                Qt.setHandler item "mousePressEvent(QGraphicsSceneMouseEvent*)" $ mouseHandler coord action
                Qt.addItem view item
                return (coord, Qt.objectCast item)
    cs <- forM (Model.cursors model) $ \(i, c) -> do
        let (x, y) = cursorPos params (Model.row c) (Model.column c)
            box    = Qt.rectF 0 0
                        (boxSize params + 2 * cursorPadding params)
                        (boxSize params + 2 * cursorPadding params)
        item <- Qt.qGraphicsRectItem_nf box
        Qt.setPos item (Qt.pointF x y)
        Qt.addItem view item
        return (i, Qt.objectCast item)
    return (Map.fromList (concat fs), IntMap.fromList cs)

updateHandler :: State -> View -> IO ()
updateHandler state view = do
    e <- isEmptyChan (messageQueue state)
    unless e $ do
        msg <- readChan (messageQueue state)
        case msg of
            Process.Changed _ _ curSeq -> do
                modifyMVar_ (sequencer state) $ \prevSeq -> do
                    forM_ [0..Model.rows curSeq - 1] $ \r ->
                        forM_ [0..Model.cols curSeq - 1] $ \c -> do
                            let prevValue = join (Model.lookup r c `fmap` prevSeq)
                                curValue = Model.lookup r c curSeq
                            when (curValue /= prevValue) (updateField state r c curValue)
                    forM_ (Model.cursors curSeq) $ \(i, c) -> do
                        case IntMap.lookup i (cursors state) of
                            Nothing -> return ()
                            Just item -> updateCursor state item i c (Model.lookupAtCursor c curSeq)
                    return (Just curSeq)
                where
                    updateField state row col value = do
                        case Map.lookup (row,col) (fields state) of
                            Nothing -> return ()
                            Just item -> do
                                let (p, b) = (if (maybe 0 id value) > 0
                                                then fs_filled
                                                else fs_empty)
                                                (fieldStyle state)
                                Qt.setPen item p
                                Qt.setBrush item b
                    updateCursor state item cursorId cursor value = do
                        let (x, y) = cursorPos (params state)
                                               (Model.row cursor)
                                               (Model.column cursor)
                            f = if (maybe 0 id value) > 0
                                    then cs_active
                                    else cs_inactive
                        Qt.setPos item (Qt.pointF x y)
                        maybe (return ())
                              (Qt.setPen item)
                              (IntMap.lookup cursorId (f (cursorStyle state)))
            Process.PatchChanged patch patchPath -> do
                setPatch False state patch patchPath
            Process.PatchLoaded _ _ -> do
                Qt.qshow (editorWindow state) ()
                Qt.activateWindow (editorWindow state) ()
            Process.PatchStored patch patchPath -> do
                setPatch True state patch (Just patchPath)
            _ -> return ()

updateProcess :: MonadIO m => View -> State -> ReceiverT Input Output m ()
updateProcess view state = loop
    where loop = recv >>= io . update view state >> loop

update :: View -> State -> Input -> IO ()
update view state msg = do
    writeChan (messageQueue state) msg
    Qt.emitSignal view "update()" ()

windowTitle :: Maybe FilePath -> String
windowTitle path = "Mescaline - " ++ maybe "Untitled" id path ++ "[*]"

setPatch :: Bool -> State -> Patch.Patch -> Maybe FilePath -> IO ()
setPatch resetUndo state patch path = do
    Qt.setPlainText (editor state) (Patch.sourceCode patch)
    Qt.setWindowTitle (editorWindow state) (windowTitle path)
    -- This is actually redundant, since setPlainText resets the undo history
    when resetUndo $ do
        Qt.setUndoRedoEnabled (editor state) False
        Qt.setUndoRedoEnabled (editor state) True

textChanged :: Process.Handle -> State -> View -> IO ()
textChanged process state _ = do
    src <- Qt.toPlainText (editor state) ()
    sendTo process (Process.SetSourceCode src)

new :: Process.Handle -> Qt.QWidget () -> IO (View, Handle Input Output)
new process editorWindow = do
    config <- Config.getConfig

    boxSize <- Config.getIO config "Sequencer" "boxSize" :: IO Double
    boxPadding <- Config.getIO config "Sequencer" "boxPadding" :: IO Double
    cursorPadding <- Config.getIO config "Sequencer" "cursorPadding" :: IO Double
    cursorLineWidth <- Config.getIO config "Sequencer" "cursorLineWidth" :: IO Double
    activeCursorLineWidth <- Config.getIO config "Sequencer" "activeCursorLineWidth" :: IO Double
    
    view <- newView
    
    -- Set up text editor
    editor <- Qt.findChild editorWindow ("<QTextEdit*>", "textEdit") :: IO (Qt.QTextEdit ())
    fontMetrics <- Qt.qFontMetrics =<< Qt.currentFont editor ()
    charWidth <- Qt.averageCharWidth fontMetrics ()
    Qt.setTabStopWidth editor (charWidth * 8)
    
    model <- query process Process.GetSequencer

    let params = Params boxSize boxPadding cursorPadding
    (fMap, cMap) <- initScene params view model
                        (\(r, c) _ -> sendTo process $ Process.ModifyCell r c (maybe (Just 1) (const Nothing)))

    mq <- newChan
    modelVar <- newMVar Nothing
    tb             <- Qt.qBrush Qt.eblack
    fs_pen         <- Qt.qPen (tb, 1::Double, Qt.eSolidLine, Qt.eSquareCap, Qt.eMiterJoin)
    fs_emptyBrush  <- Qt.qBrush Qt.etransparent
    fs_filledBrush <- Qt.qBrush Qt.edarkGray
    
    regionBrushes  <- getRegionColors config >>= mapM Qt.qBrush
    cs_inactive    <- liftM (IntMap.fromList . zip [0..]) $
        mapM (\b -> Qt.qPen (b, cursorLineWidth, Qt.eSolidLine, Qt.eSquareCap, Qt.eMiterJoin))
             regionBrushes
    cs_active      <- liftM (IntMap.fromList . zip [0..]) $
        mapM (\b -> Qt.qPen (b, activeCursorLineWidth, Qt.eSolidLine, Qt.eSquareCap, Qt.eMiterJoin))
             regionBrushes

    let state = State
                    params
                    mq
                    modelVar
                    fMap cMap
                    (FieldStyle (fs_pen, fs_emptyBrush) (fs_pen, fs_filledBrush))
                    (CursorStyle cs_inactive cs_active)
                    editorWindow
                    editor

    Qt.connectSlot view "update()" view "update()" $ updateHandler state
    update view state (Process.Changed 0 Process.Stopped model)

    uncurry (setPatch True state) =<< query process Process.GetPatch
    Qt.connectSlot editor "textChanged()" view "textChanged()" $ textChanged process state

    handle <- spawn $ updateProcess view state

    return (view, handle)
