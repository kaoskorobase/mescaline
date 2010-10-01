{-# LANGUAGE Arrows #-}
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
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Server
import qualified System.Environment as Env
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import qualified System.Random as Random

import qualified Qtc.Classes.Gui                as Qt
import qualified Qtc.Classes.Qccs               as Qt
import qualified Qtc.Classes.Qccs_h             as Qt
import qualified Qtc.ClassTypes.Gui             as Qt
import qualified Qtc.Core.Base                  as Qt
import qualified Qtc.Core.QFile                 as Qt
import qualified Qtc.Enums.Base                 as Qt
import qualified Qtc.Enums.Classes.Core         as Qt
import qualified Qtc.Enums.Core.QIODevice       as Qt
import qualified Qtc.Enums.Core.Qt              as Qt
import qualified Qtc.Enums.Gui.QGraphicsView    as Qt
import qualified Qtc.Enums.Gui.QPainter         as Qt
import qualified Qtc.Gui.Base                   as Qt
import qualified Qtc.Gui.QApplication           as Qt
import qualified Qtc.Gui.QGraphicsView          as Qt
import qualified Qtc.Gui.QWidget                as Qt
import qualified Qtc.Tools.QUiLoader            as Qt

sequencer0 :: Sequencer ()
sequencer0 = Sequencer.cons 4 16 0.125 (Bar (-1))

setEnv = setVal (P.synth.>P.attackTime) 0.01 . setVal (P.synth.>P.releaseTime) 0.02

getUnits dbFile pattern features = do
    (units, sfMap) <- DB.withDatabase dbFile $ \c -> do
        Sql.unitQuery (quickQuery' c)
              ((Sql.url Sql.sourceFile `Sql.like` pattern) `Sql.and` (Sql.segmentation Sql.unit `Sql.eq` Unit.Onset))
              features
    case units of
        Left e -> fail e
        Right us -> return us

sceneKeyPressEvent :: MVar Bool -> Chan (Sequencer a -> Sequencer a) -> SequencerView -> Qt.QKeyEvent () -> IO ()
sceneKeyPressEvent mute chan this qkev = do
    key <- Qt.key qkev ()
    if key == Qt.qEnum_toInt Qt.eKey_C
        then writeChan chan Sequencer.deleteAll
        else if key == Qt.qEnum_toInt Qt.eKey_M
            then modifyMVar_ mute (return . not)
            else return ()

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
    uiLoader <- Qt.qUiLoader ()
    -- setHandler uiLoader "(QWidget*)createWidget(const QString&,QWidget*,const QString&)" $ myCreateWidget seqView
    uiFile <- Qt.qFile =<< App.getResourcePath "mescaline.ui"
    Qt.open uiFile Qt.fReadOnly
    ui <- Qt.load uiLoader uiFile
    Qt.close uiFile ()

    units <- getUnits dbFile pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]
    
    seq_ichan <- newChan
    (seqView, seq_ochan) <- sequencerView 30 2 sequencer0 seq_ichan
    
    mute <- newMVar False
    Qt.setHandler seqView "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent mute seq_ichan
    graphicsView <- Qt.findChild ui ("<QGraphicsView*>", "sequencerView")
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
    (fspaceView, fspace_ochan) <- FeatureSpaceView.featureSpaceView fspace fspace_ichan

    graphicsView <- Qt.findChild ui ("<QGraphicsView*>", "featureSpaceView")
    Qt.setRenderHints graphicsView (Qt.fAntialiasing :: Qt.RenderHints)
    Qt.setScene graphicsView fspaceView
    Qt.setDragMode graphicsView Qt.eScrollHandDrag
    -- Qt.fitInView graphicsView (Qt.rectF 0 0 1 1)
    Qt.qscale graphicsView (600::Double, 600::Double)

    -- Pipe feature space view output to sample player
    synth <- Synth.newSampler
    forkIO $ fix $ \loop -> do
        FeatureSpaceView.Activate (t, u) <- readChan fspace_ochan
        b <- readMVar mute
        unless b $ do
            -- print u
            Synth.playEvent synth (setEnv (P.fromUnit t u))
            return ()
        loop

    forkIO $ fix $ \loop -> do
        (t, s) <- readChan seq_ochan
        let is = map fst $ indicesAtCursor s
        mapM_ (writeChan fspace_ichan . FeatureSpaceView.ActivateRegion . (,) t) is
        loop
    
    -- ctor <- evaluate engine "Calculator"
    -- scriptUi <- newQObject engine ui
    -- calc <- construct ctor [scriptUi]
    Qt.qshow ui ()
    Qt.activateWindow ui ()
    
    ok <- Qt.qApplicationExec ()
    Qt.returnGC
