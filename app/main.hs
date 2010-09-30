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
import qualified Qt as Q
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Server
import qualified System.Environment as Env
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import qualified System.Random as Random

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

sceneKeyPressEvent :: MVar Bool -> Chan (Sequencer a -> Sequencer a) -> SequencerView -> Q.QKeyEvent () -> IO ()
sceneKeyPressEvent mute chan this qkev = do
    key <- Q.key qkev ()
    if key == Q.qEnum_toInt Q.eKey_C
        then writeChan chan Sequencer.deleteAll
        else if key == Q.qEnum_toInt Q.eKey_M
            then modifyMVar_ mute (return . not)
            else return ()

main :: IO ()
main = do
    app <- Q.qApplication  ()

    -- Parse arguments
    args <- App.getArgs
    dbFile <- case args of
                (f:_) -> return f
                _     -> App.getUserDataPath "mescaline.db"
    let pattern = case args of
                    (_:p:_) -> p
                    _       -> "%"
        
    -- FIXME: .ui file is not found in the resource for some reason
    -- rcc <- Q.registerResource "app/mescaline.rcc"
    -- engine <- qScriptEngine ()
    -- scriptFile <- qFile ":/calculator.js"
    -- open scriptFile fReadOnly
    -- ss <- qTextStream scriptFile
    -- ra <- readAll ss ()
    -- dv <- evaluate engine ra
    -- close scriptFile ()
    uiLoader <- Q.qUiLoader ()
    -- setHandler uiLoader "(QWidget*)createWidget(const QString&,QWidget*,const QString&)" $ myCreateWidget seqView
    uiFile <- Q.qFile =<< App.getResourcePath "mescaline.ui"
    Q.open uiFile Q.fReadOnly
    ui <- Q.load uiLoader uiFile
    Q.close uiFile ()

    units <- getUnits dbFile pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]
    
    seq_ichan <- newChan
    (seqView, seq_ochan) <- sequencerView 30 2 sequencer0 seq_ichan
    
    mute <- newMVar False
    Q.setHandler seqView "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent mute seq_ichan
    graphicsView <- Q.findChild ui ("<QGraphicsView*>", "sequencerView")
    Q.setScene graphicsView seqView

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

    graphicsView <- Q.findChild ui ("<QGraphicsView*>", "featureSpaceView")
    Q.setScene graphicsView fspaceView
    Q.setDragMode graphicsView Q.eScrollHandDrag
    -- Q.fitInView graphicsView (Q.rectF 0 0 1 1)
    Q.qscale graphicsView (600::Double, 600::Double)

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
    Q.qshow ui ()
    Q.activateWindow ui ()
    
    ok <- Q.qApplicationExec ()
    return ()
