{-# LANGUAGE Arrows #-}
import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Chunked
import           Control.Concurrent.MVar
import           Control.Monad (unless)
import           Data.Accessor
import           Data.Char (ord)
import           Data.Function (fix)
import           Data.Maybe
import           Database.HDBC (quickQuery')
import           Mescaline (Time)
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
import           Mescaline.Synth.SSF as SF
import qualified Qt as Q
import           Sound.OpenSoundControl hiding (Time)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Server
import qualified System.Environment as Env
import qualified System.Random as Random
import           Prelude hiding (and, (.), id, init, scanl)

import Debug.Trace

-- TODO: Implement start time quantization based on master clock signal.
clock :: SF Double (Event Time)
clock = (logicalTime &&& id) >>> scanl f (Nothing, NoEvent) >>> arr snd
    where
        f (Nothing, _) (globalTime, tick)
            = (Just (globalTime+tick), Event globalTime)
        f (Just localTime, _) (globalTime, tick)
            | localTime <= globalTime = (Just $ localTime+tick, Event localTime)
            | otherwise               = (Just $ localTime, NoEvent)

sequencer0 :: Sequencer ()
sequencer0 = Sequencer.cons 4 16 0.125 (Bar (-1))

-- sequencerOld :: SSF Double (Event (Sequencer ()))
-- sequencerOld = clock >>> tag (Sequencer.step (undefined::Score)) >>> accum sequencer0

-- | Left-biased event merge.
mergeList :: Event a -> Event a -> Event [a]
mergeList NoEvent   NoEvent   = NoEvent
mergeList (Event l) NoEvent   = Event [l]
mergeList NoEvent   (Event r) = Event [r]
mergeList (Event l) (Event r) = Event [l, r]

type Update a = Event (a -> a)
type Changed a = Event a

traceEvent e@NoEvent   = traceShow "traceEvent: NoEvent" e
traceEvent e@(Event _) = traceShow "traceEvent: Event"   e

sequencer :: Sequencer a -> SF (Update (Sequencer a)) (Event (Time, Sequencer a), Changed (Sequencer a))
sequencer s0 =
    proc update -> do
        rec
            c <- clock -< t
            e <- tag (Sequencer.step (undefined::Score)) -< c
            s <- accum s0 -< (foldl (.) id `fmap` (update `mergeList` e))
            t <- hold s0 >>> arr (getVal Sequencer.tick) -< s
        returnA -< (liftA2 (,) c s, s)
-- sequencer s0 = constant noEvent &&& accum s0

pipeChan f i o = do
    x <- readChan i
    case f x of
        Nothing -> return ()
        Just e  -> writeChan o e
    pipeChan f i o

setEnv = setVal (P.synth.>P.attackTime) 0.01 . setVal (P.synth.>P.releaseTime) 0.02

-- sequencerEvents :: [Unit] -> Time -> Sequencer a -> [P.SynthEvent]
sequencerEvents units t s = map (setEnv.f) is
    where
        -- is = map (\(r, c) -> r * cols s + c) $ indicesAtCursor s
        is = map fst $ indicesAtCursor s
        f i = P.SynthEvent t (units !! i) P.defaultSynth

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

    [dbFile, pattern, n] <- Env.getArgs

    -- FIXME: .ui file is not found in the resource for some reason
    rcc <- Q.registerResource "app/mescaline.rcc"
    -- engine <- qScriptEngine ()
    -- scriptFile <- qFile ":/calculator.js"
    -- open scriptFile fReadOnly
    -- ss <- qTextStream scriptFile
    -- ra <- readAll ss ()
    -- dv <- evaluate engine ra
    -- close scriptFile ()
    uiLoader <- Q.qUiLoader ()
    -- setHandler uiLoader "(QWidget*)createWidget(const QString&,QWidget*,const QString&)" $ myCreateWidget seqView
    uiFile <- Q.qFile "app/mescaline.ui"
    Q.open uiFile Q.fReadOnly
    ui <- Q.load uiLoader uiFile
    Q.close uiFile ()
    -- FIXME: .ui file is not found in the resource for some reason
    rcc <- Q.registerResource "app/mescaline.rcc"
    -- engine <- qScriptEngine ()
    -- scriptFile <- qFile ":/calculator.js"
    -- open scriptFile fReadOnly
    -- ss <- qTextStream scriptFile
    -- ra <- readAll ss ()
    -- dv <- evaluate engine ra
    -- close scriptFile ()
    uiLoader <- Q.qUiLoader ()
    -- setHandler uiLoader "(QWidget*)createWidget(const QString&,QWidget*,const QString&)" $ myCreateWidget seqView
    uiFile <- Q.qFile "app/mescaline.ui"
    Q.open uiFile Q.fReadOnly
    ui <- Q.load uiLoader uiFile
    Q.close uiFile ()

    units <- drop (read n) `fmap` getUnits dbFile pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]
    
    ichan <- newChan
    ochan <- newChan
    seq_ichan <- newChan
    synth <- Synth.newSampler

    (seqView, ichan) <- sequencerView 30 2 sequencer0 seq_ichan
    mute <- newMVar False
    Q.setHandler seqView "keyPressEvent(QKeyEvent*)" $ sceneKeyPressEvent mute ichan
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
    -- Q.fitInView graphicsView (Q.rectF 0 0 1 1)
    Q.qscale graphicsView (400::Double, 400::Double)
    
    
    -- Execute signal function
    t <- Server.openTransport Server.defaultRTOptionsUDP "127.0.0.1" :: IO UDP
    -- forkIO $ SF.execute (SF.Options Server.defaultServerOptions t 0.005)
    --     (sequencer sequencer0 >>> first (arr (fmap (uncurry (sequencerEvents (map fst units)))))) ichan ochan
    forkIO $ SF.execute (SF.Options Server.defaultServerOptions t 0.005)
        (sequencer sequencer0) ichan ochan

    -- Pipe feature space view output to sample player
    forkIO $ fix $ \loop -> do
        FeatureSpaceView.Activate (t, u) <- readChan fspace_ochan
        b <- readMVar mute
        unless b $ do
            print u
            Synth.playEvent synth (setEnv (P.fromUnit t u))
            return ()
        loop
    
    -- Dispatch events to and from sequencer view
    forkIO $ fix $ \loop -> do
        x <- readChan ochan
        case (fst.snd $ x) of
            NoEvent -> return ()
            -- Event es -> mapM_ (Synth.playEvent synth) es
            Event (t, s) -> do
                let is = map fst $ indicesAtCursor s
                mapM_ (writeChan fspace_ichan . FeatureSpaceView.ActivateRegion . (,) t) is
        case (snd.snd $ x) of
            NoEvent -> return ()
            Event s -> writeChan seq_ichan s
        loop
    
    -- ctor <- evaluate engine "Calculator"
    -- scriptUi <- newQObject engine ui
    -- calc <- construct ctor [scriptUi]
    Q.qshow ui ()
    ok <- Q.qApplicationExec ()
    return ()
