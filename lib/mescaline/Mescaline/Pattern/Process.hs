{-# LANGUAGE BangPatterns
           , ScopedTypeVariables #-}
module Mescaline.Pattern.Process (
    Handle
  , TransportState(..)
  , TransportChange(..)
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Exception.Peel
import           Control.Monad
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Seq
import           Data.Accessor
import qualified Data.BitSet as BitSet
import           Data.Maybe (fromJust)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Mescaline (Time)
import           Mescaline.Application (AppT, runAppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Data.PriorityQueue as PQ
import qualified Mescaline.Database as DB
import qualified Mescaline.FeatureSpace.Model as FeatureSpace
-- import qualified Mescaline.FeatureSpace.Process as FeatureSpaceP
import qualified Mescaline.FeatureSpace.Unit as Unit
import           Mescaline.Pattern.Environment (Environment)
import qualified Mescaline.Pattern.Environment as Environment
import           Mescaline.Pattern.Event (Event, UI(..))
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Pattern.Compiler as Comp
import           Mescaline.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Pattern.Sequencer as Sequencer
import           Mescaline.Pattern (Pattern)
import qualified Mescaline.Pattern as Model
import           Mescaline.Pattern.Patch (Patch)
import qualified Mescaline.Pattern.Patch as Patch
import qualified Mescaline.Pattern.Patch.Version_0_0_1 as Patch
import           Prelude hiding (catch)
import qualified Sound.OpenSoundControl.Time as Time
import qualified System.Log.Logger as Log

data TransportChange = Start | Pause | Reset deriving (Eq, Show)

data Input =
    SetCell         Int Int Double
  | ModifyCell      Int Int (Maybe Double -> Maybe Double)
  | ClearCell       Int Int
  | ClearAll
  | Transport       TransportChange
  | GetSequencer    (Query Sequencer)
  -- FeatureSpace
  | LoadDatabase    FilePath String
  | GetFeatureSpace (Query FeatureSpace.FeatureSpace)
  | SetFeatureSpace FeatureSpace.FeatureSpace
  | UpdateRegion    FeatureSpace.Region
  -- Patch access
  | GetPatch        (Query (Patch, Maybe FilePath))
  | LoadPatch       FilePath
  | StorePatch      FilePath
  | SetSourceCode   String
  | RunPatch
  -- Muting
  | Mute            Int Bool
  -- Internal messages
  | Event_          Int Time Event.Event

data Output =
    Changed      Time TransportState Sequencer.Sequencer
  | Event        Time Event.Event
  | PatchChanged Patch.Patch (Maybe FilePath)
  | PatchLoaded  Patch.Patch FilePath
  | PatchStored  Patch.Patch FilePath
  -- FeatureSpace
  | DatabaseLoaded  [Unit.Unit]
  | RegionChanged   FeatureSpace.Region

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State = State {
    time          :: Time
  , patch         :: Patch.Patch
  , patchFilePath :: Maybe FilePath
  , playerThread  :: Maybe PlayerHandle
  , mutedTracks   :: BitSet.BitSet Int
  , featureSpace  :: FeatureSpace.FeatureSpace
  }

transport :: State -> TransportState
transport = maybe Stopped (const Running) . playerThread

type EnvironmentUpdate = Environment -> Environment
-- type Player = Model.Player Environment Model.Event
type PlayerHandle = Process.Handle Environment ()

logger :: String
logger = "Mescaline.Sequencer"

applyUpdates :: MonadIO m => Environment -> ReceiverT EnvironmentUpdate () m (Environment, Bool)
applyUpdates a = loop (a, False)
    where
        loop (a, b) = do
            x <- poll
            case x of
                Nothing -> return (a, b)
                Just f  -> let a' = f a in a' `seq` loop (a', True)

get :: a -> ReceiverT a b m a
get = loop
    where
        loop a = do
            x <- poll
            case x of
                Nothing -> return a
                Just a' -> loop a'

polldl' :: (a -> b -> a) -> a -> ReceiverT b i m a
polldl' f = loop
    where
        loop a = do
            x <- poll
            case x of
                Nothing -> a
                Just b -> let a' = f a b in a' `seq` loop a'

-- | Log trace messages as a side effect and return the new environment.
logEnv :: MonadIO m => Environment -> m (Environment)
logEnv e = do
    let (ms, e') = Environment.getMessages e
    _ <- io $ forkIO $ mapM_ (Log.noticeM logger) ms
    return e'

type TaggedEvent = (Event, Int)

type Schedule = (Int, Pattern Event)

data Player = Player {
    player_environment :: Environment
  , player_schedule :: Maybe (Time, Pattern)
  }

stepPlayer :: Time -> Player -> ([(Time, Event)], Player)
stepPlayer tickTime player =
    let (es, sched) = loop (player_environment player) [] (player_schedule player)
    in (es, player { player_schedule = sched })
    where
        loop envir events sched =
            case sched of
                Nothing -> (events, Nothing)
                Just (time, pattern) ->
                    if time > tickTime
                        then (events, Just (time, pattern))
                        else case Model.step envir pattern of
                                Model.Done _ -> (events, Nothing)
                                Model.Result _ event pattern' ->
                                    let time' = time + event ^. Event.delta
                                    in loop ((time, event):events) (Just (time', pattern'))

data PlayerCommand =
    Quit
  | SetFeatureSpace FeatureSpace
  | SetSequencer Sequencer

playerProcess :: MonadIO m => Duration -> Handle -> IntMap Player -> Time -> ReceiverT PlayerCommand () m ()
playerProcess dt handle = loop
    where
        loop players tickTime = do
            (q, fs, sq) <- polldl' (\(a, b, c) x -> case x of
                                        Quit -> (True, b, c)
                                        SetFeatureSpace b' -> (a, b', c)
                                        SeqSequencer c' -> (a, b, c'))
                                   (False, Nothing, Nothing)
            if q
                then return ()
                else do
                    let players' = fmap (\p -> maybe p (\x -> p { player_environment = setVal Environment.featureSpace (player_environment p) x }) fs)
                                 $ fmap (\p -> maybe p (\x -> p { player_environment = setVal Environment.sequencer    (player_environment p) x }) sq)
                                 $ players
                        result = fmap (stepPlayer tickTime) players'
                        es = concatMap fst (IntMap.elems result)
                        players'' = fmap snd result
            mapM_ (\(r, es) -> mapM_ (\(t, e) -> sendTo handle $ Event_ r t e) es) es
            let tickTime' = tickTime + dt
            liftIO $ Time.pauseThreadUntil tickTime'
            loop players'' tickTime'
            -- case Model.step envir' pattern of
            --     Model.Done _ -> do
            --         -- _ <- logEnv envir'
            --         -- io $ Log.debugM logger "playerProcess: Model.Done"
            --         return ()
            --     Model.Result _ !(event, region) !pattern' -> do
            --         sendTo handle $ Event_ region time event
            --         -- envir'' <- logEnv envir'
            --         io $ Log.debugM logger $ "Event: " ++ show event
            --         -- let (row, col) = Model.cursorPosition (event ^. Model.cursor)
            --         --     cursor = Sequencer.Cursor row col
            --         --     envir'' = Environment.sequencer ^: Sequencer.modifyCursor (const cursor) (Model.cursorId (event ^. Model.cursor)) $ envir'
            --         let dt = event ^. Event.delta
            --         if dt > 0
            --             then do
            --                 -- sendTo handle $ Environment_ envir''
            --                 let time' = time + dt
            --                 io $ Time.pauseThreadUntil time'
            --                 loop envir' pattern' time'
            --             else loop envir' pattern' time

startPlayerThread :: Duration -> Handle -> IntMap Players -> Time -> IO PlayerHandle
startPlayerThread dt handle players time = spawn $ playerProcess dt handle players time

stopPlayerThread :: PlayerHandle -> IO ()
stopPlayerThread = kill

initPatch :: MonadIO m => Handle -> State -> AppT m ()
initPatch h state = do
    mapM_ (sendTo h . UpdateRegion) (Patch.regions (patch state))

    -- If specified in the config file, fill the whole sequencer in order to facilitate debugging.
    fill <- App.config "Sequencer" "debugFill" False
    when fill $ do
        forM_ [0..Sequencer.rows (Patch.sequencer (patch state))]$ \r ->
            forM_ [0..Sequencer.cols (Patch.sequencer (patch state))] $ \c ->
                sendTo h $ SetCell r c 1

    notifyListeners h (PatchChanged (patch state) (patchFilePath state))

new :: MonadIO m => Patch.Patch -> AppT m Handle
new patch0 = do
    time <- liftIO Time.utcr
    let state = State time patch0 Nothing Nothing BitSet.empty FeatureSpace.empty
    h <- liftIO . spawn . loop state =<< ask
    initPatch h state
    return h
    where
        -- updateSequencer state update = do
        --     case transport state of
        --         Stopped ->
        --             return $ Just $ state { patch = Patch.modifySequencer update (patch state) }
        --         Running -> do
        --             sendTo (fromJust (playerThread state)) (Environment.sequencer ^: update)
        --             return Nothing
        updateSequencer state update = do
            case transport state of
                Stopped ->
                    return $ Just $ state { patch = Patch.modifySequencer update (patch state) }
                Running -> do
                    sendTo (fromJust (playerThread state)) (Environment.sequencer ^: update)
                    return Nothing
        loop !state app = do
            x <- recv
            state' <-
                case x of
                    SetCell r c v ->
                        updateSequencer state (Sequencer.insert r c v)
                    ModifyCell r c f ->
                        updateSequencer state (Sequencer.alter f r c)
                    ClearCell r c ->
                        updateSequencer state (Sequencer.delete r c)
                    ClearAll ->
                        updateSequencer state Sequencer.clear
                    Transport tc -> do
                        case transport state of
                            Stopped ->
                                case tc of
                                    Start -> do
                                        proc <- self
                                        time <- io Time.utcr
                                        case Patch.pattern (patch state) of
                                            Left e -> do
                                                io $ Log.errorM logger (show e)
                                                return Nothing
                                            Right (pattern, bindings) -> do
                                                let env = Environment.mkEnvironment
                                                            0 0
                                                            bindings
                                                            (featureSpace state)
                                                            (Patch.sequencer (patch state))
                                                    patterns = map (\r -> (r, Just (time, pattern)))
                                                                   (FeatureSpace.regions (featureSpace state))
                                                tid  <- io $ startPlayerThread 0.001 proc env patterns time
                                                return $ Just $ state { time = time
                                                                      , playerThread = Just tid }
                                    Pause -> return Nothing
                                    Reset -> return $ Just $ state { patch = Patch.modifySequencer Sequencer.resetCursors (patch state) }
                            Running ->
                                case tc of
                                    Start -> return Nothing
                                    Pause -> do
                                        maybe (return ()) (io . stopPlayerThread) (playerThread state)
                                        return $ Just $ state { playerThread = Nothing }
                                    Reset -> do
                                        maybe (return ()) (io . stopPlayerThread) (playerThread state)
                                        proc <- self
                                        time <- io Time.utcr
                                        case Patch.pattern (patch state) of
                                            Left e -> do
                                                io $ Log.errorM logger (show e)
                                                return Nothing
                                            Right (pattern, bindings) -> do
                                                let patch' = Patch.modifySequencer Sequencer.resetCursors (patch state)
                                                    env = Environment.mkEnvironment
                                                            0 0
                                                            bindings
                                                            (featureSpace state)
                                                            (Patch.sequencer patch')
                                                    patterns = map (\r -> (r, Just (time, pattern)))
                                                                   (FeatureSpace.regions (featureSpace state))
                                                tid  <- io $ startPlayerThread 0.001 proc env patterns time
                                                return $ Just $ state { time = time
                                                                      , patch = patch'
                                                                      , playerThread = Just tid }
                    GetSequencer query -> do
                        answer query $ Patch.sequencer (patch state)
                        return Nothing
                    LoadDatabase path pattern -> do
                        units <- io $ DB.withDatabase path
                                    $ Unit.getUnits pattern [
                                        "es.globero.mescaline.spectral" ]
                          -- , "com.meapsoft.AvgChunkPower"
                          -- , "com.meapsoft.AvgFreqSimple" ]
                        let f' = FeatureSpace.setUnits (featureSpace state) (seqList rseq units `seq` units)
                        notify $ DatabaseLoaded (FeatureSpace.units f')
                        return $ Just state { featureSpace = f' }
                    GetFeatureSpace q -> do
                        answer q (featureSpace state)
                        return Nothing
                    SetFeatureSpace fspace -> do
                        -- maybe (return ()) (\h -> sendTo h (setVal Environment.featureSpace fspace)) (playerThread state)
                        return $ Just state { featureSpace = fspace }
                    UpdateRegion r -> do
                        notify $ RegionChanged r
                        return $ Just state { featureSpace = FeatureSpace.updateRegion r (featureSpace state) }
                    GetPatch query -> do
                        answer query (patch state, patchFilePath state)
                        return Nothing
                    LoadPatch path -> do
                        h <- self
                        flip runAppT app $
                            do { patch' <- Patch.load path
                               ; let state' = state { patch = patch'
                                                    , patchFilePath = Just path }
                               ; initPatch h state'
                               ; notifyListeners h (PatchLoaded patch' path)
                               ; return $ Just state' }
                               `catches`
                                    [ Handler (\(e :: Patch.LoadError)   -> liftIO $ Log.errorM logger (show e) >> return Nothing)
                                    , Handler (\(e :: Comp.CompileError) -> liftIO $ Log.errorM logger (show e) >> return Nothing) ]
                    StorePatch path -> do
                        let regions = FeatureSpace.regions (featureSpace state)
                            patch' = Patch.setRegions regions (patch state)
                        h <- self
                        liftIO $
                            do { Patch.store path patch'
                               ; notifyListeners h (PatchStored patch' path) }
                               `catch`
                               (\(e :: IOException) -> Log.errorM logger (show e))
                        return $ Just state { patch = patch', patchFilePath = Just path }
                    SetSourceCode src -> do
                        return $ Just state { patch = Patch.setSourceCode src (patch state) }
                    RunPatch -> do
                        res <- flip runAppT app $ Patch.evalSourceCode (patch state)
                        case res of
                            Left e -> do
                                io $ Log.errorM logger e
                                return Nothing
                            Right patch' -> do
                                let restart = transport state == Running
                                flip sendTo (Transport Pause) =<< self
                                when restart $ flip sendTo (Transport Start) =<< self
                                return $ Just state { patch = patch' }
                    Mute track b ->
                        return $ Just state { mutedTracks = (if b then BitSet.insert else BitSet.delete)
                                                                track (mutedTracks state) }
                    Event_ region time event ->
                        case transport state of
                            Running -> do
                                -- FIXME: Associate event streams with region id
                                unless False {- (BitSet.member (event ^. Event.cursor) (mutedTracks state)) -}
                                       (notify $ Event time event)
                                return Nothing
                            _ -> error "This shouldn't happen: Received an Event_ message but player thread not running"
                    -- Environment_ envir ->
                    --     return $ Just state { patch = Patch.modifySequencer (const (envir ^. Environment.sequencer)) (patch state) }
            case state' of
                Just state' -> do
                    notify $ Changed (time state') (transport state') (Patch.sequencer (patch state'))
                    loop state' app
                Nothing ->
                    loop state app
