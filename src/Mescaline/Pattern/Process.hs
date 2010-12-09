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
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (MonadIO)
import           Data.Accessor
import qualified Data.BitSet as BitSet
import           Data.Maybe (fromJust)
import           Mescaline (Time)
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import           Mescaline.Pattern.Environment (Environment)
import qualified Mescaline.Pattern.Environment as Environment
import           Mescaline.Pattern.Event (Event)
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

data TransportChange = Start | Pause | Reset deriving (Eq, Show)

data Input =
    SetCell         Int Int Double
  | ModifyCell      Int Int (Maybe Double -> Maybe Double)
  | ClearCell       Int Int
  | ClearAll
  | Transport       TransportChange
  | GetSequencer    (Query Sequencer)
  | SetFeatureSpace FeatureSpace.FeatureSpace
  -- Patch access
  | GetPatch        (Query (Patch, Maybe FilePath))
  | LoadPatch       FilePath
  | StorePatch      FilePath
  | SetSourceCode   String
  | RunPatch
  -- Muting
  | Mute            Int Bool
  -- Internal messages
  | Event_          Time Event.Event
  | Environment_    Environment

data Output =
    Changed      Time TransportState Sequencer.Sequencer
  | Event        Time Event.Event
  | PatchChanged Patch.Patch (Maybe FilePath)
  | PatchLoaded  Patch.Patch FilePath
  | PatchStored  Patch.Patch FilePath

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State = State {
    time          :: Time
  , patch         :: Patch.Patch
  , patchFilePath :: Maybe FilePath
  , playerThread  :: Maybe PlayerHandle
  , mutedTracks   :: BitSet.BitSet Int
  }

transport :: State -> TransportState
transport = maybe Stopped (const Running) . playerThread

type EnvironmentUpdate = Environment -> Environment
-- type Player = Model.Player Environment Model.Event
type PlayerHandle = Process.Handle EnvironmentUpdate ()

logger :: String
logger = "Sequencer"

applyUpdates :: MonadIO m => Environment -> ReceiverT EnvironmentUpdate () m (Environment, Bool)
applyUpdates a = loop (a, False)
    where
        loop (a, b) = do
            x <- poll
            case x of
                Nothing -> return (a, b)
                Just f  -> let a' = f a in a' `seq` loop (a', True)

-- | Log trace messages as a side effect and return the new environment.
logEnv :: MonadIO m => Environment -> m (Environment)
logEnv e = do
    let (ms, e') = Environment.getMessages e
    _ <- io $ forkIO $ mapM_ (Log.noticeM logger) ms
    return e'

playerProcess :: MonadIO m => Handle -> Environment -> Pattern Event -> Time -> ReceiverT EnvironmentUpdate () m ()
playerProcess handle = loop
    where
        loop !_envir !pattern !time = do
            (envir, _) <- applyUpdates _envir
            case Model.step envir pattern of
                Model.Done envir' -> do
                    _ <- logEnv envir'
                    io $ Log.debugM logger "playerProcess: Model.Done"
                    return ()
                Model.Result !envir' !event !pattern' -> do
                    sendTo handle $ Event_ time event
                    envir'' <- logEnv envir'
                    io $ Log.debugM logger $ "Event: " ++ show event
                    -- let (row, col) = Model.cursorPosition (event ^. Model.cursor)
                    --     cursor = Sequencer.Cursor row col
                    --     envir'' = Environment.sequencer ^: Sequencer.modifyCursor (const cursor) (Model.cursorId (event ^. Model.cursor)) $ envir'
                    let dt = event ^. Event.delta
                    if dt > 0
                        then do
                            sendTo handle $ Environment_ envir''
                            let time' = time + dt
                            io $ Time.pauseThreadUntil time'
                            loop envir'' pattern' time'
                        else loop envir'' pattern' time

startPlayerThread :: Handle -> Pattern Event -> Environment -> Time -> IO PlayerHandle
startPlayerThread handle pattern envir time = spawn $ playerProcess handle envir pattern time

stopPlayerThread :: PlayerHandle -> IO ()
stopPlayerThread = kill

initPatch :: Handle -> FeatureSpaceP.Handle -> State -> IO ()
initPatch h fspaceP state = do
    mapM_ (sendTo fspaceP . FeatureSpaceP.UpdateRegion) (Patch.regions (patch state))

    -- If specified in the config file, fill the whole sequencer in order to facilitate debugging.
    fill <- fmap (\conf -> either (const False) id $ Config.get conf "Sequencer" "debugFill") Config.getConfig
    when fill $ do
        forM_ [0..Sequencer.rows (Patch.sequencer (patch state))]$ \r ->
            forM_ [0..Sequencer.cols (Patch.sequencer (patch state))] $ \c ->
                sendTo h $ SetCell r c 1

    notifyListeners h (PatchChanged (patch state) (patchFilePath state))

new :: Patch.Patch -> FeatureSpaceP.Handle -> IO Handle
new patch0 fspaceP = do
    time <- io Time.utcr
    let state = State time patch0 Nothing Nothing BitSet.empty
    h <- spawn (loop state)
    initPatch h fspaceP state
    return h
    where
        updateSequencer state update = do
            case transport state of
                Stopped ->
                    return $ Just $ state { patch = Patch.modifySequencer update (patch state) }
                Running -> do
                    sendTo (fromJust (playerThread state)) (Environment.sequencer ^: update)
                    return Nothing
        loop !state = do
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
                                        fspace <- query fspaceP FeatureSpaceP.GetModel
                                        case Patch.pattern (patch state) of
                                            Left e -> do
                                                io $ Log.errorM logger (show e)
                                                return Nothing
                                            Right (pattern, bindings) -> do
                                                let env = Environment.mkEnvironment
                                                            0
                                                            bindings
                                                            fspace
                                                            (Patch.sequencer (patch state))
                                                tid  <- io $ startPlayerThread proc pattern env time
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
                                        fspace <- query fspaceP FeatureSpaceP.GetModel
                                        case Patch.pattern (patch state) of
                                            Left e -> do
                                                io $ Log.errorM logger (show e)
                                                return Nothing
                                            Right (pattern, bindings) -> do
                                                let patch' = Patch.modifySequencer Sequencer.resetCursors (patch state)
                                                    env = Environment.mkEnvironment
                                                            0
                                                            bindings
                                                            fspace
                                                            (Patch.sequencer patch')
                                                tid <- io $ startPlayerThread proc pattern env time
                                                return $ Just $ state { time = time
                                                                      , patch = patch'
                                                                      , playerThread = Just tid }
                    GetSequencer query -> do
                        answer query $ Patch.sequencer (patch state)
                        return Nothing
                    SetFeatureSpace fspace -> do
                        maybe (return ()) (\h -> sendTo h (setVal Environment.featureSpace fspace)) (playerThread state)
                        return Nothing
                    GetPatch query -> do
                        answer query (patch state, patchFilePath state)
                        return Nothing
                    LoadPatch path -> do
                        h <- self
                        io $ do { patch' <- Patch.load path
                                ; let state' = state { patch = patch'
                                                     , patchFilePath = Just path }
                                ; initPatch h fspaceP state'
                                ; notifyListeners h (PatchLoaded patch' path)
                                ; return $ Just state' }
                                `catches`
                                    [ Handler (\(e :: Patch.LoadError)   -> Log.errorM logger (show e) >> return Nothing)
                                    , Handler (\(e :: Comp.CompileError) -> Log.errorM logger (show e) >> return Nothing) ]
                    StorePatch path -> do
                        regions <- liftM FeatureSpace.regions $ query fspaceP FeatureSpaceP.GetModel
                        let patch' = Patch.setRegions regions (patch state)
                        h <- self
                        io $ do { Patch.store path patch'
                                 ; notifyListeners h (PatchStored patch' path) }
                                `catch`
                                (\(e :: IOException) -> Log.errorM logger (show e))
                        return $ Just state { patch = patch', patchFilePath = Just path }
                    SetSourceCode src -> do
                        return $ Just state { patch = Patch.setSourceCode src (patch state) }
                    RunPatch -> do
                        res <- io $ Patch.evalSourceCode (patch state)
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
                    Event_ time event ->
                        case transport state of
                            Running -> do
                                unless (BitSet.member (event ^. Event.cursor) (mutedTracks state))
                                       (notify $ Event time event)
                                return Nothing
                            _ -> error "This shouldn't happen: Received an Event_ message but player thread not running"
                    Environment_ envir ->
                        return $ Just state { patch = Patch.modifySequencer (const (envir ^. Environment.sequencer)) (patch state) }
            case state' of
                Just state' -> do
                    notify $ Changed (time state') (transport state') (Patch.sequencer (patch state'))
                    loop state'
                Nothing ->
                    loop state
