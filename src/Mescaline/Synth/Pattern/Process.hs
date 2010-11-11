{-# LANGUAGE BangPatterns
           , ScopedTypeVariables #-}
module Mescaline.Synth.Pattern.Process (
    Handle
  , TransportState(..)
  , TransportChange(..)
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (MonadIO)
import           Control.Monad.Fix (fix)
-- import           Control.Monad.Reader
import           Data.Accessor
import           Data.Maybe (fromJust)
import           Data.Typeable
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import           Mescaline.Synth.Pattern.Environment (Environment)
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (Event)
import qualified Mescaline.Synth.Pattern.Event as Model
import qualified Mescaline.Synth.Pattern.Compiler as Comp
import           Mescaline.Synth.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import           Mescaline.Synth.Pattern (Pattern)
import qualified Mescaline.Synth.Pattern as Model
import qualified Mescaline.Synth.Pattern.Patch as Patch
import qualified Mescaline.Synth.Pattern.Patch.Version_0_0_1 as Patch
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
  | LoadPatch       FilePath
  | StorePatch      FilePath
  | SetSourceCode   String
  | RunPatch
  | Event_          Time Model.Event
  | Environment_    Environment

data Output =
    Changed      Time TransportState Sequencer.Sequencer
  | Event        Time Model.Event
  | PatchChanged Patch.Patch

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State = State {
    time         :: Time
  , patch        :: Patch.Patch
  , playerThread :: Maybe PlayerHandle
  }

transport :: State -> TransportState
transport = maybe Stopped (const Running) . playerThread

type EnvironmentUpdate = Environment -> Environment
-- type Player = Model.Player Environment Model.Event
type PlayerHandle = Process.Handle EnvironmentUpdate ()

applyUpdates :: MonadIO m => Environment -> ReceiverT EnvironmentUpdate () m (Environment, Bool)
applyUpdates a = loop (a, False)
    where
        loop (a, b) = do
            x <- poll
            case x of
                Nothing -> return (a, b)
                Just f  -> let a' = f a in a' `seq` loop (a', True)

playerProcess :: MonadIO m => Handle -> Environment -> Pattern Event -> Time -> ReceiverT EnvironmentUpdate () m ()
playerProcess handle = loop
    where
        loop !_envir !pattern !time = do
            (envir, _) <- applyUpdates _envir
            case Model.step envir pattern of
                Model.Done _ -> do
                    io $ Log.debugM "Sequencer" "playerProcess: Model.Done"
                    return ()
                Model.Result !envir' !event !pattern' -> do
                    io $ Log.debugM "Sequencer" $ "Event: " ++ show event
                    sendTo handle $ Event_ time event
                    -- let (row, col) = Model.cursorPosition (event ^. Model.cursor)
                    --     cursor = Sequencer.Cursor row col
                    --     envir'' = Environment.sequencer ^: Sequencer.modifyCursor (const cursor) (Model.cursorId (event ^. Model.cursor)) $ envir'
                    let dt = event ^. Model.delta
                        envir'' = envir'
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

initPatch :: Handle -> FeatureSpaceP.Handle -> Patch.Patch -> IO ()
initPatch h fspaceP patch = do
    mapM_ (sendTo fspaceP . FeatureSpaceP.UpdateRegion) (Patch.regions patch)

    -- If specified in the config file, fill the whole sequencer in order to facilitate debugging.
    fill <- fmap (\conf -> either (const False) id $ Config.get conf "Sequencer" "debugFill") Config.getConfig
    when fill $ do
        forM_ [0..Sequencer.rows (Patch.sequencer patch)]$ \r ->
            forM_ [0..Sequencer.cols (Patch.sequencer patch)] $ \c ->
                sendTo h $ SetCell r c 1

    notifyListeners h (PatchChanged patch)

new :: Patch.Patch -> FeatureSpaceP.Handle -> IO Handle
new patch0 fspaceP = do
    time <- io Time.utcr
    h <- spawn $ loop (State time patch0 Nothing)
    initPatch h fspaceP patch0
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
                                                io $ print e
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
                                    Reset -> return Nothing
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
                                                io $ print e
                                                return Nothing
                                            Right (pattern, bindings) -> do
                                                let env = Environment.mkEnvironment
                                                            0
                                                            bindings
                                                            fspace
                                                            (Patch.sequencer (patch state))
                                                tid <- io $ startPlayerThread proc pattern env time
                                                return $ Just $ state { time = time
                                                                      , playerThread = Just tid }
                    GetSequencer query -> do
                        answer query $ Patch.sequencer (patch state)
                        return Nothing
                    SetFeatureSpace fspace -> do
                        maybe (return ()) (\h -> sendTo h (setVal Environment.featureSpace fspace)) (playerThread state)
                        return Nothing
                    LoadPatch path -> do
                        h <- self
                        io $ do { patch' <- Patch.load path
                                ; initPatch h fspaceP patch' 
                                ; return $ Just $ state { patch = patch' } }
                                `catches`
                                    [ Handler (\(e :: Patch.LoadError) -> print e >> return Nothing)
                                    , Handler (\(e :: Comp.CompileError) -> print e >> return Nothing) ]
                    StorePatch path -> do
                        io $ (Patch.store path (patch state))
                                `catch`
                                (\(e :: IOException) -> print e)
                        return Nothing
                    SetSourceCode src -> do
                        res <- io $ Patch.setCode src (patch state)
                        case res of
                            Left e -> io (putStrLn e) >> return Nothing
                            Right patch' -> do { notify $ PatchChanged patch'
                                               ; return $ Just state { patch = patch' } }
                    RunPatch -> do
                        flip sendTo (Transport Pause) =<< self
                        flip sendTo (Transport Start) =<< self
                        return Nothing
                    Event_ time event ->
                        case transport state of
                            Running -> do
                                notify $ Event time event
                                return Nothing
                            _ -> error "This shouldn't happen: Received an Event_ message but player thread not running"
                    Environment_ envir ->
                        return $ Just $ state { patch = Patch.modifySequencer (const (envir ^. Environment.sequencer)) (patch state) }
            case state' of
                Just state' -> do
                    notify $ Changed (time state') (transport state') (Patch.sequencer (patch state'))
                    loop state'
                Nothing ->
                    loop state
