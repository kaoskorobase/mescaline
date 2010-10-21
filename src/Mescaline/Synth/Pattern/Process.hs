{-# LANGUAGE BangPatterns #-}
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
import           Data.Typeable
import           Mescaline (Time)
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import qualified Mescaline.Synth.Pattern.Environment as Model
import qualified Mescaline.Synth.Pattern.Event as Model
import qualified Mescaline.Synth.Pattern.Patch as Model
import qualified Mescaline.Synth.Pattern.Player as Model
import qualified Mescaline.Synth.Pattern.Track as Model
import           Prelude hiding (catch)
import qualified Sound.OpenSoundControl.Time as Time

data TransportChange = Start | Pause | Reset deriving (Eq, Show)

data Input =
    SetCell         Model.TrackId Int Double
  | ClearCell       Model.TrackId Int
  | ClearTrack      Model.TrackId
  | ClearAll
  | Transport       TransportChange
  | GetModel        (Query Model.Track)
  | SetFeatureSpace FeatureSpace.FeatureSpace
  | Event_          Time Model.TrackId Model.Event

data Output =
    Changed     Time Model.TrackId TransportState
  | Event       Time Model.TrackId Model.Event

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State a = State {
    patch        :: Model.Patch
  , time         :: Time
  , playerThread :: Maybe PlayerHandle
  }

transport :: State a -> TransportState
transport = maybe Stopped (const Running) . playerThread

type EnvironmentUpdate = Model.Environment -> Model.Environment
type Player = Model.Player Model.Environment (Model.TrackId, Model.Event)
type PlayerHandle = Process.Handle EnvironmentUpdate ()

applyUpdates :: MonadIO m => Model.Environment -> ReceiverT EnvironmentUpdate () m Model.Environment
applyUpdates a = do
    x <- poll
    case x of
        Nothing -> return a
        Just f  -> let a' = f a in a' `seq` applyUpdates a'

playerProcess :: MonadIO m => Handle -> Model.Environment -> Player -> Time -> ReceiverT EnvironmentUpdate () m ()
playerProcess handle envir0 player0 time0 = loop envir0 player0 time0
    where
        loop !_envir !player !time = do
            envir <- applyUpdates _envir
            case Model.step envir player of
                Model.Done envir' -> do
                    io $ putStrLn "playerProcess: Model.Done"
                    loop envir' player0 time
                Model.Result envir' (track, event) player' delta -> do
                    -- io $ putStrLn $ "playerProcess event " ++ show event
                    sendTo handle $ Event_ time track event
                    case delta of
                        Nothing -> loop envir' player' time
                        Just dt -> do
                            let time' = time + dt
                            io $ Time.pauseThreadUntil time'
                            loop envir' player' time'

startPlayerThread :: Handle -> Player -> Model.Environment -> Time -> IO PlayerHandle
startPlayerThread handle patternPlayer envir time = spawn $ playerProcess handle envir patternPlayer time

stopPlayerThread :: PlayerHandle -> IO ()
stopPlayerThread = kill

new :: Model.Patch -> FeatureSpaceP.Handle -> IO Handle
new patch fspaceP = do
    t <- io Time.utcr
    let player = (Model.Player t (Model.toPattern patch))
    h <- spawn $ loop (State patch t Nothing) player
    sendTo h $ Transport Start
    return h
    where
        loop state player = do
            x <- recv
            state' <-
                case x of
                    Transport tc -> do
                        case transport state of
                            Stopped ->
                                case tc of
                                    Start -> do
                                        proc <- self
                                        time <- io Time.utcr
                                        fspace <- query fspaceP FeatureSpaceP.GetModel
                                        let env = Model.mkEnvironment 0 fspace
                                        tid  <- io $ startPlayerThread proc player env time
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
                                        let env = Model.mkEnvironment 0 fspace
                                        tid  <- io $ startPlayerThread proc player env time
                                        return $ Just $ state { time = time
                                                              , playerThread = Just tid }
                    GetModel query -> do
                        -- io $ putMVar mvar $ patch state
                        return Nothing
                    SetFeatureSpace fspace -> do
                        maybe (return ()) (\h -> sendTo h (setVal Model.featureSpace fspace)) (playerThread state)
                        return Nothing
                    Event_ time track event ->
                        case transport state of
                            Running -> do
                                notify $ Event time track event
                                return Nothing
                            _ -> error "This shouldn't happen: Received an Event_ message but player thread not running"
                    _ -> return Nothing
            case state' of
                Just state' -> do
                    -- notify $ Changed (time state') (patch state') (transport state')
                    loop state' player
                Nothing ->
                    loop state player
-- new = undefined