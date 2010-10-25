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
import           Data.Maybe (fromJust)
import           Data.Typeable
import           Mescaline (Time)
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Process as FeatureSpaceP
import           Mescaline.Synth.Pattern.Environment (Environment)
import qualified Mescaline.Synth.Pattern.Environment as Environment
import qualified Mescaline.Synth.Pattern.Event as Model
import           Mescaline.Synth.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import qualified Mescaline.Synth.Pattern.Patch as Patch
import qualified Mescaline.Synth.Pattern.Player as Model
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
  | Event_          Time Model.Event
  | Environment_    Environment

data Output =
    Changed     Time TransportState Sequencer.Sequencer
  | Event       Time Model.Event

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State a = State {
    patch        :: Patch.Patch
  , sequencer    :: Sequencer.Sequencer
  , time         :: Time
  , playerThread :: Maybe PlayerHandle
  }

transport :: State a -> TransportState
transport = maybe Stopped (const Running) . playerThread

type EnvironmentUpdate = Environment -> Environment
type Player = Model.Player Environment Model.Event
type PlayerHandle = Process.Handle EnvironmentUpdate ()

applyUpdates :: MonadIO m => Environment -> ReceiverT EnvironmentUpdate () m (Environment, Bool)
applyUpdates a = loop (a, False)
    where
        loop (a, b) = do
            x <- poll
            case x of
                Nothing -> return (a, b)
                Just f  -> let a' = f a in a' `seq` loop (a', True)

playerProcess :: MonadIO m => Handle -> Environment -> Player -> Time -> ReceiverT EnvironmentUpdate () m ()
playerProcess handle envir0 player0 time0 = loop envir0 player0 time0
    where
        loop !_envir !player !time = do
            (envir, _) <- applyUpdates _envir
            case Model.step envir player of
                Model.Done envir' -> do
                    io $ putStrLn "playerProcess: Model.Done"
                    -- loop envir' player0 time
                    return ()
                Model.Result envir' event player' delta -> do
                    -- TODO: Write this to log
                    -- io $ print event
                    sendTo handle $ Event_ time event
                    case delta of
                        Nothing -> loop envir' player' time
                        Just dt -> do
                            let time' = time + dt
                            sendTo handle $ Environment_ envir'
                            -- io $ print $ map (Sequencer.column.snd) $ take 4 $ Sequencer.cursors $ envir' ^. Environment.sequencer
                            io $ Time.pauseThreadUntil time'
                            loop envir' player' time'

startPlayerThread :: Handle -> Player -> Environment -> Time -> IO PlayerHandle
startPlayerThread handle patternPlayer envir time = spawn $ playerProcess handle envir patternPlayer time

stopPlayerThread :: PlayerHandle -> IO ()
stopPlayerThread = kill

new :: Patch.Patch -> FeatureSpaceP.Handle -> IO Handle
new patch fspaceP = do
    t <- io Time.utcr
    let player = Model.Player t (Patch.pattern patch)
        sequencer = Sequencer.cons 8 8 (map (\i -> (i, Sequencer.Cursor i 0)) [0..7])
    spawn $ loop (State patch sequencer t Nothing) player
    where
        updateSequencer state update = do
            case transport state of
                Stopped ->
                    return $ Just $ state { sequencer = update (sequencer state) }
                Running -> do
                    sendTo (fromJust (playerThread state)) (Environment.sequencer ^: update)
                    return Nothing
        loop state player = do
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
                                        let env = Environment.mkEnvironment 0 fspace (sequencer state)
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
                                        let env = Environment.mkEnvironment 0 fspace (sequencer state)
                                        tid <- io $ startPlayerThread proc player env time
                                        return $ Just $ state { time = time
                                                              , playerThread = Just tid }
                    GetSequencer query -> do
                        answer query $ sequencer state
                        return Nothing
                    SetFeatureSpace fspace -> do
                        maybe (return ()) (\h -> sendTo h (setVal Environment.featureSpace fspace)) (playerThread state)
                        return Nothing
                    Event_ time event ->
                        case transport state of
                            Running -> do
                                notify $ Event time event
                                return Nothing
                            _ -> error "This shouldn't happen: Received an Event_ message but player thread not running"
                    Environment_ envir ->
                        return $ Just $ state { sequencer = envir ^. Environment.sequencer }
            case state' of
                Just state' -> do
                    notify $ Changed (time state') (transport state') (sequencer state')
                    loop state' player
                Nothing ->
                    loop state player
