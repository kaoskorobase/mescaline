module Mescaline.Synth.Sequencer.Process (
    Sequencer
  , TransportState(..)
  , TransportChange(..)
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Control.Monad.Reader
import           Data.Accessor
import           Mescaline (Time)
import qualified Mescaline.Synth.Sequencer.Model as Model
import qualified Sound.OpenSoundControl.Time as Time

data TransportChange = Start | Pause | Reset deriving (Eq, Show)

data Input a =
    ToggleField !Int !Int !a
  | ClearAll
  | Transport   !TransportChange
  | QueryModel  (MVar (Model.Sequencer a))
  | Tick_       !Time

data Output a =
    Changed Time (Model.Sequencer a) TransportState

type Sequencer a = Handle (Input a) (Output a)

data TransportState = Stopped | Running deriving (Eq, Show)

data State a = State {
    model      :: Model.Sequencer a
  , time       :: Time
  , tickThread :: Maybe ThreadId
  }

transport :: State a -> TransportState
transport = maybe Stopped (const Running) . tickThread

algorithm :: Model.Score
algorithm = undefined

tickLoop :: Sequencer a -> Double -> Time -> IO ()
tickLoop sequencer tick time = do
    sendTo sequencer $ Tick_ time
    let t' = time + tick
    Time.pauseThreadUntil t'
    tickLoop sequencer tick t'

startTickThread :: Sequencer a -> Double -> Time -> IO ThreadId
startTickThread sequencer tick time = forkIO $ tickLoop sequencer tick time
    
stopTickThread :: Maybe ThreadId -> IO ()
stopTickThread (Just tid) = killThread tid
stopTickThread Nothing    = return ()

new :: Model.Sequencer a -> IO (Sequencer a)
new s0 = do
    t <- io Time.utcr
    spawn $ loop (State s0 t Nothing)
    where
        loop state = do
            x <- recv
            state' <-
                case x of
                    ToggleField r c a ->
                        return $ Just $ state { model = Model.toggle r c a (model state) }
                    ClearAll ->
                        return $ Just $ state { model = Model.deleteAll (model state) }
                    Transport tc -> do
                        case transport state of
                            Stopped ->
                                case tc of
                                    Start -> do
                                        proc <- self
                                        time <- io Time.utcr
                                        tid  <- io $ startTickThread proc (getVal Model.tick (model state)) time
                                        return $ Just $ state { time = time
                                                              , tickThread = Just tid }
                                    Pause -> return Nothing
                                    Reset -> return $ Just $ state { model = Model.reset algorithm (model state) }
                            Running ->
                                case tc of
                                    Start -> return Nothing
                                    Pause -> do
                                        io $ stopTickThread (tickThread state)
                                        return $ Just $ state { tickThread = Nothing }
                                    Reset -> do
                                        io $ stopTickThread (tickThread state)
                                        proc <- self
                                        time <- io Time.utcr
                                        tid  <- io $ startTickThread proc (getVal Model.tick (model state)) time
                                        return $ Just $ state { model = Model.reset algorithm (model state)
                                                              , time = time
                                                              , tickThread = Just tid }
                    QueryModel mvar -> do
                        io $ putMVar mvar $ model state
                        return Nothing
                    Tick_ time' ->
                        case transport state of
                            Running -> do
                                let s' = Model.step algorithm (model state)
                                return $ Just $ state { model = s', time = time' }
                            _ -> error "This shouldn't happen: Received a Tick_ message but Tick thread not running"
            case state' of
                Just state' -> do
                    notify $ Changed (time state') (model state') (transport state')
                    loop state'
                Nothing ->
                    loop state
