module Mescaline.Synth.Sequencer.Process (
    Sequencer
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
  | Tick_

data Output a =
    Changed Time (Model.Sequencer a)

type Sequencer a = Handle (Input a) (Output a)

data TransportState = Stopped | Running deriving (Eq, Show)

data State a = State {
    model     :: Model.Sequencer a
  , time      :: Time
  , transport :: TransportState
  }

algorithm :: Model.Score
algorithm = undefined

new :: Model.Sequencer a -> IO (Sequencer a)
new s0 = do
    t0 <- io Time.utcr
    h <- spawn $ loop (State s0 t0 Stopped)
    sendTo h Tick_
    return h
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
                        io $ print ["Transport", show tc, show $ transport state]
                        case transport state of
                            Stopped ->
                                case tc of
                                    Start -> return $ Just $ state { transport = Running }
                                    Pause -> return Nothing
                                    Reset -> return $ Just $ state { model = Model.reset algorithm (model state) }
                            Running ->
                                case tc of
                                    Start -> return Nothing
                                    Pause -> return $ Just $ state { transport = Stopped }
                                    Reset -> return $ Just $ state { model = Model.reset algorithm (model state) }
                    QueryModel mvar -> do
                        io $ putMVar mvar $ model state
                        return Nothing
                    Tick_ ->
                        case transport state of
                            Stopped -> do
                                let t' = time state + getVal Model.tick (model state)
                                h <- self
                                io $ forkIO $ do
                                    Time.pauseThreadUntil t'
                                    sendTo h Tick_
                                return $ Just $ state { time = t' }
                            Running -> do
                                let s' = Model.step algorithm (model state)
                                    t' = time state + getVal Model.tick (model state)
                                h <- self
                                io $ forkIO $ do
                                    Time.pauseThreadUntil t'
                                    sendTo h Tick_
                                return $ Just $ state { model = s', time = t' }
            maybe (return ()) (notify . Changed (time state) . model) state'
            maybe (loop state) loop state'
