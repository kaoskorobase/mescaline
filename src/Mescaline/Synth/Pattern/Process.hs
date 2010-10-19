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
import           Control.Concurrent.MVar
import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix (fix)
-- import           Control.Monad.Reader
import           Data.Accessor
import           Data.Typeable
import           Mescaline (Time)
import qualified Mescaline.Synth.Pattern.Patch as Model
import qualified Mescaline.Synth.Pattern.Player as Model
import qualified Mescaline.Synth.Pattern.Track as Model
import           Prelude hiding (catch)
import qualified Sound.OpenSoundControl.Time as Time

data TransportChange = Start | Pause | Reset deriving (Eq, Show)

data Input =
    SetCell     !Model.TrackId !Int !Double
  | ClearCell   !Model.TrackId !Int
  | ClearTrack  !Model.TrackId
  | ClearAll
  | Transport   !TransportChange
  | QueryModel  (MVar Model.Track)
  | Event_      !Time !Model.TrackId !Model.Event

data Output =
    Changed     Time Model.TrackId TransportState
  | Event       Time Model.TrackId Model.Event

type Handle = Process.Handle Input Output

data TransportState = Stopped | Running deriving (Eq, Show)

data State a = State {
    patch        :: Model.Patch
  , time         :: Time
  , playerThread :: Maybe ThreadId
  }

transport :: State a -> TransportState
transport = maybe Stopped (const Running) . playerThread

type Player = Process.Handle () ()

playerLoop :: Handle -> Model.Environment -> Model.Player Model.Environment (Model.TrackId, Model.Event) -> Time -> IO ()
playerLoop handle envir0 player0 time0 = loop envir0 player0 time0
    where
        loop !envir !player !time = do
            case Model.step envir player of
                Model.Done envir' ->
                    loop envir' player0 time
                Model.Result envir' (track, event) player' delta -> do
                    sendTo handle $ Event_ time track event
                    case delta of
                        Nothing -> loop envir' player' time
                        Just dt -> do
                            let time' = time + dt
                            Time.pauseThreadUntil time'
                            loop envir' player' time'

startPlayerThread :: Handle -> Model.Player Model.Environment (Model.TrackId, Model.Event) -> Time -> IO ThreadId
startPlayerThread handle patternPlayer time = forkIO $ playerLoop handle Model.Environment patternPlayer time

stopPlayerThread :: ThreadId -> IO ()
stopPlayerThread = killThread

new :: Model.Patch -> IO Handle
new patch = do
    t <- io Time.utcr
    let ppVar = (Model.Player t (Model.toPattern patch))
    h <- spawn $ loop (State patch t Nothing) ppVar
    sendTo h $ Transport Start
    return h
    where
        loop state ppVar = do
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
                                        tid  <- io $ startPlayerThread proc ppVar time
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
                                        tid  <- io $ startPlayerThread proc ppVar time
                                        return $ Just $ state { time = time
                                                              , playerThread = Just tid }
                    QueryModel mvar -> do
                        -- io $ putMVar mvar $ patch state
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
                    loop state' ppVar
                Nothing ->
                    loop state ppVar
