{-# LANGUAGE BangPatterns #-}
module Mescaline.Pattern.Player (
    Process
  , start
  , stop
  , send
  , setSlot
  , setTempo
  , events
) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Lens ((^.))
import qualified Data.IntPSQ as PQ
import           Mescaline.Clock (Clock, beats, elapsed, logical)
import qualified Mescaline.Clock as Clock
import           Mescaline.Time (Beats, Seconds, HasDelta(..))
import           Mescaline.Pattern (Event, Pattern)
import qualified Mescaline.Pattern as P
import           Mescaline.Quant (Quant, quantize)
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified Sound.OSC.Time as OSC

currentTime :: IO Seconds
currentTime = realToFrac <$> OSC.time

type Scheduler = PQ.IntPSQ Beats [Event]

data Player = Player {
    clock    :: !Clock
  , patterns :: !Scheduler
  , produceEvent :: Clock.Time -> Event -> IO ()
  }

data Command =
    StartTransport
  | StopTransport
  | SetSlot Int Quant (Maybe (Pattern Event))
  | SetTempo Double
  deriving (Eq, Show)

data Process = Process {
    handle :: Async ()
  , channel :: TMQueue Command
  , eventSource :: R.AddHandler (Clock.Time, Event)
  }

next :: Clock -> Scheduler -> (Scheduler, Maybe Event)
next _ pq =
  case PQ.minView pq of
    Nothing -> (pq, Nothing)
    Just (i, nextBeat, p, pq') ->
      case p of
        [] -> (pq', Nothing)
        (e:p') ->
            let pq'' = PQ.insert i (nextBeat + realToFrac (e ^. delta)) p' pq'
            in (pq'', Just e)

-- registerDelayUntil :: Clock -> Seconds -> IO (TVar Bool)
-- registerDelayUntil c s = do
--   let dt = s - seconds (elapsed c)
--   print c
--   print dt
--   if dt > 0
--     then registerDelay (floor (dt * 1e6))
--     else newTVarIO True

readTMQueueWithTimeout :: b -> Int -> TMQueue a -> IO (Either b (Maybe a))
readTMQueueWithTimeout b usec queue = do
  delay <- if usec > 0 then registerDelay usec else newTVarIO True
  let wait = readTVar delay >>= \done -> if done then return (Left b) else retry
  atomically $ Right <$> readTMQueue queue <|> wait

handleCommand :: Command -> Player -> Player
handleCommand (SetSlot i q (Just p)) state =
  let t = quantize 4 q . beats . elapsed . clock $ state
  in state { patterns = PQ.insert i t (P.unP p) (patterns state) }
handleCommand (SetTempo t) state =
  state { clock = Clock.setTempo (Clock.fromBps t) (clock state) }
handleCommand _ state = state

loop :: TMQueue Command -> Player -> IO ()
loop commands !state = do
  let clk = clock state
      pq = patterns state
  -- Get next input event
  evt <- case PQ.findMin pq of
          Nothing -> Right <$> atomically (readTMQueue commands)
          Just (_, b, _) -> do
            dt <- (Clock.beatsToSeconds clk b -) <$> currentTime
            readTMQueueWithTimeout b (floor (dt * 1e6)) commands
  -- Update clock
  clk' <- Clock.setElapsed <$> currentTime <*> pure clk
  -- Dispatch event
  case evt of
    Left b' -> do
      let clk'' = Clock.setLogical b' clk'
          (pq', e) = next clk'' pq
          state' = state { clock = clk''
                         , patterns = pq' }
      maybe (return ()) (produceEvent state (logical clk'')) e
      loop commands state'
    Right (Just i) -> do
      let state' = state { clock = clk' }
      loop commands (handleCommand i state')
    Right Nothing ->
      return ()

start :: IO Process
start = do
  commands <- newTMQueueIO
  (addHandler, fire) <- R.newAddHandler
  state <- Player <$> (Clock.mkClock (Clock.fromBps 1) <$> currentTime)
                  <*> pure PQ.empty
                  <*> pure (curry fire)
  Process <$> Async.asyncBound (loop commands state) <*> pure commands <*> pure addHandler

stop :: Process -> IO ()
stop p = atomically $ do
  closeTMQueue (channel p)
  Async.waitSTM (handle p)

send :: Process -> Command -> IO ()
send p = atomically . writeTMQueue (channel p)

setSlot :: Process -> Int -> Quant -> Maybe (Pattern Event) -> IO ()
setSlot player i q p = send player (SetSlot i q p)

setTempo :: Process -> Double -> IO ()
setTempo p t = send p (SetTempo t)

events :: Process -> R.MomentIO (R.Event (Clock.Time, Event))
events = R.fromAddHandler . eventSource

