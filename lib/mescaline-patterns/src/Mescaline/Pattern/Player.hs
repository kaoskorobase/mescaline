{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mescaline.Pattern.Player (
    Process
  , start
  , stop
  , send
  , assign
  , events
) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Lens ((^.))
import           Control.Monad (unless)
import           Data.Default (Default(..))
import qualified Data.OrdPSQ as PQ
import           Mescaline.Time (HasDelta(..))
import           Mescaline.Pattern (Event, Pattern)
import qualified Mescaline.Pattern as P
import qualified Reactive.Banana as R
import qualified Reactive.Banana.Frameworks as R
import qualified Sound.OSC.Time as OSC

newtype Seconds = Seconds Double deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)
newtype Beats = Beats Double deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)

currentTime :: IO Seconds
currentTime = Seconds <$> OSC.time

data Time = Time {
    seconds :: !Seconds
  , beats :: !Beats
  } deriving (Eq, Show)

data Tempo = Tempo {
    beatsPerSecond :: Double
  , secondsPerBeat :: Double
  } deriving (Eq, Show)

mkTempo :: Double -> Tempo
mkTempo bps = Tempo bps (recip bps)

data Clock = Clock {
    tempo :: !Tempo
  , base :: !Time
  , elapsed :: !Time
  , logical :: !Time
  } deriving (Eq, Show)

mkClock :: Double -> Seconds -> Clock
mkClock bps s@(Seconds t) = Clock (mkTempo bps) base base base
  where base = Time s (Beats t)

beatsToSeconds :: Clock -> Beats -> Seconds
beatsToSeconds c b = seconds (base c) + Seconds (b' * secondsPerBeat (tempo c))
  where (Beats b') = b - beats (base c)

secondsToBeats :: Clock -> Seconds -> Beats
secondsToBeats c s = beats (base c) + Beats (s' * beatsPerSecond (tempo c))
  where (Seconds s') = s - seconds (base c)

setLogical :: Beats -> Clock -> Clock
setLogical b c = c { logical = Time (beatsToSeconds c b) b }

setElapsed :: Seconds -> Clock -> Clock
setElapsed s c = c { elapsed = Time s (secondsToBeats c s) }

type Scheduler e = PQ.OrdPSQ Int Beats [e]

data Player e = Player {
    clock    :: !Clock
  , patterns :: !(Scheduler e)
  , produceEvent :: Time -> Event -> IO ()
  }

data Quant = Quant {
    quant :: Double
  , phase :: Double
  } deriving (Eq, Show)

instance Default Quant where
  def = Quant 0 0

data Input =
    Slot Int Quant (Maybe (Pattern Event))
  | StartTransport
  | StopTransport
  | SetTempo Double
  deriving (Eq, Show)

data Output =
    Realize Event
  deriving (Eq, Show)

data Process = Process {
    handle :: Async ()
  , channel :: TMQueue Input
  , eventSource :: R.AddHandler (Time, Event)
  }

next :: HasDelta e => Clock -> Scheduler e -> (Scheduler e, Maybe e)
next c pq =
  case PQ.minView pq of
    Nothing -> (pq, Nothing)
    Just (i, nextBeat, p, pq') ->
      case p of
        [] -> (pq', Nothing)
        (e:p') ->
            let pq'' = PQ.insert i (nextBeat + Beats (e ^. delta)) p' pq'
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

handleInput :: Input -> Player P.Event -> Player P.Event
handleInput (Slot i q (Just p)) state =
  -- TODO: Quantization
  let t = beats . elapsed . clock $ state
  in state { patterns = PQ.insert i t (P.unPE p) (patterns state) }
handleInput _ state = state

loop :: TMQueue Input -> Player Event -> IO ()
loop commands !state = do
  let clk = clock state
      pq = patterns state
  -- Get next input event
  evt <- case PQ.findMin pq of
          Nothing -> Right <$> atomically (readTMQueue commands)
          Just (_, b, _) -> do
            dt <- (beatsToSeconds clk b -) <$> currentTime
            readTMQueueWithTimeout b (floor (dt * 1e6)) commands
  -- Update clock
  clk' <- setElapsed <$> currentTime <*> pure clk
  -- Dispatch event
  case evt of
    Left b' -> do
      let clk'' = setLogical b' clk'
          (pq', e) = next clk'' pq
          state' = state { clock = clk''
                         , patterns = pq' }
      maybe (return ()) (produceEvent state (logical clk'')) e
      loop commands state'
    Right (Just i) -> do
      let state' = state { clock = clk' }
      loop commands (handleInput i state')
    Right Nothing ->
      return ()

start :: IO Process
start = do
  commands <- newTMQueueIO
  (addHandler, fire) <- R.newAddHandler
  state <- Player <$> (mkClock 1 <$> currentTime) <*> pure PQ.empty <*> pure (curry fire)
  Process <$> asyncBound (loop commands state) <*> pure commands <*> pure addHandler

stop :: Process -> IO ()
stop p = atomically $ do
  closeTMQueue (channel p)
  waitSTM (handle p)

send :: Process -> Input -> IO ()
send p = atomically . writeTMQueue (channel p)

assign :: Process -> Int -> Quant -> Maybe (Pattern Event) -> IO ()
assign player i q p = send player (Slot i q p)

events :: Process -> R.MomentIO (R.Event (Time, Event))
events = R.fromAddHandler . eventSource

