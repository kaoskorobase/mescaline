module Mescaline.Clock (
    Tempo
  , fromBps
  , Time(..)
  , Clock
  , mkClock
  , tempo
  , elapsed
  , logical
  , beatsToSeconds
  , secondsToBeats
  , setTempo
  , setElapsed
  , setLogical
) where

import Mescaline.Time (Beats(..), Seconds(..))

data Tempo = Tempo {
    beatsPerSecond :: Double
  , secondsPerBeat :: Double
  } deriving (Eq, Show)

fromBps :: Double -> Tempo
fromBps bps = Tempo bps (recip bps)

data Time = Time {
    seconds :: !Seconds
  , beats :: !Beats
  } deriving (Eq, Show)

data Clock = Clock {
    tempo :: !Tempo
  , base :: !Time
  , elapsed :: !Time
  , logical :: !Time
  } deriving (Eq, Show)

mkClock :: Tempo -> Seconds -> Clock
mkClock t (Seconds s0) = Clock t t0 t0 t0
  where t0 = Time (Seconds s0) (Beats s0)

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

setTempo :: Tempo -> Clock -> Clock
setTempo t c =
  let b0 = beats (logical c)
  in c {
      base = Time (beatsToSeconds c b0) b0
    , tempo = t
    }


