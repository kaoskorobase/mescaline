module Mescaline.Synth.Pattern.Player (
    Player(..)
  , Result(..)
  , step
) where

import           Data.Accessor
import           Mescaline.Time (Duration, Time, HasDelta(..))
import qualified Mescaline.Synth.Pattern.Base as P

data Player s a = Player {
    time    :: !Time
  , pattern :: !(P.P s a)
  }

data Result s a = Result s a (Player s a) (Maybe Duration) | Done s

-- | Advance player.
step :: HasDelta a => s -> Player s a -> Result s a
step s player =
    case P.step s (pattern player) of
        P.Done s'        -> Done s'
        P.Result s' a p' -> let dt      = a ^. delta
                                player' = player { time = time player + dt, pattern = p' }
                            in dt `seq` player' `seq` Result s' a player' (if dt > 0 then Just dt else Nothing)
