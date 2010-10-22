module Mescaline.Synth.Pattern.Player (
    Player(..)
  , Result(..)
  , step
) where

import           Data.Accessor
import           Mescaline.Time
import qualified Mescaline.Synth.Pattern.Base as P

data Player s a = Player {
    time    :: !Time
  , pattern :: !(P.P s a)
  }

data Result s a = Result s a (Player s a) (Maybe Duration) | Done s

-- | Advance player.
step :: HasDuration a => s -> Player s a -> Result s a
step s player =
    case P.step s (pattern player) of
        P.Done s'        -> Done s'
        P.Result s' a p' -> let dur = a ^. duration
                                player' = player { time = time player + dur, pattern = p' }
                            in dur `seq` player' `seq` Result s' a player' (if dur > 0 then Just dur else Nothing)
