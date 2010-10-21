module Mescaline.Synth.Pattern.Player (
    Player(..)
  , Result(..)
  , step
) where

import           Data.Accessor
import           Mescaline.Time
import qualified Mescaline.Synth.Pattern.Base as P

data Player s a = Player {
    time    :: Time
  , pattern :: P.P s a
  }

data Result s a = Result s a (Player s a) (Maybe Duration) | Done s

-- | Advance player.
step :: HasDuration a => s -> Player s a -> Result s a
step s p =
    case P.step s (pattern p) of
        P.Done s'        -> Done s'
        P.Result s' a p' -> let dur = getVal duration a
                            in Result s' a
                                      (p { time = time p + dur, pattern = p' })
                                      (if dur > 0 then Just dur else Nothing)
