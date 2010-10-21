module Mescaline.Synth.Pattern (
    Pattern
  , ask, asks
  , runRand
  , module Mescaline.Synth.Pattern.Ppar
  , module Mescaline.Synth.Pattern.Base
) where

import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Data.Accessor
import           Mescaline.Synth.Pattern.Environment (Environment)
import           Mescaline.Synth.Pattern.Ppar
import           Mescaline.Synth.Pattern.Base

type Pattern = P Environment

ask :: P s s
ask = prp $ \s -> (pcons s ask, s)

asks :: Accessor s a -> P s a
asks acc = fmap (getVal acc) ask

runRand :: R.RandomGen s => P s (R.Rand s a) -> P s a
runRand = M.join . fmap (\r -> prp $ \s -> let (a, s') = R.runRand r s in (return a, s'))
