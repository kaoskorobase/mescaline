{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern (
    Pattern
    -- *Reader
  , ask
  , local
  , asks
  , askA
    -- *State
  , get
  , put
  , modify
  , gets
    -- *MonadRandom
  , runRand
    -- *Patterns
  , module Mescaline.Synth.Pattern.Ppar
  , ptrace
    -- *Base module
  , module Mescaline.Synth.Pattern.Base
) where

import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Control.Monad.Reader (MonadReader(..), asks)
import           Control.Monad.State (MonadState(..), modify, gets)
import           Data.Accessor
import           Mescaline.Synth.Pattern.Environment (Environment)
import           Mescaline.Synth.Pattern.Ppar
import           Mescaline.Synth.Pattern.Base
import           Debug.Trace

type Pattern = P Environment

instance MonadReader s (P s) where
    ask       = prp $ \s -> (pcons s ask, s)
    -- FIXME: Is this correct?!
    local f p = prp $ \s -> (p, f s)

askA :: Accessor s a -> P s a
askA acc = asks (getVal acc)

instance MonadState s (P s) where
    get   = ask
    put s = prp $ const (return (), s)

runRand :: R.RandomGen s => P s (R.Rand s a) -> P s a
runRand = M.join . fmap (\r -> prp $ \s -> let (a, s') = R.runRand r s in (return a, s'))

ptrace :: Show a => P s a -> P s a
ptrace = fmap (\a -> traceShow a a)
