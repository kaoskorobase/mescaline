{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Pattern (
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
  , module Mescaline.Pattern.Ppar
  , ptrace
  , ptraceEnv
    -- *Base module
  , module Mescaline.Pattern.Base
) where

import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Control.Monad.Reader (MonadReader(..), asks)
import           Control.Monad.State (MonadState(..), modify, gets)
import           Data.Accessor
import           Mescaline.Pattern.Environment (Environment)
import qualified Mescaline.Pattern.Environment as Env
import           Mescaline.Pattern.Ppar
import           Mescaline.Pattern.Base
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

ptrace :: Show a => String -> P s a -> P s a
ptrace tag = fmap (\a -> trace (tag ++ show a) a)

ptraceEnv :: Show a => String -> Pattern a -> Pattern a
ptraceEnv tag = punfoldr $ \s p ->
    case step s p of
        Done s' -> (s', Nothing)
        Result s' a p' -> (Env.logMessage (tag ++ show a) s', Just (a, p'))
