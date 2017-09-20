module Mescaline.Pattern (
    -- Pattern
    -- *Reader
    ask
  , local
  , asks
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
  -- , ptraceEnv
    -- *Base module
  , module Mescaline.Pattern.Base
) where

import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Control.Monad.Reader.Class (MonadReader(..), asks)
import           Control.Monad.State.Class (MonadState(..), modify, gets)
-- import           Mescaline.Pattern.Environment (Environment)
-- import qualified Mescaline.Pattern.Environment as Env
import           Mescaline.Pattern.Ppar
import           Mescaline.Pattern.Base
import           Debug.Trace

-- type Pattern = P Environment

runRand :: R.RandomGen s => P s (R.Rand s a) -> P s a
runRand = M.join . fmap (\r -> prp $ \s -> let (a, s') = R.runRand r s in (return a, s'))

ptrace :: Show a => String -> P s a -> P s a
ptrace tag = fmap (\a -> trace (tag ++ show a) a)

-- ptraceEnv :: Show a => String -> Pattern a -> Pattern a
-- ptraceEnv tag = punfoldr $ \s p ->
--     case step s p of
--         Done s' -> (s', Nothing)
--         Result s' a p' -> (Env.logMessage (tag ++ show a) s', Just (a, p'))
