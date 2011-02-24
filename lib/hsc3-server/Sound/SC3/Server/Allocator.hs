{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Sound.SC3.Server.Allocator
    ( AllocFailure(..)
    , IdAllocator(..)
    , RangeAllocator(..)
    , module Sound.SC3.Server.Allocator.Range
    ) where

import Control.Exception (Exception)
import Control.Failure (Failure)
import Control.Monad (replicateM)
import qualified Control.Monad.Trans.Class as State
import qualified Control.Monad.Trans.State.Strict as State
import Data.Typeable (Typeable)
import Sound.SC3.Server.Allocator.Range

data AllocFailure =
    NoFreeIds
  | InvalidId
  deriving (Show, Typeable)

instance Exception AllocFailure

class IdAllocator i a where
    alloc :: Failure AllocFailure m => a -> m (i, a)
    free  :: Failure AllocFailure m => i -> a -> m a
    
    allocMany :: Failure AllocFailure m => Int -> a -> m ([i], a)
    allocMany n = State.runStateT (replicateM n (modifyM alloc))
        where
            modifyM f = do
                (a, s') <- State.get >>= State.lift . f
                State.put $! s'
                return a

class IdAllocator i a => RangeAllocator i a where
    allocRange :: Failure AllocFailure m => Int -> a -> m (Range i, a)
    freeRange  :: Failure AllocFailure m => Range i -> a -> m a
