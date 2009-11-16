{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Sound.SC3.Server.Allocator (
    IdAllocator(..)
  , RangeAllocator(..)
  , module Sound.SC3.Server.Allocator.Range
) where

import Control.Monad.Error (MonadError)
import Control.Monad.State.Strict (MonadState, get, put, runStateT)
import Sound.SC3.Server.Allocator.Range

class IdAllocator i a where
    alloc :: MonadError String m => a -> m (i, a)
    free  :: MonadError String m => i -> a -> m a
    
    allocMany :: MonadError String m => Int -> a -> m ([i], a)
    allocMany n = runStateT (sequence (replicate n allocM))

class IdAllocator i a => RangeAllocator i a where
    allocRange :: MonadError String m => Int -> a -> m (Range i, a)
    freeRange  :: MonadError String m => Range i -> a -> m a

modifyM :: (Monad m, MonadState s m) => (s -> m (a, s)) -> m a
modifyM f = do
    (a, s') <- get >>= f
    put $! s'
    return a

allocM :: (IdAllocator i a, MonadError String m, MonadState a m) => m i
allocM = modifyM alloc

allocManyM :: (IdAllocator i a, MonadError String m, MonadState a m) => Int -> m [i]
allocManyM n = modifyM (allocMany n)

allocRangeM :: (RangeAllocator i a, MonadError String m, MonadState a m) => Int -> m (Range i)
allocRangeM n = modifyM (allocRange n)
