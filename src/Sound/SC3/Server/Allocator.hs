{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses #-}

module Sound.SC3.Server.Allocator where

import Control.Monad.State (MonadState, get, put, runState)

class IdAllocator i a where
    alloc :: Monad m => a -> m (i, a)
    
    allocMany :: Monad m => Int -> a -> m ([i], a)
    allocMany = allocConsecutive
    
    allocConsecutive :: Monad m => Int -> a -> m ([i], a)
    allocConsecutive n = return . runState (mapM (const allocM) [1..n])

modifyM :: (Monad m, MonadState s m) => (s -> m (a, s)) -> m a
modifyM f = do
    (a, s') <- get >>= f
    put s'
    return a

allocM :: (IdAllocator i a, MonadState a m) => m i
allocM = modifyM alloc

allocManyM :: (IdAllocator i a, MonadState a m) => Int -> m [i]
allocManyM n = modifyM (allocMany n)

allocConsecutiveM :: (IdAllocator i a, MonadState a m) => Int -> m [i]
allocConsecutiveM n = modifyM (allocConsecutive n)

-- ====================================================================
-- SimpleAllocator

newtype SimpleAllocator i = SimpleAllocator i

newSimpleAllocator :: i -> SimpleAllocator i
newSimpleAllocator = SimpleAllocator

instance (Bounded i, Integral i) => IdAllocator i (SimpleAllocator i) where
    alloc (SimpleAllocator i) | i == maxBound = return (i, SimpleAllocator minBound)
    alloc (SimpleAllocator i)                 = return (i, SimpleAllocator (i + 1))

    allocConsecutive n (SimpleAllocator i) | n + fromIntegral i > maxBound = fail "blah"
    allocConsecutive n a                                                   = allocMany n a
