{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses #-}
module Sound.SC3.Server.Allocator.SimpleAllocator (
    SimpleAllocator
  , cons
) where

import Control.DeepSeq (NFData(..))
import Sound.SC3.Server.Allocator

newtype SimpleAllocator i = SimpleAllocator i deriving (Eq, Show)

instance NFData i => NFData (SimpleAllocator i) where
    rnf (SimpleAllocator x1) = rnf x1 `seq` ()

cons :: Num i => SimpleAllocator i
cons = SimpleAllocator 0

sa_alloc :: (Num i, Monad m) => SimpleAllocator i -> m (i, SimpleAllocator i)
sa_alloc (SimpleAllocator n) = return (n, SimpleAllocator (n+1))

sa_free :: (Monad m) => i -> SimpleAllocator i -> m (SimpleAllocator i)
sa_free _ sa = return sa

instance (Integral i) => IdAllocator i (SimpleAllocator i) where
    alloc = sa_alloc
    free  = sa_free
