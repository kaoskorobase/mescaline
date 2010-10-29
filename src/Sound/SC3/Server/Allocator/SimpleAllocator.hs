{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Sound.SC3.Server.Allocator.SimpleAllocator (
    SimpleAllocator
  , cons
) where

import Control.Monad.Error
import Control.DeepSeq (NFData(..))
import Data.BitSet as Set
import Sound.SC3.Server.Allocator

data SimpleAllocator i =
    SimpleAllocator
        {-# UNPACK #-} !(Range i)
        {-# UNPACK #-} !(Set.BitSet i)
        {-# UNPACK #-} !i
        deriving (Eq, Show)

instance NFData i => NFData (SimpleAllocator i) where
    rnf (SimpleAllocator x1 x2 x3) =
        rnf x1 `seq`
            x2 `seq`
        rnf x3 `seq` ()

cons :: Range i -> SimpleAllocator i
cons r = SimpleAllocator r Set.empty (lowerBound r)

findNext :: (Integral i) => SimpleAllocator i -> Maybe i
findNext (SimpleAllocator r u n) = loop (succ n)
    where
        loop !i
            | i == n = Nothing
            | i == upperBound r = loop (lowerBound r)
            | Set.member (fromIntegral i) u = loop (succ i)
            | otherwise = Just i

sa_alloc :: (Integral i, MonadError String m) => SimpleAllocator i -> m (i, SimpleAllocator i)
sa_alloc s@(SimpleAllocator r u n)
    | Set.member (fromIntegral n) u = throwError "SimpleAllocator: No free Ids left"
    | otherwise = case findNext s of
                    Nothing -> throwError "SimpleAllocator: No free Ids left"
                    Just n' -> return (n, SimpleAllocator r (Set.insert (fromIntegral n) u) n')

sa_free :: (Integral i, MonadError String m) => i -> SimpleAllocator i -> m (SimpleAllocator i)
sa_free i (SimpleAllocator r u n) | Set.member (fromIntegral i) u = return (SimpleAllocator r u' n)
                                  | otherwise = throwError "SimpleAllocator: Id not in use"
    where u' = Set.delete (fromIntegral i) u

-- Simplest allocator imaginable, for testing.

sa_alloc_ :: (Enum i, Eq i, Monad m) => SimpleAllocator i -> m (i, SimpleAllocator i)
sa_alloc_ (SimpleAllocator r u n) | n == upperBound r = return (n, SimpleAllocator r u (lowerBound r))
                                  | otherwise         = return (n, SimpleAllocator r u (succ n)      )

sa_free_ :: (Monad m) => i -> SimpleAllocator i -> m (SimpleAllocator i)
sa_free_ _ = return

instance (Integral i) => IdAllocator i (SimpleAllocator i) where
    alloc = sa_alloc
    free  = sa_free
