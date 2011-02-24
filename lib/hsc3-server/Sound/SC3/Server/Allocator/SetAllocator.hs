{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses #-}
module Sound.SC3.Server.Allocator.SetAllocator (
    SetAllocator
  , cons
) where

import Control.Failure (Failure, failure)
import Control.DeepSeq (NFData(..))
import Data.BitSet as Set
import Sound.SC3.Server.Allocator

data SetAllocator i =
    SetAllocator
        {-# UNPACK #-} !(Range i)
        {-# UNPACK #-} !(Set.BitSet i)
                       !i
        deriving (Eq, Show)

instance NFData i => NFData (SetAllocator i) where
    rnf (SetAllocator x1 x2 x3) =
        rnf x1 `seq`
            x2 `seq`
        rnf x3 `seq` ()

cons :: Range i -> SetAllocator i
cons r = SetAllocator r Set.empty (lowerBound r)

findNext :: (Integral i) => SetAllocator i -> Maybe i
findNext (SetAllocator r u n) = loop (succ n)
    where
        loop !i
            | i == n = Nothing
            | i == upperBound r = loop (lowerBound r)
            | Set.member (fromIntegral i) u = loop (succ i)
            | otherwise = Just i

sa_alloc :: (Integral i, Failure AllocFailure m) => SetAllocator i -> m (i, SetAllocator i)
sa_alloc s@(SetAllocator r u n)
    | Set.member (fromIntegral n) u = failure NoFreeIds
    | otherwise = case findNext s of
                    Nothing -> failure NoFreeIds
                    Just n' -> return (n, SetAllocator r (Set.insert (fromIntegral n) u) n')

sa_free :: (Integral i, Failure AllocFailure m) => i -> SetAllocator i -> m (SetAllocator i)
sa_free i (SetAllocator r u n) | Set.member (fromIntegral i) u = return (SetAllocator r u' n)
                               | otherwise = failure InvalidId
    where u' = Set.delete (fromIntegral i) u

instance (Integral i) => IdAllocator i (SetAllocator i) where
    alloc = sa_alloc
    free  = sa_free
