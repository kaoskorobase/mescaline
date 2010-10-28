{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Sound.SC3.Server.Allocator.SimpleAllocator (
    SimpleAllocator
  , cons
) where

import Control.DeepSeq (NFData(..))
import Data.Set as Set
import Sound.SC3.Server.Allocator
import Sound.SC3.Server.Allocator.Range as Range

data SimpleAllocator i = SimpleAllocator {
    sa_range :: !(Range i)
  , sa_used  :: !(Set.Set i)
  , sa_next  :: !i
  , sa_succ  :: !(i -> i)
}

instance Show i => Show (SimpleAllocator i) where
    show a = show (show $ sa_range a, show $ sa_used a, show $ sa_next a)

instance NFData i => NFData (SimpleAllocator i) where
    rnf (SimpleAllocator x1 x2 x3 x4) =
        rnf x1 `seq`
        rnf x2 `seq`
        rnf x3 `seq`
            x4 `seq` ()

cons :: Enum i => Range i -> SimpleAllocator i
cons r = SimpleAllocator r Set.empty (lowerBound r) succ

usedRange :: (Enum i, Num i, Ord i) => Set.Set i -> Range i
usedRange s | Set.null s = Range.empty
            | otherwise  = range (Set.findMin s) (succ (Set.findMax s))

getNext :: Ord i => SimpleAllocator i -> (i, SimpleAllocator i)
getNext (SimpleAllocator r u n f) = (n, SimpleAllocator r u' n' f)
    where
        n' = f n
        u' = Set.insert n u

findInSet :: (Enum i, Ord i, Monad m) => SimpleAllocator i -> i -> m (i, SimpleAllocator i)
findInSet s@(SimpleAllocator r u _ f) i
    | not (i `within` r) = fail "SimpleAllocator: No free Ids left"
    | Set.notMember i u  = return (i, SimpleAllocator r u (succ i) f)
    | otherwise          = findInSet s (succ i)

findNext :: (Enum i, Ord i, Monad m) => SimpleAllocator i -> Range i -> m (i, SimpleAllocator i)
findNext s@(SimpleAllocator r u _ _) r' =
    if not (Range.null ru)
    then return (getNext (SimpleAllocator r u (lowerBound ru) succ))
    else if not (Range.null rl)
         then return (getNext (SimpleAllocator r u (pred (upperBound rl)) pred))
         else findInSet s (lowerBound r)
    where
        rl = range (lowerBound r) (lowerBound r')
        ru = range (upperBound r') (upperBound r)

sa_alloc :: (Enum i, Num i, Ord i, Monad m) => SimpleAllocator i -> m (i, SimpleAllocator i)
sa_alloc s@(SimpleAllocator r u n _) =
    if n `within` r && not (n `within` r')
    then return (getNext s)
    else findNext s r'
    where r' = usedRange u

sa_free :: (Enum i, Num i, Ord i, Monad m) => i -> SimpleAllocator i -> m (SimpleAllocator i)
sa_free i (SimpleAllocator r u n f) | Set.member i u = return (SimpleAllocator r u' n f)
                                    | otherwise      = fail "SimpleAllocator: Id not in use"
    where u' = Set.delete i u

-- instance (Bounded i, Integral i) => IdAllocator i (SimpleAllocator i) where
--     alloc (SimpleAllocator i) | i == maxBound = return (i, SimpleAllocator minBound)
--     alloc (SimpleAllocator i)                 = return (i, SimpleAllocator (i + 1))
--     free i a = return a

instance (Enum i, Num i, Ord i) => IdAllocator i (SimpleAllocator i) where
    alloc = sa_alloc
    free  = sa_free
