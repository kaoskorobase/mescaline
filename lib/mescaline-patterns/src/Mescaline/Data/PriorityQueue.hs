{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mescaline.Data.PriorityQueue (
    PriorityQueue
  , empty
  , singleton
  , union
  , insert
  , add
  , fromList
  , toList
  , elems
  , null
  , minView
  , minViewWithKey
  , minKey
  , minElem
  , split
  , assocs
) where

import           Control.Arrow (second)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import           Data.Monoid (Monoid)
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import Prelude hiding (null)

newtype PriorityQueue k v = PriorityQueue (PQueue k v)
                            deriving (Foldable, Functor, Monoid)

instance (Ord k, Show k, Show v) => Show (PriorityQueue k v) where
    show pq = "fromList " ++ show (assocs pq)

empty :: Ord k => PriorityQueue k v
empty = PriorityQueue PQ.empty

singleton :: Ord k => k -> v -> PriorityQueue k v
singleton k v = PriorityQueue (PQ.singleton k v)

union :: Ord k => PriorityQueue k v -> PriorityQueue k v -> PriorityQueue k v
union (PriorityQueue pq1) (PriorityQueue pq2) = PriorityQueue (PQ.union pq1 pq2)

insert :: Ord k => k -> v -> PriorityQueue k v -> PriorityQueue k v
insert k v (PriorityQueue pq) = PriorityQueue (PQ.insert k v pq)

add :: Ord k => k -> v -> PriorityQueue k v -> PriorityQueue k v
add k v (PriorityQueue pq) = PriorityQueue (PQ.add k v pq)

fromList :: Ord k => [(k, v)] -> PriorityQueue k v
fromList = PriorityQueue . PQ.fromList

toList :: Ord k => PriorityQueue k v -> [v]
toList = Fold.toList

elems :: Ord k => PriorityQueue k v -> [v]
elems = toList

null :: Ord k => PriorityQueue k v -> Bool
null (PriorityQueue pq) = PQ.null pq

minView :: Ord k => PriorityQueue k v -> Maybe (v, PriorityQueue k v)
minView (PriorityQueue pq) = fmap (second PriorityQueue) (PQ.minView pq)

minViewWithKey :: Ord k => PriorityQueue k v -> Maybe ((k, v), PriorityQueue k v)
minViewWithKey (PriorityQueue pq) = fmap (second PriorityQueue) (PQ.minViewWithKey pq)

minKey :: Ord k => PriorityQueue k v -> Maybe k
minKey = fmap (fst.fst) . minViewWithKey

minElem :: Ord k => PriorityQueue k v -> Maybe v
minElem = fmap (snd.fst) . minViewWithKey

-- | Pop all elements from a priority queue whose key is smaller than or equal to k0.
split :: Ord k => k -> PriorityQueue k v -> ([(k, v)], PriorityQueue k v)
split k0 = loop
    where
        -- loop pq as = case minViewWithKey pq of
        --                 Nothing            -> (reverse as, pq)
        --                 Just ((k, a), pq') -> if k > k0
        --                                       then (reverse as, pq)
        --                                       else loop pq' (a:as)
        loop pq = case minViewWithKey pq of
                    Nothing            -> ([], pq)
                    Just (x@(k, _), pq') -> if k > k0
                                            then ([], pq)
                                            else let (xs, pq'') = loop pq' in (x:xs, pq'')

assocs :: Ord k => PriorityQueue k v -> [(k, v)]
assocs pq = case minViewWithKey pq of
                Nothing -> []
                Just ((k, v), pq') -> (k, v) : assocs pq'
