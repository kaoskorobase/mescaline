module Mescaline.Data.PriorityQueue (
    PQueue
  , empty
  , singleton
  , union
  , insert
  , add
  , fromList
  , toList
  , null
  , minView
  , minViewWithKey
  , minKey
  , minElem
  , split
  , assocs
) where

import Data.Foldable (toList)
import Data.PriorityQueue.FingerTree
import Prelude hiding (null)

minKey :: Ord k => PQueue k a -> Maybe k
minKey = fmap (fst.fst) . minViewWithKey

minElem :: Ord k => PQueue k a -> Maybe a
minElem = fmap (snd.fst) . minViewWithKey

-- | Pop all elements from a priority queue whose key is smaller than or equal to k0.
split :: Ord k => k -> PQueue k a -> ([(k, a)], PQueue k a)
split k0 = loop
    where
        -- loop pq as = case minViewWithKey pq of
        --                 Nothing            -> (reverse as, pq)
        --                 Just ((k, a), pq') -> if k > k0
        --                                       then (reverse as, pq)
        --                                       else loop pq' (a:as)
        loop pq = case minViewWithKey pq of
                    Nothing            -> ([], pq)
                    Just (x@(k, a), pq') -> if k > k0
                                            then ([], pq)
                                            else let (xs, pq'') = loop pq' in (x:xs, pq'')

assocs :: Ord k => PQueue k a -> [(k, a)]
assocs pq = case minViewWithKey pq of
                Nothing -> []
                Just ((k, a), pq') -> (k, a) : assocs pq'
