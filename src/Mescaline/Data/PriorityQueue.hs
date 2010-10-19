module Mescaline.Data.PriorityQueue (
    module Data.PriorityQueue.FingerTree
  , minKey
  , popUntil
) where

import Data.PriorityQueue.FingerTree

minKey :: Ord k => PQueue k a -> Maybe k
minKey = fmap (\((k, _), _) -> k) . minViewWithKey
                
-- | Pop all elements from a priority queue whose key is smaller than t
popUntil :: Ord k => PQueue k a -> k -> ([a], PQueue k a)
pq `popUntil` k0 = loop pq []
    where
        loop pq as = case minViewWithKey pq of
                        Nothing            -> (reverse as, pq)
                        Just ((k, a), pq') -> if k > k0
                                              then (reverse as, pq)
                                              else loop pq' (a:as)
