module Data.ProbabilisticSuffixTree.Context (
    NodeMap(counts, nodes)
  , Node(count, children)
  , prefixes
  , construct
  , insert
) where

import qualified Data.List as L
import qualified Data.Map as M

data NodeMap a = NodeMap {
    counts :: !Int
  , nodes :: M.Map a (Node a)
  } deriving (Eq, Show)

empty :: NodeMap a
empty = NodeMap 0 M.empty

elems :: NodeMap a -> [Node a]
elems = M.elems . nodes

data Node a = Node {
    count :: !Int
  , children :: NodeMap a
  } deriving (Eq, Show)

mapCount :: (Int -> Int) -> Node a -> Node a
mapCount f (Node c ns) = Node (f c) ns

mapChildren :: (NodeMap a -> NodeMap a) -> Node a -> Node a
mapChildren f (Node c ns) = Node c (f ns)

prefixes :: [a] -> [(a, [[a]])]
prefixes xs = map (\l -> let ([a]:as) = tail (L.inits l) in (a, [] : map tail as))
		  		(init (L.tails (reverse xs)))

construct :: Ord a => [a] -> NodeMap a
construct = L.foldl' (\t (a, pps) ->
					    L.foldl' (\t' ps -> insert a ps t') t pps)
                     empty
            . prefixes

insert :: Ord a => a -> [a] -> NodeMap a -> NodeMap a
insert a [] (NodeMap cs ns) =
    NodeMap (cs+1) (M.insertWith' (const (mapCount (+1))) a (Node 1 empty) ns)
insert a (p:ps) (NodeMap cs ns) =
	case M.lookup p ns of
		Nothing -> NodeMap cs (M.insert p (Node 0 (insert a ps empty)) ns)
		Just n -> NodeMap cs (M.insert p (mapChildren (insert a ps) n) ns)
