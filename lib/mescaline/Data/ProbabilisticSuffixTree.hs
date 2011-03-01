module Data.ProbabilisticSuffixTree (
    Tree(..)
  , NodeMap
  , Node(..)
  , empty
  , construct
  , toTree
  , printTree
) where

import qualified Data.Map as M
import qualified Data.ProbabilisticSuffixTree.Context as C
import qualified Data.Tree as T

data Tree a = Tree { nodes :: NodeMap a }
			  deriving (Eq, Show)

type NodeMap a = M.Map a (Node a)

data Node a = Node {
    symbol :: a
  , count :: !Int
  , probability :: !Double
  , children :: NodeMap a
  } deriving (Eq, Show)

empty :: Tree a
empty = Tree M.empty

-- This function could also do some tree pruning and pmf smoothing.
fromContext :: C.NodeMap a -> Tree a
fromContext ns = Tree (M.mapWithKey (fromContextNode (C.counts ns)) (C.nodes ns))

fromContextNode :: Int -> a -> C.Node a -> Node a
fromContextNode ss a n = Node a c (fromIntegral c / fromIntegral ss) (M.mapWithKey (fromContextNode (C.counts ns)) (C.nodes ns))
    where
        c = C.count n
        ns = C.children n

construct :: Ord a => [a] -> Tree a
construct = fromContext . C.construct

toTree :: Tree a -> T.Tree (Maybe (a, Int, Double))
toTree (Tree ns) = T.Node Nothing (map toTreeNode (M.elems ns))

toTreeNode :: Node a -> T.Tree (Maybe (a, Int, Double))
toTreeNode n = T.Node (Just (symbol n, count n, probability n)) (map toTreeNode (M.elems (children n)))

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . T.drawTree . fmap show . toTree
