{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | kd-Tree data structure for building multi-dimensional, real-valued
--   indices.
--
-- /References/
--
-- (1) <http://en.wikipedia.org/wiki/Kdtree>
module Data.KDTree
  ( Tree
  , empty
  , fromList
  , elems
  , Distance
  , sqrEuclidianDistance
  , closest
  , withinRadius
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Arrow
import           Control.DeepSeq (NFData(..))
import           Data.Foldable
import qualified Data.List as List
import           Data.Monoid
import           Data.Ord
import           Data.Traversable
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
-- import           Statistics.Sample

data Node v a = Leaf !(v Double) a
              | Node {-# UNPACK #-} !Int {-# UNPACK #-} !Double (Node v a) (Node v a)

instance Functor (Node v) where
    fmap f (Leaf v a)     = Leaf v (f a)
    fmap f (Node i x l r) = Node i x (fmap f l) (fmap f r)

instance Foldable (Node v) where
    foldMap f (Leaf v a)     = f a
    foldMap f (Node _ _ l r) = foldMap f l `mappend` foldMap f r

instance Traversable (Node v) where
    traverse f (Leaf v x)     = Leaf v <$> f x
    traverse f (Node i p l r) = Node i p <$> traverse f l <*> traverse f r

instance NFData a => NFData (Node v a) where
    rnf (Leaf x1 x2)       = x1 `seq` rnf x2 `seq` ()
    rnf (Node x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` ()

data Tree v a = Empty | Tree (Node v a)

-- instance (Vector v Double, Show (v Double), Show a) => Show (Tree v a) where
--     show (Node i b l r) = "Node " ++ show i ++ " " ++ show b ++ " (" ++ show l ++ ")" ++ " (" ++ show r ++ ")"
--     show (Leaf v a)     = "Leaf (" ++ show v ++ ") (" ++ show a ++ ")"

instance Functor (Tree v) where
    fmap _ Empty    = Empty
    fmap f (Tree n) = Tree (fmap f n)

instance Foldable (Tree v) where
    foldMap _ Empty    = mempty
    foldMap f (Tree n) = foldMap f n
    -- foldMap f (Leaf v a) = f a
    -- foldMap f (Node _ _ l r) = foldMap f l `mappend` foldMap f r

instance Traversable (Tree v) where
    traverse _ Empty    = pure Empty
    traverse f (Tree n) = Tree <$> traverse f n
    -- traverse f (Leaf v x) = Leaf v <$> f x
    -- traverse f (Node i p l r) = Node i p <$> traverse f l <*> traverse f r

instance NFData a => NFData (Tree v a) where
    rnf Empty     = ()
    rnf (Tree x1) = rnf x1 `seq` ()

transposeV :: Vector v a => [v a] -> [v a]
transposeV [] = []
transposeV vs@(v0:_) = map f [0..V.length v0 - 1]
    where f i = V.fromList (map (\v -> v ! i) vs)

empty :: Tree v a
empty = Empty

errorEmpty :: String -> a
errorEmpty who = error $ who ++ ": Empty tree"

fromList :: (Vector v Double) => [(v Double, a)] -> Tree v a
fromList []             = Empty
fromList xs@((v0, _):_) = Tree (nodeFromList (V.length v0) xs)

nodeFromList :: (Vector v Double) => Int -> [(v Double, a)] -> Node v a
nodeFromList _ [(v, a)] = Leaf v a
nodeFromList k xs       = build k 0 xs
    where
        at v i | V.length v < k = error "fromList: Vector too short"
               | otherwise      = v ! i
        build k depth xs =
            let axis = depth `mod` k
                xs' = List.sortBy (comparing (flip (!) axis . fst)) xs
                median = length xs' `div` 2
            in Node axis (fst (xs' !! median) `at` axis)
                    (nodeFromList k (take median xs'))
                    (nodeFromList k (drop median xs'))

-- fromList [] = Empty
-- fromList [(v, a)] = Leaf v [a]
-- fromList xs =
--     let
--         dims = transposeV (map fst xs)
--         meanVars = filter (\(_, (_, v)) -> v > 0) (zip [0..] (map meanVariance dims))
--     in
--         if null meanVars
--         then Leaf (fst (head xs)) (map snd xs)
--         else let (minVarDim, (minMean, _)) = List.minimumBy (comparing (snd.snd)) meanVars
--                  (left, right) = List.partition (\(v, _) -> v ! minVarDim < minMean) xs
--              in Node minVarDim minMean (fromList (traceShow left left)) (fromList right)

elems :: Tree v a -> [a]
elems = toList

type Distance v = v Double -> v Double -> Double

findBest :: Vector v Double => v Double -> Node v a -> (v Double, a)
{-# INLINE findBest #-}
findBest _ (Leaf v a) = (v, a)
findBest p (Node dim dimValue left right) =
    if p ! dim < dimValue
        then findBest p left
        else findBest p right

sqr :: Double -> Double
{-# INLINE sqr #-}
sqr x = x * x

sqrAxisDistance :: Vector v Double => Int -> Double -> v Double -> Double
{-# INLINE sqrAxisDistance #-}
sqrAxisDistance i x v = sqr (v ! i - x)

sqrEuclidianDistance :: Vector v Double => v Double -> v Double -> Double
{-# INLINE sqrEuclidianDistance #-}
sqrEuclidianDistance a b = loop 0 0 (V.length a `min` V.length b)
    where
        loop !acc !i !n
            | i >= n = acc
            | otherwise = let x = (a V.! i) - (b V.! i) in loop (acc + x*x) (i+1) n

closest' :: Vector v Double => Distance v -> v Double -> Node v a -> (v Double, a) -> Double -> ((v Double, a), Double)
{-# INLINE closest' #-}
closest' dist point node best bestDist =
    case node of
        Leaf v a ->
            let bestDist' = point `dist` v
            in if bestDist' < bestDist
               then ((v, a), bestDist')
               else (best, bestDist)
        Node dim dimValue left right ->
            let (nearChild, farChild) = if point ! dim < dimValue then (left, right) else (right, left)
                (best', bestDist') = closest' dist point nearChild best bestDist
            in if sqrAxisDistance dim dimValue point < bestDist'
               then closest' dist point farChild best' bestDist'
               else (best', bestDist')

closest :: Vector v Double => Distance v -> v Double -> Tree v a -> Maybe ((v Double, a), Double)
{-# INLINE closest #-}
closest dist p Empty       = Nothing
closest dist p (Tree node) = Just $ second sqrt $ closest' dist p node (findBest p node) (1/0)

withinRadius :: Vector v Double => Distance v -> v Double -> Double -> Tree v a -> [((v Double, a), Double)]
{-# INLINE withinRadius #-}
withinRadius dist point radius tree =
    case tree of
        Empty     -> []
        Tree node -> loop node []
    where
        sqrRadius = signum radius * sqr radius
        loop node result =
            case node of
                Leaf v a ->
                    let d = point `dist` v
                    in if d < sqrRadius
                       then ((v, a), sqrt d) : result
                       else result
                Node dim dimValue left right ->
                    let (nearChild, farChild) = if point ! dim < dimValue then (left, right) else (right, left)
                        result' = loop nearChild result
                        result'' = if sqrAxisDistance dim dimValue point < sqrRadius
                                   then loop farChild result'
                                   else result'
                    in result''
