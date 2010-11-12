{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Data.KDTree (
    Tree
  , empty
  , fromList
  , elems
  , Distance
  , sqrEuclidianDistance
  , closest
  , withinRadius
  , samples
) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Arrow
import           Data.Foldable
import qualified Data.List as List
import           Data.Monoid
import           Data.Ord
import           Data.Traversable
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
-- import           Statistics.Sample

import Debug.Trace

data Tree v a =
    Empty
  | Leaf !(v Double) a
  | Node {-# UNPACK #-} !Int {-# UNPACK #-} !Double (Tree v a) (Tree v a)

-- instance (Vector v Double, Show (v Double), Show a) => Show (Tree v a) where
--     show (Node i b l r) = "Node " ++ show i ++ " " ++ show b ++ " (" ++ show l ++ ")" ++ " (" ++ show r ++ ")"
--     show (Leaf v a)     = "Leaf (" ++ show v ++ ") (" ++ show a ++ ")"

instance Functor (Tree v) where
    fmap _ Empty = Empty
    fmap f (Leaf v a) = Leaf v (f a)
    fmap f (Node i x l r) = Node i x (fmap f l) (fmap f r)

instance Foldable (Tree v) where
    foldMap _ Empty = mempty
    foldMap f (Leaf v a) = f a
    foldMap f (Node _ _ l r) = foldMap f l `mappend` foldMap f r

instance Traversable (Tree v) where
    traverse _ Empty = pure Empty
    traverse f (Leaf v x) = Leaf v <$> f x
    traverse f (Node i p l r) = Node i p <$> traverse f l <*> traverse f r

transposeV :: Vector v a => [v a] -> [v a]
transposeV [] = []
transposeV vs@(v0:_) = map f [0..V.length v0 - 1]
    where f i = V.fromList (map (\v -> v ! i) vs)

empty :: Tree v a
empty = Empty

fromList :: (Vector v Double) => [(v Double, a)] -> Tree v a
fromList [] = Empty
fromList [(v, a)] = Leaf v a
fromList xs@((v0, _):_) = build (V.length v0) 0 xs
    where
        build k depth xs =
            let axis = depth `mod` k
                xs' = List.sortBy (comparing (flip (!) axis . fst)) xs
                median = length xs' `div` 2
            in Node axis (fst (xs' !! median) ! axis) (fromList (take median xs')) (fromList (drop median xs'))

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

findBest :: Vector v Double => v Double -> Tree v a -> (v Double, a)
{-# INLINE findBest #-}
findBest _ Empty = error "findBest: empty tree"
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
-- sqrEuclidianDistance a b = V.sum (V.map sqr (V.zipWith (-) a b))
sqrEuclidianDistance a b = loop 0 0 (V.length a `min` V.length b)
    where
        loop !acc !i !n
            | i >= n = acc
            | otherwise = let x = (a V.! i) - (b V.! i) in loop (acc + x*x) (i+1) n

closest' :: Vector v Double => Distance v -> v Double -> Tree v a -> (v Double, a) -> Double -> ((v Double, a), Double)
{-# INLINE closest' #-}
closest' dist point tree best bestDist =
    case tree of
        Empty ->
            error "closest': empty tree"
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
closest dist p Empty = Nothing
closest dist p tree = Just $ second sqrt $ closest' dist p tree (findBest p tree) (1/0)

withinRadius :: Vector v Double => Distance v -> v Double -> Double -> Tree v a -> [((v Double, a), Double)]
{-# INLINE withinRadius #-}
withinRadius dist point radius tree = map (second sqrt) (loop tree [])
    where
        sqrRadius = sqr radius
        loop tree result =
            case tree of
                Empty ->
                    result
                Leaf v a ->
                    let d = point `dist` v
                    in if d < sqrRadius
                       then ((v, a), d) : result
                       else result
                Node dim dimValue left right ->
                    let (nearChild, farChild) = if point ! dim < dimValue then (left, right) else (right, left)
                        result' = loop nearChild result
                        result'' = if sqrAxisDistance dim dimValue point < sqrRadius
                                   then loop farChild result'
                                   else result'
                    in result''

samples :: [(UV.Vector Double, Int)]
samples = zip [
    V.fromList [0, 0]
  , V.fromList [1, 1]
  -- , V.fromList [1, 1]
  , V.fromList [0, 1]
  , V.fromList [1, 0]
  , V.fromList [0.5, 0.5]
  , V.fromList [0.25, 0.5]
  , V.fromList [0.25, 0.75]
  , V.fromList [0.25, 0.25]
  ]
  [0..]