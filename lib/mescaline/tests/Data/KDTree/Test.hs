{-# LANGUAGE FlexibleContexts #-}
module Data.KDTree.Test ( tests ) where

import           Control.Applicative
import           Data.KDTree
import           Data.Ord (comparing)
import           Data.Vector.Generic (Vector, (!))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as Set

import           Test.Framework (Test)
import           Test.Framework (testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

newtype Elems a = Elems { unElems :: ([(U.Vector Double, a)], U.Vector Double) }
                    deriving (Show)

vector' :: (Arbitrary a, Vector v a) => Int -> Gen (v a)
vector' = fmap V.fromList . vector

instance Arbitrary a => Arbitrary (Elems a) where
    arbitrary = do
        k <- choose (1, 32)
        es <- listOf $ (,) <$> vector' k <*> arbitrary
        v <- vector' k
        return $ Elems (es, v)

proto_euclidianDistance :: (Floating a, Vector v a) => v a -> v a -> a
proto_euclidianDistance a b = sqrt (V.sum (V.map (\x -> x * x) (V.zipWith (-) a b)))

prop_euclidianDistance :: Gen Bool
prop_euclidianDistance = do
    k <- choose (1, 32)
    a <- vector' k :: Gen (U.Vector Double)
    b <- vector' k
    return $ proto_euclidianDistance a b == proto_euclidianDistance b a

prop_sqrEuclidianDistance :: Gen Bool
prop_sqrEuclidianDistance = do
    k <- choose (1, 32)
    a <- vector' k :: Gen (U.Vector Double)
    b <- vector' k
    let d1 = sqrEuclidianDistance a b
        d2 = proto_euclidianDistance a b
    return $ (sqrt d1 == d2) && (d1 >= 0) && (d2 >= 0)

proto_closest :: Vector v Double => v Double -> [(v Double, a)] -> Maybe ((v Double, a), Double)
proto_closest = go Nothing
    where
        go r _ [] = r
        go r v ((x, a):xs) =
            case r of
                Nothing ->
                    let r = ((x, a), v `proto_euclidianDistance` x)
                    in go (Just r) v xs 
                Just r@(_, d) ->
                    let d' = v `proto_euclidianDistance` x
                        r' = if d' < d then ((x, a), d') else r
                    in go (Just r') v xs

prop_closest :: Elems Int -> Bool
prop_closest es =
    let (xs, v) = unElems es
    in closest sqrEuclidianDistance v (fromList xs) == proto_closest v xs

proto_withinRadius :: Vector v Double => v Double -> Double -> [(v Double, a)] -> [((v Double, a), Double)]
proto_withinRadius v = go [] v . abs
    where
        go rs _ _ [] = rs
        go rs v r ((x, a):xs) =
            let d = v `proto_euclidianDistance` x
                rs' = if d < r
                      then ((x, a), d):rs
                      else rs
            in go rs' v r xs

prop_withinRadius :: Elems Int -> Double -> Bool
prop_withinRadius es r =
    let (xs, v) = unElems es
        rs1 = withinRadius sqrEuclidianDistance v r (fromList xs)
        rs2 = proto_withinRadius v r xs
    in Set.fromList rs1 == Set.fromList rs2

tests :: [Test]
tests =
    [ testGroup "Data.KDTree"
        [ testProperty "euclidianDistance" prop_euclidianDistance
        , testProperty "sqrEuclidianDistance" prop_sqrEuclidianDistance
        , testProperty "closest" prop_closest
        , testProperty "withinRadius" prop_withinRadius
        ]
    ]
