{-# LANGUAGE ScopedTypeVariables #-}
module Data.PriorityQueue.Test where

import Mescaline.Data.PriorityQueue

import Control.Monad (liftM)
import Data.Monoid (mappend)
import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (PQueue k v) where
    arbitrary = liftM (fromList) arbitrary

instance (Ord k, Show k, Show v) => Show (PQueue k v) where
    show pq = "fromList " ++ show (assocs pq)

tests :: [Test]
tests =
    [ testGroup "properties"
        [ testProperty "split" $ \(k :: Double) (pq :: PQueue Double Char) ->
            let (as, pq') = split k pq in as `mappend` assocs pq' == assocs pq
        ]
    ]

main :: IO ()
main = defaultMain tests
