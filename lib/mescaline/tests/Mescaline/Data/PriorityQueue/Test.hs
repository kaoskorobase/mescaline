{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Data.PriorityQueue.Test ( tests ) where

import Mescaline.Data.PriorityQueue

import Control.Monad (liftM)
import Data.Monoid (mappend)
import Test.Framework (Test)
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (PriorityQueue k v) where
    arbitrary = liftM (fromList) arbitrary

tests :: [Test]
tests =
    [ testGroup "Mescaline.Data.PriorityQueue"
        [ testProperty "split" $ \(k :: Double) (pq :: PriorityQueue Double Char) ->
            let (as, pq') = split k pq in as `mappend` assocs pq' == assocs pq
        ]
    ]
