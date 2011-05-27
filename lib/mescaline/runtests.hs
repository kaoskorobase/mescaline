module Main where

import qualified Data.KDTree.Test
import qualified Mescaline.Data.PriorityQueue.Test

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
        $  Data.KDTree.Test.tests
        ++ Mescaline.Data.PriorityQueue.Test.tests

