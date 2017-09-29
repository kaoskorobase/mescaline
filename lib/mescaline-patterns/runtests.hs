module Main where

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain Data.KDTree.Test.tests

