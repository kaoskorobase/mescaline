{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mescaline.Quant (
  Quant(..)
, Meter(..)
, beatsUntilQuantized
, quantize
, isQuantized
) where

import Data.Default (Default(..))
import Data.Fixed (mod')
import Mescaline.Time (Beats)

data Quant = Quant {
    quant :: Beats
  , phase :: Beats
  } deriving (Eq, Show)

instance Default Quant where
  def = Quant 0 0

newtype Meter = Meter {
    beatsPerBar :: Int
  } deriving (Eq, Num, Show)

wrap :: (Num a, Ord a) => a -> a -> a
wrap a b
  | b == 0 = a
  | a < 0 = wrap (a + b) b
  | otherwise = a

beatsUntilQuantized :: Meter -> Quant -> Beats -> Beats
beatsUntilQuantized m (Quant q p) b
  | q == 0 = p
  | otherwise =
      let qx = if q < 0
               then fromIntegral (beatsPerBar m) * negate q
               else q
          px = wrap p qx
          rx = mod' b qx
          bx = if rx == 0
               then 0
               else qx - rx
      in bx + px

quantize :: Meter -> Quant -> Beats -> Beats
quantize m q b = b + beatsUntilQuantized m q b

isQuantized :: Meter -> Quant -> Beats -> Bool
isQuantized m q b =
  let t = beatsUntilQuantized m q b
  in abs t < 1e-9
