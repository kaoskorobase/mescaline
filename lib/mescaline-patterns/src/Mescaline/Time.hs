{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mescaline.Time (
    Duration
  , Seconds
  , Beats
  , HasDelta(..)
  , HasDuration(..)
  , ToRest(..)
) where

import Control.Lens

-- type Time     = Double -- ^ Absolute or relative time stamp
type Duration = Double -- ^ Difference of times

newtype Seconds = Seconds Double deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)
newtype Beats = Beats Double deriving (Eq, Fractional, Num, Ord, Real, RealFrac, Show)

class HasDelta a where
    delta :: Lens' a Duration

class HasDuration a where
    duration :: Lens' a Duration

class ToRest a where
    rest :: Duration -> a

