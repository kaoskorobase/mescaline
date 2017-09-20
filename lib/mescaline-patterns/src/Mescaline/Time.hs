module Mescaline.Time (
    Time
  , Duration
  , HasDelta(..)
  , HasDuration(..)
  , ToRest(..)
) where

import Control.Lens

type Time     = Double -- ^ Absolute or relative time stamp
type Duration = Double -- ^ Difference of times

class HasTime a where
    time :: Lens' a Time

class HasDelta a where
    delta :: Lens' a Duration

class HasDuration a where
    duration :: Lens' a Duration

class ToRest a where
    rest :: Duration -> a

