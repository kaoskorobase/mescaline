module Mescaline.Time (
    Time
  , Duration
  , HasDuration(..)
  , ToRest(..)
) where

import Data.Accessor

type Time     = Double -- ^ Absolute or relative time stamp
type Duration = Double -- ^ Difference of times

class HasTime a where
    time :: Accessor a Time

class HasDuration a where
    duration :: Accessor a Duration

class ToRest a where
    rest :: Duration -> a

data Timed a = Timed Time a

instance HasTime (Timed a) where
    time = accessor (\(Timed t _) -> t) (\t' (Timed t a) -> Timed t' a)
