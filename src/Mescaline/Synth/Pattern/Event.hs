{-# LANGUAGE CPP, DeriveDataTypeable #-}

#include "Accessor.h"

module Mescaline.Synth.Pattern.Event (
    Event
  , fromUnit
  , rest
  , delta
  , duration
  , unit
  , synth
  , isRest
  , SynthParams
  , defaultSynth
  , attackTime
  , releaseTime
  , sustainLevel
  , gateLevel
  , latency
) where

import           Data.Accessor
import           Mescaline.Time (Time, Duration)
import qualified Mescaline.Time as Time
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import qualified Sound.SC3.Lang.Pattern.Step as P

-- import           Sound.OpenSoundControl hiding (Time)
import qualified Data.PriorityQueue.FingerTree as PQ

import           Prelude hiding (filter, init, scanl)

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

data SynthParams = SynthParams {
    _attackTime   :: Double
  , _releaseTime  :: Double
  , _sustainLevel :: Double
  , _gateLevel    :: Double
  , _latency      :: Double
} deriving (Eq, Show)

defaultSynth :: SynthParams
defaultSynth = SynthParams {
    _attackTime   = 0
  , _releaseTime  = 0
  , _sustainLevel = 1
  , _gateLevel    = 0
  , _latency      = 0.2
}

ACCESSOR(attackTime,    _attackTime,    SynthParams, Double)
ACCESSOR(releaseTime,   _releaseTime,   SynthParams, Double)
ACCESSOR(sustainLevel,  _sustainLevel,  SynthParams, Double)
ACCESSOR(gateLevel,     _gateLevel,     SynthParams, Double)
ACCESSOR(latency,       _latency,       SynthParams, Double)

data Event =
    Rest {
        _delta    :: Duration
    }
  | Event {
        _delta    :: Duration
      , _duration :: Duration
      , _unit     :: Unit.Unit
      , _synth    :: SynthParams
      } deriving (Eq, Show)

fromUnit :: Unit.Unit -> Event
fromUnit u = Event d d u defaultSynth
    where d = Unit.duration u

rest :: Duration -> Event
rest = Rest

ACCESSOR(delta,    _delta,    Event, Double)
ACCESSOR(duration, _duration, Event, Double)
ACCESSOR(unit,     _unit,     Event, Unit.Unit)
ACCESSOR(synth,    _synth,    Event, SynthParams)

isRest :: Event -> Bool
isRest e = case e of
            Rest _ -> True
            _      -> False

instance Time.HasDelta (Event) where
    delta = delta

instance Time.HasDuration (Event) where
    duration = duration
