{-# LANGUAGE CPP, DeriveDataTypeable #-}

#include "Accessor.h"

module Mescaline.Synth.Pattern.Event (
    Event
  , Cursor
  , rest
  , synthEvent
  , delta
  , cursor
  , synth
  , isRest
  , withSynth
  , Synth
  , defaultSynth
  , unit
  , duration
  , rate
  , attackTime
  , releaseTime
  , sustainLevel
  , gateLevel
  , latency
) where

import           Data.Accessor
import           Mescaline.Time (Duration)
import qualified Mescaline.Time as Time
import           Mescaline.Synth.FeatureSpace.Model ()
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit

data Synth = Synth {
    _unit         :: Unit.Unit
  , _duration     :: Duration
  , _rate         :: Double
  , _attackTime   :: Double
  , _releaseTime  :: Double
  , _sustainLevel :: Double
  , _gateLevel    :: Double
  , _latency      :: Double
  } deriving (Eq, Show)

defaultSynth :: Unit.Unit -> Synth
defaultSynth u =
    Synth {
        _unit         = u
      , _duration     = Unit.duration u
      , _rate         = 1
      , _attackTime   = 0
      , _releaseTime  = 0
      , _sustainLevel = 1
      , _gateLevel    = 0
      , _latency      = 0.2
      }

ACCESSOR(unit,          _unit,          Synth, Unit.Unit)
ACCESSOR(duration,      _duration,      Synth, Duration)
ACCESSOR(rate,          _rate,          Synth, Double)
ACCESSOR(attackTime,    _attackTime,    Synth, Double)
ACCESSOR(releaseTime,   _releaseTime,   Synth, Double)
ACCESSOR(sustainLevel,  _sustainLevel,  Synth, Double)
ACCESSOR(gateLevel,     _gateLevel,     Synth, Double)
ACCESSOR(latency,       _latency,       Synth, Double)

-- data Cursor =
--     NoCursor
--   | Cursor {
--         cursorId       :: Int
--       , cursorPosition :: (Int,Int)
--       , cursorValue    :: Double
--       }
--     deriving (Eq, Read, Show)

type Cursor = Int

data Event =
    Event {
        _delta      :: Duration
      , _cursor     :: Cursor
      , _synth      :: Maybe Synth
      }
    deriving (Eq, Show)

ACCESSOR(delta,  _delta,  Event, Double)
ACCESSOR(cursor, _cursor, Event, Cursor)
ACCESSOR(synth,  _synth,  Event, Maybe Synth)

rest :: Cursor -> Duration -> Event
rest c d = Event d c Nothing

synthEvent :: Cursor -> Unit.Unit -> Event
synthEvent c u = Event d c (Just (defaultSynth u))
    where d = Unit.duration u

isRest :: Event -> Bool
isRest (Event _ _ Nothing) = True
isRest _                   = False

withSynth :: a -> (Synth -> a) -> Event -> a
withSynth a0 f e = case _synth e of
                    Nothing -> a0
                    Just s  -> f s

instance Time.HasDelta (Event) where
    delta = delta

-- instance Time.HasDuration (Event) where
--     duration = duration
