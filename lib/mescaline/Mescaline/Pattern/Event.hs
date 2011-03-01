{-# LANGUAGE CPP, DeriveDataTypeable #-}

#include "Accessor.h"

module Mescaline.Pattern.Event (
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
  , offset
  , duration
  , rate
  , pan
  , attackTime
  , releaseTime
  , sustainLevel
  , gateLevel
  , sendLevel1
  , sendLevel2
  , fxParam1
  , fxParam2
  , latency
) where

import           Data.Accessor
import           Mescaline.Time (Duration)
import qualified Mescaline.Time as Time
import           Mescaline.FeatureSpace.Model ()
import qualified Mescaline.FeatureSpace.Unit as Unit

data Synth = Synth {
    _unit         :: Unit.Unit
  , _offset       :: Duration
  , _duration     :: Duration
  , _rate         :: Double
  , _pan          :: Double
  , _attackTime   :: Double
  , _releaseTime  :: Double
  , _sustainLevel :: Double
  , _gateLevel    :: Double
  , _sendLevel1   :: Double
  , _sendLevel2   :: Double
  , _fxParam1     :: Double
  , _fxParam2     :: Double
  , _latency      :: Double
  } deriving (Eq, Show)

defaultSynth :: Unit.Unit -> Synth
defaultSynth u =
    Synth {
        _unit         = u
      , _offset       = 0
      , _duration     = Unit.duration u
      , _rate         = 1
      , _pan          = 0
      , _attackTime   = 0
      , _releaseTime  = 0
      , _sustainLevel = 1
      , _gateLevel    = 0
      , _sendLevel1   = 0
      , _sendLevel2   = 0
      , _fxParam1     = 0
      , _fxParam2     = 0
      , _latency      = 0.2
      }

ACCESSOR(unit,          _unit,          Synth, Unit.Unit)
ACCESSOR(offset,        _offset,        Synth, Duration)
ACCESSOR(duration,      _duration,      Synth, Duration)
ACCESSOR(rate,          _rate,          Synth, Double)
ACCESSOR(pan,           _pan,           Synth, Double)
ACCESSOR(attackTime,    _attackTime,    Synth, Double)
ACCESSOR(releaseTime,   _releaseTime,   Synth, Double)
ACCESSOR(sustainLevel,  _sustainLevel,  Synth, Double)
ACCESSOR(gateLevel,     _gateLevel,     Synth, Double)
ACCESSOR(sendLevel1,    _sendLevel1,    Synth, Double)
ACCESSOR(sendLevel2,    _sendLevel2,    Synth, Double)
ACCESSOR(fxParam1,      _fxParam1,      Synth, Double)
ACCESSOR(fxParam2,      _fxParam2,      Synth, Double)
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
