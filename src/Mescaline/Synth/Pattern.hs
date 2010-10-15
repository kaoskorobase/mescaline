{-# LANGUAGE CPP, DeriveDataTypeable #-}

#include "Accessor.h"

module Mescaline.Synth.Pattern (
    Event
  , time
  , delta
  , unit
  , synth
  , SynthParams
  , defaultSynth
  , attackTime
  , releaseTime
  , sustainLevel
  , gateLevel
  , latency
  , PCons(..)
) where

import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Monad
import           Data.Accessor
-- import           Data.Accessor.Template
import           Data.List (scanl1)
import           Data.Typeable (Typeable)
import           Debug.Trace (traceShow)
import           Mescaline (Time)
import qualified Sound.SC3.Lang.Pattern.List as P
import qualified Mescaline.Database.Unit as Unit

-- import           Sound.OpenSoundControl hiding (Time)
-- import qualified Data.PriorityQueue.FingerTree as PQ

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
    _attackTime   = 0,
    _releaseTime  = 0,
    _sustainLevel = 1,
    _gateLevel    = 0,
    _latency      = 0.2
}

ACCESSOR(attackTime,    _attackTime,    SynthParams, Double)
ACCESSOR(releaseTime,   _releaseTime,   SynthParams, Double)
ACCESSOR(sustainLevel,  _sustainLevel,  SynthParams, Double)
ACCESSOR(gateLevel,     _gateLevel,     SynthParams, Double)
ACCESSOR(latency,       _latency,       SynthParams, Double)

-- $(deriveAccessors ''SynthParams)

data Event = Event {
    _time  :: Time
  , _delta :: Double
  , _unit  :: Unit.Unit
  , _synth :: SynthParams
} deriving (Eq, Show)

ACCESSOR(time,  _time,  Event, Time)
ACCESSOR(delta, _delta,  Event, Double)
ACCESSOR(unit,  _unit,  Event, Unit.Unit)
ACCESSOR(synth, _synth, Event, SynthParams)

-- $(deriveAccessors ''SynthEvent)

fromUnit :: Double -> Unit.Unit -> Event
fromUnit t u = Event t (Unit.duration u) u defaultSynth

data Environment = Environment {
    -- tracks :: [
}

-- data Event = Event | Rest

-- Enviroment -> P Event

-- (ptrack Track1) `fmap` (\e -> setVal time e (getVal unit.time) e

-- data Env = Env {
--     e_time     :: Time
-- }

-- data Input    = Input
-- type Output   = Event SynthEvent
-- type Pattern  = SF (Env, Event Input) Output

-- newtype PCons = PCons (Database -> Pattern) deriving (Typeable)
data PCons = PCons deriving (Typeable)

-- pattern :: (Database -> Pattern) -> PCons
-- pattern = PCons

-- event :: Unit.Unit -> Event
-- event u = Event {
--     delta_ = Unit.duration u,
--     unit_  = u,
--     synth_ = defaultSynth
-- }

-- pevent :: Unit.Unit -> Pattern
-- pevent = return . event

-- ptrace :: Show a => P.P a -> P.P a
-- ptrace = fmap f
--     where f a = traceShow a a

-- ====================================================================
-- Driver

-- data Input = ITimer Time | IControl String Double | IPattern Pattern
-- 
-- execute :: (SynthEvent -> Double -> IO ()) -> Chan Pattern -> IO ()
-- execute f pc = do
--     c <- newChan
--     env <- newEnv
--     forkIO $ readChan pc >>= writeChan c . IPattern
--     loop c env Nothing
--     where
--         newEnv = do
--             t <- utcr
--             return $ Env {
--                 e_realTime = t
--               , e_timers = PQ.empty
--             }
--         -- perform e t t' = f e t >> pauseThreadUntil t'
--         toList pq = case PQ.minViewWithKey pq of
--                         Nothing       -> []
--                         Just (x, pq') -> x : toList pq'
--         scheduleTimers c env = mapM_ (\(t, _) -> pauseThreadUntil t >> writeChan c (ITimer t)) (toList (e_timers env))
--         loop c env Nothing = do
--             i <- readChan c
--             case i of
--                 IPattern p -> do
--                     env <- newEnv
--                     loop c env (Just p)
--                 _ -> loop c env Nothing
--         loop c env (Just p) = do
--             let ((env', e), p') = runSF p env
--             scheduleTimers c env'
--             case e of
--                 NoEvent -> return ()
--                 Event e -> f e (e_realTime env')
--             i <- readChan c
--             case i of
--                 ITimer t -> loop c (env { e_realTime = t }) (Just p)
--                 IControl k v -> loop c env (Just p)
--                 IPattern p -> do
--                     env <- newEnv
--                     loop c env (Just p)

-- pq `popUntil` t = f pq []
--     where
--         f pq l = case PQ.minViewWithKey pq of
--                     Nothing       -> (reverse l, pq)
--                     Just (x, pq') -> if fst x > t
--                                      then (reverse l, pq)
--                                      else f pq' (snd x:l)
-- 
-- execute :: Double -> Pattern -> Chan Input -> Chan (Env, Output) -> IO ()
-- execute tick sf ichan ochan = loop (Env 0) sf
--     where
--         loop env sf = do
--             empty <- isEmptyChan ichan
--             i <- if empty then return NoEvent else liftM Event (readChan ichan)
--             let (o, sf') = runSF sf (env, i)
--                 t' = e_time env + tick
--             writeChan ochan (env, o)
--             pauseThreadUntil t'
--             loop (Env t') sf'
