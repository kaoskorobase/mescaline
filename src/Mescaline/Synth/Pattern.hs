{-# LANGUAGE Arrows, DeriveDataTypeable, TemplateHaskell #-}

module Mescaline.Synth.Pattern where

import           Control.CCA.Types
import           Control.Arrow
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad
import           Data.Accessor
import           Data.Accessor.Template
import           Data.List (scanl1)
import           Data.Typeable (Typeable)
import           Debug.Trace (traceShow)
import           Mescaline
import qualified Sound.SC3.Lang.Pattern.List as P
import           Mescaline.Database.FlatFile (Database)
import qualified Mescaline.Database.Unit as Unit

import           Sound.OpenSoundControl hiding (Time)
import           Euterpea.Signal.SF
import qualified Data.PriorityQueue.FingerTree as PQ

import           Prelude hiding (filter, init, scanl)

-- import Language.Haskell.TH              (Dec, Name, Q)

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

data SynthParams = SynthParams {
    attackTime_   :: Double,
    releaseTime_  :: Double,
    sustainLevel_ :: Double,
    gateLevel_    :: Double,
    latency_      :: Double
} deriving (Eq, Show)

defaultSynth :: SynthParams
defaultSynth = SynthParams {
    attackTime_   = 0,
    releaseTime_  = 0,
    sustainLevel_ = 1,
    gateLevel_    = 0,
    latency_      = 0.2
}

$(deriveAccessors ''SynthParams)

-- attackTime_ :: Double -> Event -> Event
-- attackTime_ x e = e { synth = (synth e) { attackTime = x } }
-- 
-- releaseTime_ :: Double -> Event -> Event
-- releaseTime_ x e = e { synth = (synth e) { releaseTime = x } }
-- 
-- amp_ :: Double -> Event -> Event
-- amp_ x e = e { synth = (synth e) { sustainLevel = x } }
-- 
-- latency_ :: Double -> Event -> Event
-- latency_ x e = e { synth = (synth e) { latency = x } }

data SynthEvent = SynthEvent {
    time_  :: Double,
    unit_  :: Unit.Unit,
    synth_ :: SynthParams
} deriving (Eq, Show)

$(deriveAccessors ''SynthEvent)

data Env = Env {
    e_time     :: Time
}

data Input    = Input
type Output   = Event SynthEvent
type Pattern  = SF (Env, Event Input) Output

newtype PCons = PCons (Database -> Pattern) deriving (Typeable)

pattern :: (Database -> Pattern) -> PCons
pattern = PCons

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
-- Signal functions

-- | Identity: identity = arr id
identity :: SF a a
identity = arr id

-- | Identity: constant b = arr (const b)
constant :: b -> SF a b
constant = arr . const

-- ====================================================================
-- Event functions

data Event a = NoEvent | Event a deriving (Eq, Read, Show)

noEvent :: Event a
noEvent = NoEvent

instance Functor Event where
    fmap _ NoEvent   = NoEvent
    fmap f (Event a) = Event (f a)

instance Applicative Event where
    pure = Event
    NoEvent   <*> _         = NoEvent
    _         <*> NoEvent   = NoEvent
    (Event f) <*> (Event a) = Event (f a)

instance Alternative Event where
    empty = noEvent
    NoEvent <|> p = p
    Event a <|> _ = Event a

-- | Transform initial output value.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF tf) = SF (\a0 -> let (b0, tf') = tf a0 in (f b0, tf'))

-- | Override initial output value.
-- Initialization operator (cf. Lustre/Lucid Synchrone).
(-->) :: b -> SF a b -> SF a b
-- (-->) b0 (SF tf) = SF (\a0 -> (b0, fst (tf a0)))
(-->) b0 = (-=>) (const b0)

-- | Transform initial input value.
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF tf) = SF (\a0 -> tf (f a0))

-- | Override initial input value.
(>--) :: a -> SF a b -> SF a b
-- (>--) a0 (SF f) = SF (\_ -> f a0)
(>--) a0 = (>=-) (const a0)

-- Override initial value of input signal.
initially :: a -> SF a a
initially = (--> identity)

-- | Replace event value.
tag :: b -> SF (Event a) (Event b)
tag b = arr (b <$)

-- | Filter out events that don't satisfy some predicate.
filter :: (a -> Bool) -> SF (Event a) (Event a)
filter p = arr f
    where
        f e@(Event a) = if (p a) then e else NoEvent
        f NoEvent     = NoEvent

-- | Accumulate from an initial value and an update event.
accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = SF (tf a0)
    where
        tf a NoEvent   = (NoEvent, SF (tf a))
        tf a (Event f) = let a' = f a in a' `seq` (Event a', SF (tf a'))

-- | asdjhaskjdh .
tagList :: [b] -> SF (Event a) (Event b)
tagList bs = SF (tf bs)
    where
        tf [] _             = (NoEvent, constant NoEvent)
        tf bs NoEvent       = (NoEvent, SF (tf bs))
        tf (b:bs) (Event _) = (Event b, SF (tf bs))

scanl :: (b -> a -> b) -> b -> SF a b
scanl f b0 =
    proc a -> do
        rec
            b <- init b0 -< f b a
        returnA -< b

-- -- | Mealy-style state machine, given initial value and transition
-- -- function.  Carries along event data.  See also 'mealy_'.
-- mealy :: (b -> a -> b) -> b -> SF a (a, b)
-- mealy b0 f = scanl g (a0, s0)
--     where
--         b0         = error "mealy: no initial value"
--         g (_, s) a = (a, f s a)
-- 
-- -- | Mealy-style state machine, given initial value and transition
-- -- function.  Forgetful version of 'mealy'.
-- mealy_ :: s -> (s -> s) -> SF a s
-- mealy_ s0 f = mealy s0 f >>> arr snd

edge :: SF Bool (Event ())
edge = scanl f (False, NoEvent) >>> arr snd
    where
        f (False, _) False = (False, NoEvent)
        f (False, _) True  = (True, Event ())
        f (True, _)  False = (False, NoEvent)
        f (True, _)  True  = (True, NoEvent)

countDown :: ArrowInit a => Int -> a () Int
countDown x = 
    proc _ -> do
      rec 
          i <- init x -< i - 1
      returnA -< i

countUp :: ArrowInit a => a () Int
countUp = 
    proc _ -> do
      rec 
         i <- init 0 -< i + 1
      returnA -< i

-- ====================================================================
-- Pattern functions

logicalTime :: SF Env Time
logicalTime = arr e_time

-- --| Outputs the time passed since the signal function instance was started.
-- localTime :: SF a Time
-- localTime = constant 1.0 >>> integral
-- 
-- -- Alternative name for localTime.
-- time :: SF a Time
-- time = localTime

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

pq `popUntil` t = f pq []
    where
        f pq l = case PQ.minViewWithKey pq of
                    Nothing       -> (reverse l, pq)
                    Just (x, pq') -> if fst x > t
                                     then (reverse l, pq)
                                     else f pq' (snd x:l)

executeSF :: Double -> SF (Double, Event a) b -> Chan a -> Chan (Double, b) -> IO ()
executeSF tick sf ichan ochan = utcr >>= loop sf
    where
        loop sf t = do
            empty <- isEmptyChan ichan
            a <- if empty then return NoEvent else liftM Event (readChan ichan)
            let (b, sf') = runSF sf (t, a)
                t' = t + tick
            b `seq` writeChan ochan (t, b)
            pauseThreadUntil t'
            loop sf' t'

execute :: Double -> Pattern -> Chan Input -> Chan (Env, Output) -> IO ()
execute tick sf ichan ochan = loop (Env 0) sf
    where
        loop env sf = do
            empty <- isEmptyChan ichan
            i <- if empty then return NoEvent else liftM Event (readChan ichan)
            let (o, sf') = runSF sf (env, i)
                t' = e_time env + tick
            writeChan ochan (env, o)
            pauseThreadUntil t'
            loop (Env t') sf'
