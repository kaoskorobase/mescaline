{-# LANGUAGE Arrows #-}
module Mescaline.Synth.SF (
    module Euterpea.Signal.SF
  , identity
  , constant
  , Event(..)
  , noEvent
  , event
  , initially
  , tag
  , filter
  , hold
  , accum
  , scanl
  , edge
) where

import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer (lift)
import           Control.Arrow.Transformer.Reader
import           Control.Applicative
import           Control.CCA.Types
import           Euterpea.Signal.SF
import           Prelude hiding (filter, init, scanl)

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

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

event :: b -> (a -> b) -> Event a -> b
event b _ NoEvent   = b
event _ f (Event a) = f a

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

-- | Zero order hold.
hold :: a -> SF (Event a) a
hold a0 = scanl f a0
    where
        f a NoEvent   = a
        f _ (Event a) = a

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
