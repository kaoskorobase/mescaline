{-# LANGUAGE Arrows #-}
module Data.Signal.SF (
    module Data.Signal.SF.Base
  , module Data.Signal.SF.Event
  , identity
  , constant
  , (>=-)
  , (-=>)
  , (>--)
  , (-->)
  , initially
  , tag
  , never
  , now
  , filter
  , hold
  , accum
  , accumHold
  , scanl
  , edge
  , sample
  , sample_
  , switch
  , switch'
  , rswitch
  , rswitch'
) where

import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer (lift)
import           Control.Arrow.Transformer.Reader
import           Control.Applicative
import           Control.CCA.Types
import           Data.Signal.SF.Base
import           Data.Signal.SF.Event
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

-- | Transform initial input value.
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF tf) = SF (\a0 -> tf (f a0))

-- | Transform initial output value.
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF tf) = SF (\a0 -> let (b0, tf') = tf a0 in (f b0, tf'))

-- | Override initial input value.
(>--) :: a -> SF a b -> SF a b
-- (>--) a0 (SF f) = SF (\_ -> f a0)
(>--) a0 = (>=-) (const a0)

-- | Override initial output value.
--
-- Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- TODO: equivalent to 'delay' in 'ArrowCircuit'?
(-->) :: b -> SF a b -> SF a b
-- (-->) b0 (SF tf) = SF (\a0 -> (b0, fst (tf a0)))
(-->) b0 = (-=>) (const b0)

-- | Override initial value of input signal.
initially :: a -> SF a a
initially = (--> identity)

-- ====================================================================
-- Event sources

-- | Event source that never occurs.
never :: SF a (Event b)
never = constant NoEvent

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: b -> SF a (Event b)
now b0 = (Event b0 --> never)

-- ====================================================================
-- Event modifiers

-- | Replace event value.
tag :: b -> SF (Event a) (Event b)
tag b = arr (b <$)

-- | asdjhaskjdh .
tagList :: [b] -> SF (Event a) (Event b)
tagList bs = SF (tf bs)
    where
        tf [] _             = (NoEvent, constant NoEvent)
        tf bs NoEvent       = (NoEvent, SF (tf bs))
        tf (b:bs) (Event _) = (Event b, SF (tf bs))

-- | Filter out events that don't satisfy a predicate.
filter :: (a -> Bool) -> SF (Event a) (Event a)
filter p = arr f
    where
        f e@(Event a) = if (p a) then e else NoEvent
        f NoEvent     = NoEvent

-- ====================================================================
-- Event/signal conversion

-- | Zero order hold.
hold :: a -> SF (Event a) a
hold a0 = scanl f a0
    where
        f a NoEvent   = a
        f _ (Event a) = a

-- | Signal to event
edge :: SF Bool (Event ())
edge = scanl f (False, NoEvent) >>> arr snd
    where
        f (False, _) False = (False, NoEvent)
        f (False, _) True  = (True, Event ())
        f (True, _)  False = (False, NoEvent)
        f (True, _)  True  = (True, NoEvent)

sample :: SF (a, Event b) (Event (a, b))
sample = arr (\(a, e) -> event NoEvent (\b -> Event (a, b)) e)

sample_ :: SF (a, Event b) (Event a)
sample_ = sample >>> arr (fmap fst)

-- ====================================================================
-- Accumulators

-- | Accumulate from an initial value and an update event.
accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = SF (tf a0)
    where
        tf a NoEvent   = (NoEvent, SF (tf a))
        tf a (Event f) = let a' = f a in a' `seq` (Event a', SF (tf a'))

accumHold :: a -> SF (Event (a -> a)) a
accumHold a0 = scanl g a0
    where
        g a NoEvent   = a
        g a (Event f) = f a

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
-- Switching combinators

-- | Lazy (delayed) switch.
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch sf f = SF (g sf)
    where
        g sf a = case runSF sf a of
                    ((b, NoEvent), sf') -> (b, SF (g sf'))
                    ((b, Event c), _)   -> (b, f c)

-- | Eager (immediate) switch.
switch' :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch' sf f = SF (g sf)
    where
        g sf a = case runSF sf a of
                    ((b, NoEvent), sf') -> (b, SF (g sf'))
                    ((_, Event c), _)   -> runSF (f c) a

-- | Recurring switch.
rswitch :: SF a b -> SF (a, Event (SF a b)) b
rswitch sf = switch (first sf) ((second (const NoEvent) >=-) . rswitch)

-- | Recurring switch.
rswitch' :: SF a b -> SF (a, Event (SF a b)) b
rswitch' sf = switch' (first sf) ((second (const NoEvent) >=-) . rswitch')
