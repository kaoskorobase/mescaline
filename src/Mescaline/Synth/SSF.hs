{-# LANGUAGE Arrows #-}
module Mescaline.Synth.SSF (
    Event(..)
  , noEvent
  , event
  , identity
  , constant
  , initially
  , tag
  , filter
  , hold
  , accum
  , scanl
  , edge
  , SF
  , lift
  , realTime
  , logicalTime
  , execute
) where

import           Control.Arrow
import qualified Control.Arrow.Operations as State
import           Control.Arrow.Transformer (lift)
import qualified Control.Arrow.Transformer.State as State
import           Control.Monad (liftM)
import           Control.Concurrent.Chan.Chunked
import qualified Data.List as List
import           Mescaline (Time)
import           Mescaline.Synth.SF (Event(..), noEvent, event)
import qualified Mescaline.Synth.SF as SF
import qualified Sound.OpenSoundControl as OSC
import           Prelude hiding (filter, init, scanl)

-- ====================================================================
-- Signal functions

-- | Identity: identity = arr id
identity :: SF a a
identity = lift SF.identity

-- | Identity: constant b = arr (const b)
constant :: b -> SF a b
constant b = lift (SF.constant b)

-- -- | Transform initial output value.
-- (-=>) :: (b -> b) -> SSF a b -> SSF a b
-- (-=>) = lift (SF.-=>)
-- 
-- -- | Override initial output value.
-- -- Initialization operator (cf. Lustre/Lucid Synchrone).
-- (-->) :: b -> SSF a b -> SSF a b
-- -- (-->) b0 (SF tf) = SF (\a0 -> (b0, fst (tf a0)))
-- (-->) = lift SF.(-->)
-- 
-- -- | Transform initial input value.
-- (>=-) :: (a -> a) -> SSF a b -> SSF a b
-- (>=-) f = lift (SF.(>=-) f)
-- 
-- -- | Override initial input value.
-- (>--) :: a -> SSF a b -> SSF a b
-- (>--) a0 = lift (SF.(>--) a0)

-- Override initial value of input signal.
initially :: a -> SF a a
initially a = lift (SF.initially a)

-- | Replace event value.
tag :: b -> SF (Event a) (Event b)
tag b = lift (SF.tag b)

-- | Filter out events that don't satisfy some predicate.
filter :: (a -> Bool) -> SF (Event a) (Event a)
filter p = lift (SF.filter p)

-- | Zero order hold.
hold :: a -> SF (Event a) a
hold a0 = lift (SF.hold a0)

-- | Accumulate from an initial value and an update event.
accum :: a -> SF (Event (a -> a)) (Event a)
accum a0 = lift (SF.accum a0)

scanl :: (b -> a -> b) -> b -> SF a b
scanl f b0 = lift (SF.scanl f b0)

edge :: SF Bool (Event ())
edge = lift SF.edge

-- ====================================================================
-- Sampled Signal functions

data State = State {
    s_realTime    :: !Time
  , s_logicalTime :: !Time
} deriving (Eq, Show)

type SF = State.StateArrow State SF.SF

realTime :: SF a Time
realTime = State.fetch >>> arr s_realTime

logicalTime :: SF a Time
logicalTime = State.fetch >>> arr s_logicalTime

-- ====================================================================
-- Driver

execute :: Double -> SF (Event a) b -> Chan a -> Chan ((Time,Time), b) -> IO ()
execute tick ssf ichan ochan = do
    t <- OSC.utcr
    loop (State t t) (State.elimState ssf)
    where
        loop state0 sf = do
            empty <- isEmptyChan ichan
            as <- if empty then return [NoEvent] else map Event `fmap` readChanAvailable ichan
            rt <- OSC.utcr
            let state1 = state0 { s_realTime = rt }
                ((bs, state2), sf') = List.foldl'
                                        (\((bs, s), sf) a -> let ((b, s'), sf') = SF.runSF sf (a, s)
                                                             in b `seq` s' `seq` ((b:bs, s'), sf'))
                                        (([], state1), sf)
                                        as
            -- let ((b, state'), sf') = SF.runSF sf (a, state { s_realTime = rt })
            -- b `seq` state' `seq` writeChan ochan ((s_realTime state', s_logicalTime state'), b)
            writeList2Chan ochan $ map ((,) (s_realTime state2, s_logicalTime state2)) bs
            let state3 = state2 { s_logicalTime = s_logicalTime state2 + tick }
            OSC.pauseThreadUntil (s_logicalTime state3)
            state3 `seq` loop state3 sf'
