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
  , SSF
  , lift
  , realTime
  , logicalTime
  , executeSSF
) where

import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer (lift)
import           Control.Arrow.Transformer.Reader
import           Control.Monad (liftM)
import           Control.Concurrent.Chan
import           Mescaline (Time)
import           Mescaline.Synth.SF (Event(..), noEvent, event)
import qualified Mescaline.Synth.SF as SF
import qualified Sound.OpenSoundControl as OSC
import           Prelude hiding (filter, init, scanl)

-- ====================================================================
-- Signal functions

-- | Identity: identity = arr id
identity :: SSF a a
identity = lift SF.identity

-- | Identity: constant b = arr (const b)
constant :: b -> SSF a b
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
initially :: a -> SSF a a
initially a = lift (SF.initially a)

-- | Replace event value.
tag :: b -> SSF (Event a) (Event b)
tag b = lift (SF.tag b)

-- | Filter out events that don't satisfy some predicate.
filter :: (a -> Bool) -> SSF (Event a) (Event a)
filter p = lift (SF.filter p)

-- | Zero order hold.
hold :: a -> SSF (Event a) a
hold a0 = lift (SF.hold a0)

-- | Accumulate from an initial value and an update event.
accum :: a -> SSF (Event (a -> a)) (Event a)
accum a0 = lift (SF.accum a0)

scanl :: (b -> a -> b) -> b -> SSF a b
scanl f b0 = lift (SF.scanl f b0)

edge :: SSF Bool (Event ())
edge = lift SF.edge

-- ====================================================================
-- Sampled Signal functions

newtype Env = Env { getEnv :: (Time, Time) }

type SSF = ReaderArrow Env SF.SF

realTime :: SSF a Time
realTime = readState >>> arr (fst.getEnv)

logicalTime :: SSF a Time
logicalTime = readState >>> arr (snd.getEnv)

-- ====================================================================
-- Driver

executeSSF :: Double -> SSF (Event a) b -> Chan a -> Chan ((Time,Time), b) -> IO ()
executeSSF tick ssf ichan ochan = OSC.utcr >>= loop (runReader ssf)
    where
        loop sf lt = do
            empty <- isEmptyChan ichan
            a <- if empty then return NoEvent else liftM Event (readChan ichan)
            t <- OSC.utcr
            let e = Env (t, lt)
                (b, sf') = SF.runSF sf (a, e)
                lt' = lt + tick
            b `seq` writeChan ochan (getEnv e, b)
            OSC.pauseThreadUntil lt'
            loop sf' lt'
