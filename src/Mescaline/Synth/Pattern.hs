{-# LANGUAGE DeriveDataTypeable #-}

module Mescaline.Synth.Pattern where

import           Data.List (scanl1)
import           Data.Typeable (Typeable)
import           Debug.Trace (traceShow)
import qualified Sound.SC3.Lang.Pattern as P
import           Mescaline.Database (Database)
import qualified Mescaline.Database.Unit as Unit

import           Sound.OpenSoundControl

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

data SynthParams = SynthParams {
    attackTime   :: Double,
    releaseTime  :: Double,
    sustainLevel :: Double,
    gateLevel    :: Double,
    latency      :: Double
} deriving (Eq, Show, Typeable)

defaultSynth :: SynthParams
defaultSynth = SynthParams {
    attackTime   = 0,
    releaseTime  = 0,
    sustainLevel = 1,
    gateLevel    = 0,
    latency      = 0.2
}

attackTime_ :: Double -> SynthParams -> SynthParams
attackTime_ x p = p { attackTime = x }

releaseTime_ :: Double -> SynthParams -> SynthParams
releaseTime_ x p = p { releaseTime = x }

amp_ :: Double -> SynthParams -> SynthParams
amp_ x p = p { sustainLevel = x }

latency_ :: Double -> SynthParams -> SynthParams
latency_ x p = p { latency = x }

data Event = Event {
    delta :: Double,
    unit  :: Unit.Unit,
    synth :: SynthParams
} deriving (Eq, Show, Typeable)

type Pattern  = P.P Event
newtype PCons = PCons (Database -> Pattern) deriving (Typeable)

pattern :: (Database -> Pattern) -> PCons
pattern = PCons

event :: Unit.Unit -> Event
event u = Event {
    delta = Unit.duration u,
    unit  = u,
    synth = defaultSynth
}

pevent :: Unit.Unit -> Pattern
pevent = return . event

ptrace :: Show a => P.P a -> P.P a
ptrace = fmap g
    where g a = traceShow a a

execute :: (Event -> Double -> IO ()) -> Pattern -> IO ()
execute f p = do
    t0 <- utcr
    let es = P.evalP 0 p
        ts = (map (+t0) . scanl1 (+) . map delta) es
    -- sequence_ (zipWith g es ts)
    loop es t0
    where
        loop [] _     = return ()
        loop (e:es) t = do
            f e t
            let t' = t + delta e
            pauseThreadUntil t'
            loop es t'
