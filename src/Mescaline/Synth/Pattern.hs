{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Mescaline.Synth.Pattern where

import           Data.Accessor
import           Data.Accessor.Template
import           Data.List (scanl1)
import           Data.Typeable (Typeable)
import           Debug.Trace (traceShow)
import qualified Sound.SC3.Lang.Pattern as P
import           Mescaline.Database (Database)
import qualified Mescaline.Database.Unit as Unit

import           Sound.OpenSoundControl


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
} deriving (Eq, Show, Typeable)

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

data Event = Event {
    delta_ :: Double,
    unit_  :: Unit.Unit,
    synth_ :: SynthParams
} deriving (Eq, Show, Typeable)

$(deriveAccessors ''Event)

type Pattern  = P.P Event
newtype PCons = PCons (Database -> Pattern) deriving (Typeable)

pattern :: (Database -> Pattern) -> PCons
pattern = PCons

event :: Unit.Unit -> Event
event u = Event {
    delta_ = Unit.duration u,
    unit_  = u,
    synth_ = defaultSynth
}

pevent :: Unit.Unit -> Pattern
pevent = return . event

ptrace :: Show a => P.P a -> P.P a
ptrace = fmap f
    where f a = traceShow a a

execute :: (Event -> Double -> IO ()) -> Pattern -> IO ()
execute f p = do
    t0 <- utcr
    let es = P.evalP 0 p
        ts = (map (+t0) . scanl1 (+) . map (getVal delta)) es
    -- sequence_ (zipWith g es ts)
    loop es t0
    where
        loop [] _     = return ()
        loop (e:es) t = do
            f e t
            let t' = t + getVal delta e
            pauseThreadUntil t'
            loop es t'
