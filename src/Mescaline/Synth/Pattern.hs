{-# LANGUAGE DeriveDataTypeable #-}

module Mescaline.Synth.Pattern where

import           Data.Typeable (Typeable)
import qualified Sound.SC3.Lang.Pattern as P
import           Mescaline.Database (Database)
import qualified Mescaline.Database.Unit as Unit

import           Sound.OpenSoundControl

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

data Event = Event {
    delta :: Double,
    unit :: Unit.Unit
} deriving (Eq, Show, Typeable)

type Pattern  = P.P Event
newtype PCons = PCons (Database -> Pattern) deriving (Typeable)

pattern :: (Database -> Pattern) -> PCons
pattern = PCons

event :: Unit.Unit -> Event
event u = Event (Unit.duration u) u

pevent :: Unit.Unit -> Pattern
pevent = return . event

execute :: (Event -> Double -> IO ()) -> Pattern -> IO ()
execute f p = do
    t0 <- utcr
    let es = P.evalP 0 p
        ts = scanl (\t e -> t + delta e) t0 es
    sequence_ (zipWith g es ts)
    where g e t = f e t >> pauseThreadUntil t
