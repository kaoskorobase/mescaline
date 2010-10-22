{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Synth.Pattern.Patch (
    Pattern
  , Patch
  , fromPattern
  , pattern
) where

import           Control.Applicative
import           Data.Accessor
import           Data.Typeable
import           Mescaline.Synth.Pattern.Environment
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern
import qualified Mescaline.Time as Time

data Patch = Patch { pattern :: Pattern Event } deriving (Typeable)

fromPattern :: Pattern Event -> Patch
fromPattern = Patch

-- instance Time.HasDuration (TrackId, Event) where
--     duration = accessor (\(_, e) -> getVal duration e) (\d (t, e) -> (t, setVal duration d e))

-- toPattern :: Patch -> Pattern (TrackId, Event)
-- toPattern (Patch ps) = ppar ps'
--     where
--         ps' = map (\(t, p) -> pzip (pure t) p) ps
