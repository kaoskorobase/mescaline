{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Synth.Pattern.Patch (
    Pattern
  , Patch(..)
  , toPattern
  , module Mescaline.Synth.Pattern.Environment
  , module Mescaline.Synth.Pattern.Event
) where

import           Control.Applicative
import           Data.Accessor
import           Data.Typeable
import           Mescaline.Synth.Pattern.Environment
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern.Ppar (ppar)
import qualified Mescaline.Synth.Pattern.Step as P
import qualified Mescaline.Time as Time

type Pattern = P.P Environment
data Patch = Patch [(TrackId, Pattern Event)] deriving (Typeable)

instance Time.HasDuration (TrackId, Event) where
    duration = accessor (\(_, e) -> getVal duration e) (\d (t, e) -> (t, setVal duration d e))

toPattern :: Patch -> Pattern (TrackId, Event)
toPattern (Patch ps) = ppar ps'
    where
        ps' = map (\(t, p) -> P.pzip (pure t) p) ps
