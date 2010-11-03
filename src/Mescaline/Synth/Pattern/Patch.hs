{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Synth.Pattern.Patch (
    Patch
  , cons
  , mkDefault
  , pattern
  , sequencer
  , regions
) where

import           Control.Applicative
import           Data.Accessor
import           Data.Typeable
import           Mescaline.Synth.FeatureSpace.Model (Region, defaultRegions)
import           Mescaline.Synth.Pattern
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import qualified Mescaline.Time as Time

data Patch = Patch {
    tracks    :: Int -> Pattern () (Maybe Event)
  , sequencer :: Sequencer
  , regions   :: [Region]
  } deriving (Typeable)

cons :: (Int -> Pattern () (Maybe Event)) -> Sequencer -> [Region] -> Patch
cons = Patch

mkDefault :: (Int -> Pattern () (Maybe Event)) -> Patch
mkDefault f = cons f (Sequencer.empty n n) regions
    where
        regions = defaultRegions
        n       = length regions

pattern :: Patch -> Pattern () (Maybe Event)
pattern patch = par (map (tracks patch) [0..n-1])
    where n = Sequencer.rows (sequencer patch)
