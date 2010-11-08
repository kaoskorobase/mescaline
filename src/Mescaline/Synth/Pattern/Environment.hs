{-# LANGUAGE CPP #-}

#include "Accessor.h"

module Mescaline.Synth.Pattern.Environment (
    Environment
  , mkEnvironment
  , featureSpace
  , sequencer
) where

import           Data.Accessor
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified System.Random as Random

data Environment = Environment {
    _randomGen    :: !Random.StdGen
  , _event        :: Maybe Event.Event
  , _featureSpace :: !FeatureSpace.FeatureSpace
  , _sequencer    :: !Sequencer.Sequencer
  }

instance Random.RandomGen Environment where
    next e  = let (i, g) = Random.next (_randomGen e)
              in (i, e { _randomGen = g })
    split e = let (g, g') = Random.split (_randomGen e)
              in (e { _randomGen = g }, e { _randomGen = g' })

mkEnvironment :: Int -> FeatureSpace.FeatureSpace -> Sequencer.Sequencer -> Environment
mkEnvironment seed = Environment (Random.mkStdGen seed) Nothing

ACCESSOR(event,        _event,        Environment, Maybe Event.Event)
ACCESSOR(featureSpace, _featureSpace, Environment, FeatureSpace.FeatureSpace)
ACCESSOR(sequencer,    _sequencer,    Environment, Sequencer.Sequencer)
