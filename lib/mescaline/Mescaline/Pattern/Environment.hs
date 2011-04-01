{-# LANGUAGE CPP #-}

#include "Accessor.h"

module Mescaline.Pattern.Environment (
    Environment
  , mkEnvironment
  , region
  , bindings
  , featureSpace
  , sequencer
  , logMessage
  , getMessages
) where

import           Data.Accessor
import           Mescaline.Pattern.Binding (Bindings)
import qualified Mescaline.Pattern.Sequencer as Sequencer
import qualified Mescaline.FeatureSpace.Model as FeatureSpace
import qualified System.Random as Random

data Environment = Environment {
    _region       :: Int
  , _randomGen    :: !Random.StdGen
  , _messages     :: [String]
  , _bindings     :: Bindings Environment
  , _featureSpace :: !FeatureSpace.FeatureSpace
  , _sequencer    :: !Sequencer.Sequencer
  }

instance Random.RandomGen Environment where
    next e  = let (i, g) = Random.next (_randomGen e)
              in (i, e { _randomGen = g })
    split e = let (g, g') = Random.split (_randomGen e)
              in (e { _randomGen = g }, e { _randomGen = g' })

mkEnvironment :: Int -> Int -> Bindings Environment -> FeatureSpace.FeatureSpace -> Sequencer.Sequencer -> Environment
mkEnvironment seed region = Environment region (Random.mkStdGen seed) []

ACCESSOR(region,       _region,       Environment, Int)
ACCESSOR(bindings,     _bindings,     Environment, Bindings Environment)
ACCESSOR(featureSpace, _featureSpace, Environment, FeatureSpace.FeatureSpace)
ACCESSOR(sequencer,    _sequencer,    Environment, Sequencer.Sequencer)

logMessage :: String -> Environment -> Environment
logMessage msg e = e { _messages = msg : _messages e }

getMessages :: Environment -> ([String], Environment)
getMessages e = (reverse (_messages e), e { _messages = [] })
