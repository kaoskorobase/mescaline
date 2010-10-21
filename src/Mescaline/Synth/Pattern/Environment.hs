{-# LANGUAGE CPP #-}

#include "Accessor.h"

module Mescaline.Synth.Pattern.Environment (
    Environment
  , mkEnvironment
  , featureSpace
  , TrackId
) where

import           Data.Accessor
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified System.Random as Random

type TrackId = Int

data Environment = Environment {
    _randomGen    :: Random.StdGen
  , _featureSpace :: FeatureSpace.FeatureSpace
  }

instance Random.RandomGen Environment where
    next e  = let (i, g) = Random.next (_randomGen e)
              in (i, e { _randomGen = g })
    split e = let (g, g') = Random.split (_randomGen e)
              in (e { _randomGen = g }, e { _randomGen = g' })

mkEnvironment :: Int -> FeatureSpace.FeatureSpace -> Environment
mkEnvironment seed = Environment (Random.mkStdGen seed)

ACCESSOR(featureSpace, _featureSpace, Environment, FeatureSpace.FeatureSpace)

-- data EnvironmentSink = EnvironmentSink {
--     es_tracks  :: Map TrackId (Chan TrackData)
--   , es_ctrls   :: Map Ctrl (Chan Double)
--   , es_seconds :: Chan Time
--   , es_beats   :: Chan Time
-- }
-- 
-- data Environment = Environment {
--     tracks  :: Map TrackId [TrackData]
--   , ctrls   :: Map Ctrl [Double]
--   , seconds :: [Time]
--   , beats   :: [Time]
--   -- Feature space: list of feature spaces
--   }
-- 
-- data TrackId = Track1
--              | Track2
--              | Track3
--              | Track4
--              | Track5
--              | Track6
--              | Track7
--              | Track8
--              deriving (Eq, Ord, Read, Show)
-- 
-- data Ctrl = Ctrl1
--           | Ctrl2
--           | Ctrl3
--           | Ctrl4
--           deriving (Eq, Ord, Read, Show)
-- 
-- data TrackData = TrackData Int Int Int [Maybe Double] deriving (Eq, Show)
-- data Region    = M | E | S | C | A | L | I | N deriving (Eq, Show)
-- 
-- mkEnvironment :: [TrackId] -> [Ctrl] -> IO (Environment, EnvironmentSink)
-- mkEnvironment ts cs = do
--     es_tracks  <- Map.fromList `fmap` mapM (\k -> newChan >>= \c -> return (k, c)) ts
--     es_ctrls   <- Map.fromList `fmap` mapM (\k -> newChan >>= \c -> return (k, c)) cs
--     es_seconds <- newChan
--     es_beats   <- newChan
--     e_tracks   <- forM es_tracks getChanContents
--     e_ctrls    <- forM es_ctrls getChanContents
--     e_seconds  <- getChanContents es_seconds
--     e_beats    <- getChanContents es_beats
--     return (
--         Environment e_tracks e_ctrls e_seconds e_beats
--       , EnvironmentSink es_tracks es_ctrls es_seconds es_beats )
