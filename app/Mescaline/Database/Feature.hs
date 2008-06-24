module Mescaline.Database.Feature (
    FeatureDescriptor(..),
    FeatureData(..),
    Feature(..),
    Vector
) where

import qualified Data.ByteString.Lazy as B

data FeatureDescriptor = FeatureDescriptor {
    id :: Int,
    name :: String
} deriving (Show)


type Vector a = [a]

data FeatureData = FeatureData {
    sfid :: Int,
    uid :: Int,
    onset_time :: Double,
    chunck_length :: Double,
    feature_id :: Int, 
    intval :: Maybe Int, 
    realval :: Maybe Double, 
    textval :: Maybe String, 
    arrayval :: Maybe (Vector Double)
} deriving (Show)

data Feature =
    AvgChroma (Vector Double)
 |  AvgChromaScalar Double
 |  AvgChunkPower Double
 |  AvgFreqSimple Double
 |  AvgMelSpec (Vector Double)
 |  AvgMFCC (Vector Double)
 |  AvgPitchSimple Double
 |  AvgSpec (Vector Double)
 |  AvgSpecCentroid Double
 |  AvgSpecFlatness Double
