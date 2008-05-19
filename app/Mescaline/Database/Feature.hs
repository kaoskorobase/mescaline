module Mescaline.Database.Feature where

data FeatureDescriptor = FeatureDescriptor {
    id :: Int,
    name :: String
} deriving (Show)

type Vector a = [a]

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
