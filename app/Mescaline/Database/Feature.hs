module Mescaline.Database.Feature (
    FeatureDescriptor(..),
    Feature(..),
    Vector
) where

import qualified Data.ByteString.Lazy as B
import Mescaline.Database.Unique as Unique
import Prelude hiding (id)

data FeatureDescriptor = FeatureDescriptor {
    id   :: Id,
    name :: String
} deriving (Show)

instance Unique (FeatureDescriptor) where
    uid = id

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
 deriving (Show)
 