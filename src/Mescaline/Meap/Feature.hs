module Mescaline.Meap.Feature (
	Type(..),
	Feature(..),
	FeatureMap,
	extract
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Mescaline.Data.ListReader (ListReader, get, runListReader)

data Type = 
    AvgChroma
  | AvgChromaScalar
  | AvgChunkPower
  | AvgFreqSimple
  | AvgMelSpec
  | AvgMFCC
  | AvgPitchSimple
  | AvgSpec
  | AvgSpecCentroid
  | AvgSpecFlatness
  | Likelihood
  | SpectralStability
  deriving (Eq, Ord, Show)

data Feature a = Feature {
    featureType :: Type,
    reader      :: ListReader Double a
}

data Value = ScalarValue Double | VectorValue [Double]

type FeatureMap = Map Type Value

getScalar :: ListReader Double Value
getScalar = (ScalarValue . head) `fmap` get 1

getVector :: Int -> ListReader Double Value
getVector = fmap VectorValue . get

featureExtractors :: Map Type (ListReader Double Value)
featureExtractors = Map.fromList [
    ( AvgChroma             , getVector 12  ),
    ( AvgChromaScalar       , getScalar     ),
    ( AvgChunkPower         , getScalar     ),
    ( AvgFreqSimple         , getScalar     ),
    ( AvgMelSpec            , getVector 40  ),
    ( AvgMFCC               , getVector 12  ),
    ( AvgPitchSimple        , getScalar     ),
    ( AvgSpec               , getVector 512 ),
    ( AvgSpecCentroid       , getScalar     ),
    ( AvgSpecFlatness       , getScalar     ),
    ( Likelihood            , getScalar     ),
    ( SpectralStability     , getScalar     )
    ]

featureValues :: [Type] -> ListReader Double [Value]
featureValues = mapM (fromJust . flip Map.lookup featureExtractors)

extract :: [Type] -> [Double] -> Either String (Map Type Value)
extract types list =
    case runListReader (featureValues types) list of
        Left e   -> Left e
        Right vs -> Right $ Map.fromList (zip types vs)
