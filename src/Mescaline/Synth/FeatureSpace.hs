module Mescaline.Synth.FeatureSpace (
) where

import qualified Control.Monad.State as State
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.BKTree (BKTree)
import qualified Data.Set.BKTree as BKTree
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified System.Random as Random

newtype Unit = Unit (Unit.Unit, Feature.Feature) deriving (Show)

unit :: Unit -> Unit.Unit
unit (Unit (u, _)) = u

feature :: Unit -> Feature.Feature
feature (Unit (_, f)) = f

value :: Unit -> Feature.Value
value = Feature.value . feature

precision :: Double
precision = 100

withPrecision :: Double -> Int
withPrecision = round . (*) precision

instance Eq (Unit) where
    a == b = f a == f b
        where
            f :: Unit -> SV.Vector Int
            f = V.map withPrecision . value

instance BKTree.Metric Unit where
    -- Euclidian distance. Note: Assuming that feature dimensions are scaled to [0,1].
    distance a b = withPrecision d
        where
            x = V.zipWith (-) (value a) (value b)
            d = sqrt (V.foldl (+) 0 (V.zipWith (*) x x))

data Region = Region {
    center :: Feature.Value -- Circle center in feature space
  , radius :: Double        -- Circle radius in feature space  
  } deriving (Eq, Show)   

data FeatureSpace = FeatureSpace { 
    units :: BKTree Unit
  , randomGen :: Random.StdGen
  , regions :: IntMap Region
  , activeUnits :: Set Unit.Unit
  }

fromList :: [(Unit.Unit, Feature.Feature)] -> Random.StdGen -> FeatureSpace
fromList = undefined

addRegion :: Int -> Region -> FeatureSpace -> FeatureSpace
addRegion = undefined

-- Return the next random unit from region i and an updated FeatureSpace.
activateUnit :: Int -> FeatureSpace -> (Unit, FeatureSpace)
activateUnit i f = (u'', f { randomGen = g', activeUnits = Set.insert (unit u'') (activeUnits f) })
    where
        Just r = IntMap.lookup i (regions f)
        Unit (u, Feature.Feature (uid, d, v)) = head (BKTree.elems (units f))
        u' = Unit (u, Feature.cons uid d (center r))
        n = withPrecision (radius r)
        us = BKTree.elemsDistance n u' (units f)
        (j, g') = Random.randomR (0, length us - 1) (randomGen f)
        u'' = us !! j

activateUnits :: [Int] -> FeatureSpace -> ([Unit], FeatureSpace)
activateUnits = State.runState . sequence . map (State.State . activateUnit)

deactivateUnit :: Unit.Unit -> FeatureSpace -> FeatureSpace
deactivateUnit u f = f { activeUnits = Set.delete u (activeUnits f) }
