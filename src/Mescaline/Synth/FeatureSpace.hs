module Mescaline.Synth.FeatureSpace (
    Unit
  , unit
  , feature
  , value
  , Region(..)
  , FeatureSpace
  , activeUnits
  , units
  , fromList
  , insertRegion
  , deleteRegion
  , updateRegion
  , regions
  , regionList
  , activateRegion
  , activateRegions
  , deactivateUnit
) where

import           Control.Arrow
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
    featureSpace :: BKTree Unit
  , randomGen :: Random.StdGen
  , regions :: IntMap Region
  , activeUnits :: Set Unit.Unit
  }

units :: FeatureSpace -> [Unit]
units = BKTree.elems . featureSpace

fromList :: [(Unit.Unit, Feature.Feature)] -> Random.StdGen -> FeatureSpace
fromList us g = FeatureSpace (BKTree.fromList (map Unit us)) g IntMap.empty Set.empty

insertRegion :: Int -> Region -> FeatureSpace -> FeatureSpace
insertRegion i r f = f { regions = IntMap.insert i r (regions f) }

deleteRegion :: Int -> FeatureSpace -> FeatureSpace
deleteRegion i f = f { regions = IntMap.delete i (regions f) }

updateRegion :: Int -> (Region -> Region) -> FeatureSpace -> FeatureSpace
updateRegion i f fs = fs { regions = IntMap.update (Just . f) i (regions fs) }
-- update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap aSource

regionList :: FeatureSpace -> [Region]
regionList = IntMap.elems . regions

-- Return the next random unit from region i and an updated FeatureSpace.
activateRegion :: Int -> FeatureSpace -> (Maybe Unit, FeatureSpace)
activateRegion i f = (u'', f { randomGen = g', activeUnits = au })
    where
        Just r = IntMap.lookup i (regions f)
        Unit (u, Feature.Feature (uid, d, _)) = head (BKTree.elems (featureSpace f))
        u' = Unit (u, Feature.cons uid d (center r))
        n = withPrecision (radius r)
        us = BKTree.elemsDistance n u' (featureSpace f)
        (j, g') = Random.randomR (0, length us - 1) (randomGen f)
        u'' = if null us then Nothing else Just (us !! j)
        au = maybe (activeUnits f) (flip Set.insert (activeUnits f) . unit) u''

filterMaybe :: [Maybe a] -> [a]
filterMaybe l = [ x | Just x <- l ]

activateRegions :: [Int] -> FeatureSpace -> ([Unit], FeatureSpace)
activateRegions is f = first filterMaybe $ State.runState (sequence (map (State.State . activateRegion) is)) f

deactivateUnit :: Unit.Unit -> FeatureSpace -> FeatureSpace
deactivateUnit u f = f { activeUnits = Set.delete u (activeUnits f) }
