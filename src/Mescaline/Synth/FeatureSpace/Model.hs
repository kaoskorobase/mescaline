module Mescaline.Synth.FeatureSpace.Model (
    Unit
  , unit
  , feature
  , value
  , RegionId
  , Region
  , mkRegion
  , regionId
  , center
  , radius
  , minRadius
  , maxRadius
  , FeatureSpace
  , randomGen
  , activeUnits
  , units
  , setFeatureSpace
  , fromList
  , nextRegionId
  , addRegion
  , deleteRegion
  -- , updateRegionById
  , updateRegion
  , regions
  , regionList
  , activateRegion
  , activateRegions
  , activateUnit
  , deactivateUnit
) where

import           Control.Arrow (first)
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
precision = fromIntegral (maxBound :: Int)

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

type RegionId = Int

data Region = Region {
    regionId :: RegionId
  , center   :: Feature.Value -- Circle center in feature space
  , radius   :: Double        -- Circle radius in feature space  
  } deriving (Eq, Show)   

data FeatureSpace = FeatureSpace { 
    featureSpace :: BKTree Unit
  , randomGen    :: Random.StdGen
  , regions      :: IntMap Region
  , activeUnits  :: Set Unit.Unit
  }

minPos :: Double
minPos = 0

maxPos :: Double
maxPos = 1

minRadius :: Double
minRadius = 1e-9

maxRadius :: Double
maxRadius = 0.8

clip :: Double -> Double -> Double -> Double
clip lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x

mkRegion :: RegionId -> Feature.Value -> Double -> Region
mkRegion i c r = Region i (V.fromList [clip minPos maxPos (c V.! 0), clip minPos maxPos (c V.! 1)])
                          (clip minRadius maxRadius r)

units :: FeatureSpace -> [Unit]
units = BKTree.elems . featureSpace

setFeatureSpace :: FeatureSpace -> [(Unit.Unit, Feature.Feature)] -> FeatureSpace
setFeatureSpace f us = f { featureSpace = BKTree.fromList (map Unit us) }

fromList :: Random.StdGen -> [(Unit.Unit, Feature.Feature)] -> FeatureSpace
fromList g us = FeatureSpace (BKTree.fromList (map Unit us)) g IntMap.empty Set.empty

nextRegionId :: FeatureSpace -> Int
nextRegionId f
    | null ids  = 0
    | otherwise = loop (last ids + 1)
    where
        ids = IntMap.keys (regions f)
        loop i
            | not (i `elem` ids) = i
            | otherwise          = loop (i+1)

addRegion :: Region -> FeatureSpace -> FeatureSpace
addRegion r f = f { regions = IntMap.insert (regionId r) r (regions f) }

deleteRegionById :: RegionId -> FeatureSpace -> FeatureSpace
deleteRegionById i f = f { regions = IntMap.delete i (regions f) }

deleteRegion :: Region -> FeatureSpace -> FeatureSpace
deleteRegion r = deleteRegionById (regionId r)

updateRegionById :: RegionId -> (Region -> Region) -> FeatureSpace -> FeatureSpace
updateRegionById i f fs = fs { regions = IntMap.update (Just . f) i (regions fs) }

updateRegion :: Region -> FeatureSpace -> FeatureSpace
updateRegion r = updateRegionById (regionId r) (const r)

regionList :: FeatureSpace -> [Region]
regionList = IntMap.elems . regions

-- Return the next random unit from region i and an updated FeatureSpace.
activateRegion :: RegionId -> FeatureSpace -> (Maybe Unit, FeatureSpace)
activateRegion i f = (u'', au (f { randomGen = g' }))
    where
        Just r = IntMap.lookup i (regions f)
        Unit (u, Feature.Feature (uid, d, _)) = head (BKTree.elems (featureSpace f))
        u' = Unit (u, Feature.cons uid d (center r))
        n = withPrecision (radius r)
        us = BKTree.elemsDistance n u' (featureSpace f)
        (j, g') = Random.randomR (0, length us - 1) (randomGen f)
        u'' = if null us then Nothing else Just (us !! j)
        au f = maybe f (flip activateUnit f . unit) u''

filterMaybe :: [Maybe a] -> [a]
filterMaybe l = [ x | Just x <- l ]

activateRegions :: [RegionId] -> FeatureSpace -> ([Unit], FeatureSpace)
activateRegions is f = first filterMaybe $ State.runState (sequence (map (State.State . activateRegion) is)) f

activateUnit :: Unit.Unit -> FeatureSpace -> FeatureSpace
activateUnit u f = f { activeUnits = Set.insert u (activeUnits f) }

deactivateUnit :: Unit.Unit -> FeatureSpace -> FeatureSpace
deactivateUnit u f = f { activeUnits = Set.delete u (activeUnits f) }
