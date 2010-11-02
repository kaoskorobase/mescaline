module Mescaline.Synth.FeatureSpace.Model (
    Unit
  , RegionId
  , Region
  , mkRegion
  , defaultRegions
  , regionId
  , center
  , radius
  , minRadius
  , maxRadius
  , FeatureSpace
  , UnitSet
  , unitSet
  , randomGen
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
  , regionUnits
  , activateRegion
  , activateRegions
) where

import           Control.Arrow (first)
import qualified Control.Monad.State as State
import           Data.Complex
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.BKTree (BKTree)
import qualified Data.Set.BKTree as BKTree
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace.Unit (Unit)
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import qualified System.Random as Random

type UnitSet = BKTree Unit

-- FIXME: These instances should be defined on a `UnitView' instead, i.e. some kind
-- of lense that is defined on a specific feature (index)

instance Eq (Unit) where
    a == b = f a == f b
        where
            {-# INLINE f #-}
            f :: Unit -> SV.Vector Int
            f = V.map withPrecision . Unit.value 0 -- FIXME

instance BKTree.Metric Unit where
    -- Euclidian distance. Note: Assuming that feature dimensions are scaled to [0,1].
    distance a b = withPrecision d
        where
            x = V.zipWith (-) (Unit.value 0 a) (Unit.value 0 b) -- FIXME
            d = sqrt (V.foldl (+) 0 (V.zipWith (*) x x))

precision :: Double
precision = fromIntegral (maxBound :: Int)

{-# INLINE withPrecision #-}
withPrecision :: Double -> Int
withPrecision = truncate . (*) precision

type RegionId = Int

data Region = Region {
    regionId :: RegionId
  , center   :: Feature.Value -- Circle center in feature space
  , radius   :: Double        -- Circle radius in feature space  
  } deriving (Eq, Show)   

data FeatureSpace = FeatureSpace { 
    unitSet     :: !UnitSet
  , randomGen   :: Random.StdGen
  , regions     :: IntMap Region
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

defaultRegions :: [Region]
defaultRegions = zipWith (\i v -> mkRegion i v radius) [0..n-1] centers
    where
        n = 8
        radius = 0.025
        phis = take n $ iterate (+2*pi/fromIntegral n) 0
        centers = map (c2v . mkPolar (radius*3)) phis
        offset = 0.5
        c2v c = V.fromList [offset + realPart c, offset + imagPart c]

units :: FeatureSpace -> [Unit]
units = BKTree.elems . unitSet

setFeatureSpace :: FeatureSpace -> [Unit.Unit] -> FeatureSpace
setFeatureSpace f us = f { unitSet = BKTree.fromList us }

fromList :: Random.StdGen -> [Unit.Unit] -> FeatureSpace
fromList g us = FeatureSpace (BKTree.fromList us) g IntMap.empty

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

-- | Return a list of all units contained in a particular region.
regionUnits :: RegionId -> FeatureSpace -> [Unit]
regionUnits i f =
    case IntMap.lookup i (regions f) of
        Nothing -> []
        Just r  ->
            case BKTree.elems (unitSet f) of
                (u:_) ->
                    let u' = Unit.withValues u [center r]
                        n  = withPrecision (radius r)
                    in BKTree.elemsDistance n u' (unitSet f)
                [] -> []

-- Return the next random unit from region i and an updated FeatureSpace.
activateRegion :: RegionId -> FeatureSpace -> (Maybe Unit, FeatureSpace)
activateRegion i f = (u, f { randomGen = g' })
    where
        us      = regionUnits i f
        (j, g') = Random.randomR (0, length us - 1) (randomGen f)
        u       = if null us then Nothing else Just (us !! j)

filterMaybe :: [Maybe a] -> [a]
filterMaybe l = [ x | Just x <- l ]

activateRegions :: [RegionId] -> FeatureSpace -> ([Unit], FeatureSpace)
activateRegions is f = first filterMaybe $ State.runState (sequence (map (State.State . activateRegion) is)) f
