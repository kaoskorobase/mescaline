{-# LANGUAGE BangPatterns, CPP, FlexibleContexts #-}
module Mescaline.FeatureSpace.Model (
    Unit
  , RegionId
  , Region
  , mkRegion
  , regionId
  , center
  , center2D
  , radius
  , minRadius
  , maxRadius
  , defaultRegions
  , FeatureSpace
  , UnitSet
  , units
  , setUnits
  , fromList
  , numRegions
  , regions
  , updateRegion
  , lookupRegion
  , regionUnits
  , activateRegion
  , activateRegions
  , closest2D
) where

import           Control.Arrow (first)
import qualified Control.Monad.State as State
import           Data.Complex
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.KDTree as KDTree
import qualified Data.List as List
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import           Mescaline.FeatureSpace.Unit (Unit)
import qualified Mescaline.FeatureSpace.Unit as Unit
import qualified System.Random as Random

type UnitSet = KDTree.Tree SV.Vector Unit

type RegionId = Int

data Region = Region {
    regionId :: RegionId
  , center   :: SV.Vector Double -- Circle center in feature space
  , radius   :: Double           -- Circle radius in feature space  
  } deriving (Eq, Show)   

pair :: (V.Vector v a, Num a) => v a -> (a, a)
pair v | V.length v >= 2 = (v V.! 0, v V.! 1)
       | V.length v >= 1 = (v V.! 0, 0)
       | otherwise       = (0, 0)

center2D :: Region -> (Double, Double)
center2D = pair . center

data FeatureSpace = FeatureSpace { 
    unitSet     :: !UnitSet
  , randomGen   :: Random.StdGen
  , regionMap   :: IntMap Region
  , cachedUnits :: IntMap [Unit]
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

mkRegion :: RegionId -> SV.Vector Double -> Double -> Region
mkRegion i c r = Region i (V.fromList [clip minPos maxPos (c V.! 0), clip minPos maxPos (c V.! 1)])
                          (clip minRadius maxRadius r)

defaultRegionMap :: Int -> IntMap Region
defaultRegionMap n = IntMap.fromList $ zipWith (\i v -> (i, mkRegion i v radius)) [0..n-1] centers
    where
        radius = 0.025
        phis = take n $ iterate (+2*pi/fromIntegral n) 0
        centers = map (c2v . mkPolar (radius*3)) phis
        offset = 0.5
        c2v c = V.fromList [offset + realPart c, offset + imagPart c]

defaultNumRegions :: Int
defaultNumRegions = 8

defaultRegions :: [Region]
defaultRegions = IntMap.elems (defaultRegionMap defaultNumRegions)

units :: FeatureSpace -> [Unit]
units = KDTree.elems . unitSet

setUnits :: FeatureSpace -> [Unit.Unit] -> FeatureSpace
setUnits fs us = List.foldl' (flip updateRegion) fs' (regions fs')
    where fs' = fs { unitSet = KDTree.fromList (map (\u -> (Unit.value 0 u, u)) us) }

fromList :: Random.StdGen -> [Unit.Unit] -> FeatureSpace
fromList g = setUnits (FeatureSpace KDTree.empty g (defaultRegionMap defaultNumRegions) IntMap.empty)

numRegions :: FeatureSpace -> Int
numRegions = IntMap.size . regionMap

regions :: FeatureSpace -> [Region]
regions = IntMap.elems . regionMap

updateRegion :: Region -> FeatureSpace -> FeatureSpace
updateRegion r fs =
    fs { regionMap = IntMap.insert (regionId r) r (regionMap fs)
       , cachedUnits = IntMap.insert (regionId r) (getRegionUnits r fs) (cachedUnits fs)
       }

lookupRegion :: RegionId -> FeatureSpace -> Maybe Region
lookupRegion i = IntMap.lookup i . regionMap

-- | Return a list of all units contained in a particular region.
regionUnits :: RegionId -> FeatureSpace -> [Unit]
regionUnits i = maybe [] id . IntMap.lookup i . cachedUnits

getRegionUnits :: Region -> FeatureSpace -> [Unit]
getRegionUnits r f =
    map (snd.fst) $
        KDTree.withinRadius KDTree.sqrEuclidianDistance (center r) (radius r) (unitSet f)

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
activateRegions is f = first filterMaybe $ State.runState (sequence (map (State.state . activateRegion) is)) f

-- | Return the closest unit to a point in feature space.
closest2D :: (Double, Double) -> FeatureSpace -> Maybe (Unit, Double)
closest2D (x,y) f = do
    ((_, u), d) <- KDTree.closest KDTree.sqrEuclidianDistance (V.fromList [x, y]) (unitSet f)
    return (u, d)
