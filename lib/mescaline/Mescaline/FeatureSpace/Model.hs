{-# LANGUAGE BangPatterns, CPP, FlexibleContexts #-}
module Mescaline.FeatureSpace.Model (
    Unit
  , RegionId
  , Region
  , mkRegion
  , defaultRegions
  , regionId
  , center
  , center2D
  , radius
  , minRadius
  , maxRadius
  , FeatureSpace
  , UnitSet
  , units
  , setUnits
  , fromList
  , empty
  , numRegions
  , regions
  , updateRegion
  , lookupRegion
  , region
  , regionUnits
  , closest2D
) where

import           Data.Complex
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.KDTree as KDTree
import qualified Data.List as List
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import           Mescaline.FeatureSpace.Unit (Unit)
import qualified Mescaline.FeatureSpace.Unit as Unit

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
  , regions     :: BV.Vector Region
  , cachedUnits :: BV.Vector [Unit]
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

mkDefaultRegions :: Int -> BV.Vector Region
mkDefaultRegions n = V.fromList (zipWith (\i c -> mkRegion i c radius) [0..n-1] centers)
    where
        radius = 0.025
        phis = take n $ iterate (+2*pi/fromIntegral n) 0
        centers = map (c2v . mkPolar (radius*3)) phis
        offset = 0.5
        c2v c = V.fromList [offset + realPart c, offset + imagPart c]

defaultNumRegions :: Int
defaultNumRegions = 8

defaultRegions :: [Region]
defaultRegions = V.toList $ mkDefaultRegions defaultNumRegions

units :: FeatureSpace -> [Unit]
units = KDTree.elems . unitSet

setUnits :: FeatureSpace -> [Unit.Unit] -> FeatureSpace
setUnits fs us = List.foldl' (flip updateRegion) fs' (V.toList (regions fs))
    where fs' = fs { unitSet = KDTree.fromList (map (\u -> (Unit.value 0 u, u)) us) }

fromList :: [Unit.Unit] -> FeatureSpace
fromList = setUnits (FeatureSpace KDTree.empty (mkDefaultRegions n) (V.replicate n []))
    where n = defaultNumRegions

empty :: FeatureSpace
empty = fromList []

numRegions :: FeatureSpace -> Int
numRegions = V.length . regions

updateRegion :: Region -> FeatureSpace -> FeatureSpace
updateRegion r fs =
    fs { regions = regions fs V.// [(regionId r, r)]
       , cachedUnits = cachedUnits fs V.// [(regionId r, getRegionUnits r fs)]
       }

lookupRegion :: RegionId -> FeatureSpace -> Maybe Region
lookupRegion i fs = regions fs V.!? i

region :: RegionId -> FeatureSpace -> Region
region i fs = maybe (error $ "Invalid region " ++ show i) id (lookupRegion i fs)

-- | Return a list of all units contained in a particular region.
regionUnits :: RegionId -> FeatureSpace -> [Unit]
regionUnits i fs = maybe [] id (cachedUnits fs V.!? i)

getRegionUnits :: Region -> FeatureSpace -> [Unit]
getRegionUnits r f =
    map (snd.fst) $
        KDTree.withinRadius KDTree.sqrEuclidianDistance (center r) (radius r) (unitSet f)

-- | Return the closest unit to a point in feature space.
closest2D :: (Double, Double) -> FeatureSpace -> Maybe (Unit, Double)
closest2D (x,y) f = do
    ((_, u), d) <- KDTree.closest KDTree.sqrEuclidianDistance (V.fromList [x, y]) (unitSet f)
    return (u, d)
