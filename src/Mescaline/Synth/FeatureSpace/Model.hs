{-# LANGUAGE BangPatterns, CPP, FlexibleContexts #-}
module Mescaline.Synth.FeatureSpace.Model (
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
  , unitSet
  , randomGen
  , units
  , setFeatureSpace
  , fromList
  , nextRegionId
  , addRegion
  , lookupRegion
  , deleteRegion
  -- , updateRegionById
  , updateRegion
  , regions
  , regionList
  , regionUnits
  , activateRegion
  , activateRegions
  , closest2D
) where

#define USE_KDTREE 1

import           Control.Arrow (first, second)
import qualified Control.Monad.State as State
import           Data.Bits (shiftR)
import           Data.Complex
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
#if USE_KDTREE == 1
import qualified Data.KDTree as KDTree
#else
import           Data.Set.BKTree (BKTree)
import qualified Data.Set.BKTree as BKTree
#endif -- USE_KDTREE
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified GHC.Float as GHC
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Synth.FeatureSpace.Unit (Unit)
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import qualified System.Random as Random

#if USE_KDTREE == 1
type UnitSet = KDTree.Tree SV.Vector Unit
#else
type UnitSet = BKTree Unit
#endif -- USE_KDTREE

-- FIXME: These instances should be defined on a `UnitView' instead, i.e. some kind
-- of lense that is defined on a specific feature (index)

#if USE_KDTREE == 1
unitDistance :: Unit -> Unit -> Double
{-# INLINE unitDistance #-}
unitDistance a b = KDTree.sqrEuclidianDistance (Unit.value 0 a) (Unit.value 0 b)

instance Eq (Unit) where
    (==) a b = Unique.uuid (Unit.unit a) == Unique.uuid (Unit.unit b)
#else
instance Eq (Unit) where
    (==) = unitEq

instance BKTree.Metric Unit where
    distance = unitDistance

unitEq :: Unit -> Unit -> Bool
{-# INLINE unitEq #-}
unitEq a b = f a == f b
    where
        {-# INLINE f #-}
        f :: Unit -> SV.Vector Int
        f = V.map withPrecision . Unit.value 0 -- FIXME

-- Euclidian distance. Note: Assuming that feature dimensions are scaled to [0,1].
unitDistance :: Unit -> Unit -> Int
{-# INLINE unitDistance #-}
unitDistance a b = withPrecision $ euclidianDistance (Unit.value 0 a) (Unit.value 0 b)

euclidianDistance :: V.Vector v Double => v Double -> v Double -> Double
{-# INLINE euclidianDistance #-}
euclidianDistance a b =
    -- sqrt $ V.foldl (\acc x -> acc + x * x) 0 (V.zipWith (-) a b)
    sqrt $ loop 0 0 (V.length a `min` V.length b)
    where
        loop !acc !i !n
            | i >= n = acc
            | otherwise = let x = (a V.! i) - (b V.! i) in loop (acc + x*x) (i+1) n

precision :: Double
{-# INLINE precision #-}
precision = GHC.int2Double ((maxBound :: Int) `shiftR` 1)

precision' :: Double
{-# INLINE precision' #-}
precision' = recip precision

withPrecision :: Double -> Int
{-# INLINE withPrecision #-}
withPrecision = GHC.double2Int . (*) precision

withoutPrecision :: Int -> Double
{-# INLINE withoutPrecision #-}
withoutPrecision = (*) precision' . GHC.int2Double
#endif -- USE_KDTREE

type RegionId = Int

data Region = Region {
    regionId :: RegionId
  , center   :: Feature.Value -- Circle center in feature space
  , radius   :: Double        -- Circle radius in feature space  
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
#if USE_KDTREE == 1
units = KDTree.elems . unitSet
#else
units = BKTree.elems . unitSet
#endif -- USE_KDTREE

setFeatureSpace :: FeatureSpace -> [Unit.Unit] -> FeatureSpace
#if USE_KDTREE == 1
setFeatureSpace f us = f { unitSet = KDTree.fromList (map (\u -> (Unit.value 0 u, u)) us) }
#else
setFeatureSpace f us = f { unitSet = BKTree.fromList us }
#endif -- USE_KDTREE

fromList :: Random.StdGen -> [Unit.Unit] -> FeatureSpace
#if USE_KDTREE == 1
fromList g us = FeatureSpace (KDTree.fromList (map (\u -> (Unit.value 0 u, u)) us)) g IntMap.empty
#else
fromList g us = FeatureSpace (BKTree.fromList us) g IntMap.empty
#endif -- USE_KDTREE

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

lookupRegion :: RegionId -> FeatureSpace -> Maybe Region
lookupRegion i = IntMap.lookup i . regions

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
#if USE_KDTREE == 1
regionUnits i f =
    case IntMap.lookup i (regions f) of
        Nothing -> []
        Just r  -> map (snd.fst) $ KDTree.withinRadius KDTree.sqrEuclidianDistance (center r) (radius r) (unitSet f)
#else
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
#endif -- USE_KDTREE

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

-- | Return the closest unit to a point in feature space.
closest2D :: (Double, Double) -> FeatureSpace -> Maybe (Unit, Double)
#if USE_KDTREE == 1
closest2D (x,y) f = do
    ((_, u), d) <- KDTree.closest KDTree.sqrEuclidianDistance (V.fromList [x, y]) (unitSet f)
    return (u, d)
#else
closest2D (x,y) f =
    case BKTree.elems (unitSet f) of
        (u:_) ->
            let u' = Unit.withValues u [V.fromList [x, y]]
            in fmap (second withoutPrecision) (BKTree.closest u' (unitSet f))
        [] -> Nothing
#endif -- USE_KDTREE
