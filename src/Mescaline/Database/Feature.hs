{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}

module Mescaline.Database.Feature where

import Prelude hiding (id)

newtype Descriptor = Descriptor (Int, String, Int) deriving (Eq, Show) 
newtype Feature    = Feature (Descriptor, Int) deriving (Eq, Show)

mkDescriptor :: Int -> String -> Int -> Descriptor
mkDescriptor i n d = Descriptor (i, n, d)

id :: Descriptor -> Int
id (Descriptor (i, _, _)) = i

name :: Descriptor -> String
name (Descriptor (_, n, _)) = n

degree :: Descriptor -> Int
degree (Descriptor (_, _, d)) = d

indices :: Descriptor -> [Int]
indices d = [0..degree d - 1]

descriptor :: Feature -> Descriptor
descriptor (Feature (d, _)) = d

column :: Feature -> Int
column (Feature (_, c)) = c

slice :: Feature -> (Int, Int)
slice f = (column f, (degree.descriptor)f)

sqlTableName :: Descriptor -> String
sqlTableName f = "feature_" ++ (map tr $ name f)
    where
        tr '.' = '_'
        tr c   = c

{-
module Mescaline.Database.Feature (
    FeatureDescriptor,
    Feature(..),
    F_Feature(..),
    F_Value(..),
    Vector,
    Chroma,
    ChromaCentroid,
    Power,
    SpectralPeakFrequency,
    MelSpectrum, 
    MFCC,
    SpectralCentroid,
    SpectralFlatness,
    insertUnit, insertSourceFile,
    getFeatureMap
) where

import Data.List                                        (intercalate)
import Data.Map                                         (Map)
import qualified Data.Map                               as Map
import Data.Maybe                                       (fromJust)
import Mescaline.Data.Array.Vector                      (UA, Vector, fromU, toU, lengthU)
import Mescaline.Data.ListReader                        (ListReader, get, runListReader)
import Mescaline.Database                               (DB)
import qualified Mescaline.Database                     as DB
import Mescaline.Database.FeatureDescriptor             (FeatureDescriptor)
import qualified Mescaline.Database.FeatureDescriptor   as FeatureDescriptor
import Mescaline.Database.SourceFile                    (SourceFile)
import qualified Mescaline.Database.SourceFile          as SourceFile
import Mescaline.Database.Unit                          (Unit)
import qualified Mescaline.Database.Unit                as Unit
import Mescaline.Database.Unique                        (uid)
import qualified Database.HDBC                          as DB
import qualified Mescaline.Meap.Feature                 as Meap
import Text.Printf                                      (printf)

data Mean = Mean deriving (Enum, Eq, Show)

data OldFeature =
    AvgChroma			(Vector Double)
 |  AvgChromaScalar		Double
 |  AvgChunkPower 		Double
 |  AvgFreqSimple 		Double
 |  AvgMelSpec 			(Vector Double)
 |  AvgMFCC				(Vector Double)
 |  AvgPitchSimple 		Double
 |  AvgSpecCentroid 	Double
 |  AvgSpecFlatness 	Double
 deriving (Show)

data F_Feature =
    F_Chroma
  | F_ChromaCentroid
  | F_Power
  | F_SpectralPeakFrequency
  | F_MelSpectrum
  | F_MFCC
  | F_SpectralCentroid
  | F_SpectralFlatness
  deriving (Enum, Eq, Ord, Show)

data F_Value = F_Scalar Double | F_Vector (Vector Double) deriving (Eq, Show)

getScalar :: ListReader Double F_Value
getScalar = (F_Scalar . head) `fmap` get 1

getVector :: Int -> ListReader Double F_Value
getVector = fmap (F_Vector . toU) . get

featureExtractors :: Map F_Feature (ListReader Double F_Value)
featureExtractors = Map.fromList [
    ( F_Chroma                  , getVector 12 )
  , ( F_ChromaCentroid          , getScalar    )
  , ( F_Power                   , getScalar    )
  , ( F_SpectralPeakFrequency   , getScalar    )
  , ( F_MelSpectrum             , getVector 40 )
  , ( F_MFCC                    , getVector 12 )
  , ( F_SpectralCentroid        , getScalar    )
  , ( F_SpectralFlatness        , getScalar    )
  ]

data Chroma                 = Chroma { unChroma :: Vector Double }                      deriving (Eq, Show)
data ChromaCentroid         = ChromaCentroid { unChromaCentroid :: Double }               deriving (Eq, Show)
data Power                  = Power { unPower :: Double }                               deriving (Eq, Show)
data SpectralPeakFrequency  = SpectralPeakFrequency { unSpectralPeakFrequency :: Double } deriving (Eq, Show)
data MelSpectrum            = MelSpectrum { unMelSpectrum :: Vector Double }            deriving (Eq, Show)
data MFCC                   = MFCC { unMFCC :: Vector Double }                          deriving (Eq, Show)
data SpectralCentroid       = SpectralCentroid { unSpectralCentroid :: Double }           deriving (Eq, Show)
data SpectralFlatness       = SpectralFlatness { unSpectralFlatness :: Double }           deriving (Eq, Show)

-- data Feature m a = Feature m a deriving (Eq, Show)

class Feature a where
    name :: a -> String
    meap :: a -> Meap.Feature a
    putSql :: DB.Env -> Unit -> a -> IO ()
    -- fromSql :: DB.Env -> Unit -> IO a

class FromList a where
    fromList :: ListReader Double a

-- instance FromList (Double) where
--     fromList []     = Nothing
--     fromList (x:xs) = Just (x, xs)

-- vectorFromList :: (UA a) => Int -> [a] -> Maybe (Vector a, [a])
-- vectorFromList n l = if lengthU v == n then Just (v, l) else Nothing
--     where v  = toU (take n l)
--           l' = drop n l

-- vectorFeatureFromList :: (Vector Double -> a) -> Int -> [Double] -> Maybe (a, [Double])
-- vectorFeatureFromList f n l = do
--     (v, l') <- vectorFromList n l
--     return (f v, l')
-- 
-- scalarFeatureFromList :: (Double -> a) -> [Double] -> Maybe (a, [Double])
-- scalarFeatureFromList _ []     = Nothing
-- scalarFeatureFromList f (x:xs) = Just (f x, xs)

vectorFeatureFromList :: (Vector Double -> a) -> Int -> ListReader Double a
vectorFeatureFromList f n = do
    v <- toU `fmap` get n
    if lengthU v == n
        then return (f v)
        else fail "Couldn't parse vector"

scalarFeatureFromList :: (Double -> a) -> ListReader Double a
scalarFeatureFromList f = (f.head) `fmap` get 1

insertScalarFeature :: (Feature a) => (a -> Double) -> DB.Env -> Unit -> a -> IO ()
insertScalarFeature f e u a = DB.withTransaction (DB.handle e) $ \h -> do
        DB.run h "INSERT INTO unit_feature (unit_id, feature_id, '0') VALUES(?,?,?)"
                 [DB.toSql (uid u), DB.toSql feature_id, DB.toSql (f a)]
        return ()
    where feature_id = FeatureDescriptor.id (DB.getFeatureDescriptor (name a) e)

insertVectorFeature :: (Feature a) => (a -> Vector Double) -> DB.Env -> Unit -> a -> IO ()
-- insertVectorFeature f e u a = return ()
insertVectorFeature f e u a = DB.withTransaction (DB.handle e) $ \h -> do
        --print ["insertVectorFeature", sql]
        DB.run h sql ([DB.toSql (uid u), DB.toSql feature_id]
                       ++ (map DB.toSql (fromU v)))
        return ()
    where
        v          = f a
        feature_id = FeatureDescriptor.id (DB.getFeatureDescriptor (name a) e)
        gen_cols v = intercalate "," (replicate (lengthU v) "?" ++ replicate (40 - lengthU v) "NULL")
        sql        = printf "INSERT INTO unit_feature VALUES(?,?,NULL,NULL,%s)" (gen_cols v)

getFeatureValue :: F_Feature -> [DB.SqlValue] -> F_Value
getFeatureValue f vs = case runListReader (fromJust (Map.lookup f featureExtractors)) (map DB.fromSql vs) of
                        Left e  -> error e
                        Right v -> v

getFeature :: [DB.SqlValue] -> (F_Feature, F_Value)
getFeature (_:fid:_:_:vs) = (f, getFeatureValue f vs)
    where f = toEnum $ DB.fromSql fid - 1

getFeatureMap :: DB.Env -> Unit -> IO (Map F_Feature F_Value)
getFeatureMap e u = DB.withTransaction (DB.handle e) $ \h -> do
        -- rows <- quickQuery' dbh (printf "select sf.id as source_file_id, \
        --     \u.id, u.onset, u.duration, uf.feature_id as feature_id, \
        --     \uf.intval, uf.realval, uf.textval, uf.arrayval \
        --     \from source_file sf, unit u, unit_feature uf \
        --     \where source_file_id=u.sfid and sf.id = ? \
        --     \and uf.unit_id=u.id and feature_id in (%s) order by u.id asc" f_ids) [toSql (head sf_id)]
        -- return $ map getDetail res
        rows <- DB.quickQuery' h "select * from unit_feature where unit_id=?" [DB.toSql (Unit.id u)]
        return $ Map.fromList (map getFeature rows)

instance Feature (Chroma) where
    name = const "Chroma"
    putSql = insertVectorFeature unChroma
instance FromList (Chroma) where
    fromList = vectorFeatureFromList Chroma 12

instance Feature (ChromaCentroid) where
    name = const "ChromaCentroid"
    putSql = insertScalarFeature unChromaCentroid
instance FromList (ChromaCentroid) where
    fromList = scalarFeatureFromList ChromaCentroid

instance Feature (Power) where
    name = const "Power"
    putSql = insertScalarFeature unPower
instance FromList (Power) where
    fromList = scalarFeatureFromList Power

instance Feature (SpectralPeakFrequency) where
    name = const "SpectralPeakFrequency"
    putSql = insertScalarFeature unSpectralPeakFrequency
instance FromList (SpectralPeakFrequency) where
    fromList = scalarFeatureFromList SpectralPeakFrequency

instance Feature (MelSpectrum) where
    name = const "MelSpectrum"
    putSql = insertVectorFeature unMelSpectrum
instance FromList (MelSpectrum) where
    fromList = vectorFeatureFromList MelSpectrum 40

instance Feature (MFCC) where
    name = const "MFCC"
    putSql = insertVectorFeature unMFCC
instance FromList (MFCC) where
    fromList = vectorFeatureFromList MFCC 12

instance Feature (SpectralCentroid) where
    name = const "SpectralCentroid"
    putSql = insertScalarFeature unSpectralCentroid
instance FromList (SpectralCentroid) where
    fromList = scalarFeatureFromList SpectralCentroid

instance Feature (SpectralFlatness) where
    name = const "SpectralFlatness"
    putSql = insertScalarFeature unSpectralFlatness
instance FromList (SpectralFlatness) where
    fromList = scalarFeatureFromList SpectralFlatness

data FeatureSet = FeatureSet {
    chroma                :: Chroma                ,
    chromaCentroid        :: ChromaCentroid        ,
    power                 :: Power                 ,
    spectralPeakFrequency :: SpectralPeakFrequency ,
    melSpectrum           :: MelSpectrum           ,
    mFCC                  :: MFCC                  ,
    spectralCentroid      :: SpectralCentroid      ,
    spectralFlatness      :: SpectralFlatness
}

instance FromList (FeatureSet) where
    fromList = do
        chroma                <- fromList
        chromaCentroid        <- fromList
        power                 <- fromList
        spectralPeakFrequency <- fromList
        melSpectrum           <- fromList
        mFCC                  <- fromList
        spectralCentroid      <- fromList
        spectralFlatness      <- fromList
        return $ FeatureSet 
                    chroma
                    chromaCentroid
                    power
                    spectralPeakFrequency
                    melSpectrum
                    mFCC
                    spectralCentroid
                    spectralFlatness

instance Feature (FeatureSet) where
    name = error "FIXME: FeatureSet.name: refactor this mess"
    -- putSql putSql :: DB.Env -> Unit f -> a -> IO ()
    putSql env unit a = do
        putSql env unit (chroma                a)
        putSql env unit (chromaCentroid        a)
        putSql env unit (power                 a)
        putSql env unit (spectralPeakFrequency a)
        putSql env unit (melSpectrum           a)
        putSql env unit (mFCC                  a)
        putSql env unit (spectralCentroid      a)
        putSql env unit (spectralFlatness      a)

-- data FeatureProxy = forall a . (Feature a) => FeatureProxy a
-- 
-- instance Feature (FeatureProxy) where
--     name (FeatureProxy a) = name a
--     -- meap (FeatureProxy a) = meap a
--     putSql env unit (FeatureProxy a) = putSql env unit a
-- 
-- data FL = forall a . (Feature a, FromList a, Show a) => FL ([Double] -> Maybe (a, [Double]))
-- 
-- ffFromList :: [FL] -> [Double] -> Maybe [FeatureProxy]
-- ffFromList [] _ = Just []
-- ffFromList ((FL f):fs) l = do
--     (a, l') <- f l
--     as      <- ffFromList fs l'
--     return (FeatureProxy a : as)
-- 
-- type FromListFunc a = [Double] -> Maybe (a, [Double])
-- 
-- fls :: [FL]
-- fls = [
--         FL (fromList :: FromListFunc Chroma)
--       , FL (fromList :: FromListFunc ChromaCentroid)
--       , FL (fromList :: FromListFunc Power)
--       , FL (fromList :: FromListFunc SpectralPeakFrequency)
--       , FL (fromList :: FromListFunc MelSpectrum)
--       , FL (fromList :: FromListFunc MFCC)
--       , FL (fromList :: FromListFunc SpectralCentroid)
--       , FL (fromList :: FromListFunc SpectralFlatness)
--     ]

insertSourceFile :: DB.Env -> FilePath -> [[Double]] -> IO ()
insertSourceFile env path content = do
    DB.run (DB.handle env) "INSERT INTO source_file values(NULL,?,?);" [DB.toSql path, DB.toSql ""]
    DB.commit (DB.handle env)
    [[sfid]] <- DB.quickQuery' (DB.handle env) "SELECT max(@@id) FROM source_file" []
    -- print sql
    -- [[sfid]] <- DB.quickQuery' (DB.handle env) sql []
    print (DB.fromSql sfid :: Int)
    let sf = SourceFile.SourceFile (DB.fromSql sfid) path ""
    mapM_ (insertUnit env sf) content
    -- where sql = (printf "SELECT id FROM source_file WHERE path = '%s'" path)

insertUnit :: DB.Env -> SourceFile -> [Double] -> IO ()
insertUnit env sf (onset:dur:row) = do
    DB.run (DB.handle env)
        "INSERT INTO unit values(NULL,?,?,?)"
        [ DB.toSql (SourceFile.id sf), DB.toSql onset, DB.toSql dur ]
    DB.commit (DB.handle env)
    [[uid]] <- DB.quickQuery' (DB.handle env) "SELECT max(@@id) FROM unit" []
    -- print sql
    -- [[uid]] <- DB.quickQuery' (DB.handle env) sql []
    print (uid)
    let unit = Unit.Unit (DB.fromSql uid) sf onset dur
    case runListReader fromList row :: Either String FeatureSet of
        Left e   -> putStrLn ("insertUnit: error in parsing row: " ++ e)
        Right fs -> putSql env unit fs >> DB.commit (DB.handle env)
    -- where sql = (printf "SELECT id FROM unit WHERE source_file_id = %d" (SourceFile.id sf))
insertUnit _ _ _ = fail "BUG(insertUnit): missing onset and duration"
-}
