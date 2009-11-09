{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Mescaline.Database.FlatFile (
    Database(..)
  , Result
  , open
  , query
  , module Mescaline.Database.Query
) where

import           Control.Monad (zipWithM)
import           Data.List (group)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)

import           Mescaline.Data.Array.Vector
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.SoundFile as SF
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Query
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.Unit (Unit)
import qualified Sound.Analysis.Meapsoft as Meap
import           System.FilePath
import qualified System.FilePath.Find as Find
import qualified System.Random as Random

data Feature = Feature {
    f_name :: String
  , f_degree :: Int
  , f_column :: Int
} deriving (Eq, Show)

type FeatureMap = Map.Map String Feature

data Database = Database {
    features    :: FeatureMap,
    sourceFiles :: [SourceFile],
    units       :: [Unit]
} deriving (Show)

type Result = [Unit]

featureDegrees :: [(String, Int)]
featureDegrees = [
    ( "AvgChroma"         , 12  ),
    ( "AvgChromaScalar"   , 1   ),
    ( "AvgChunkPower"     , 1   ),
    ( "AvgFreqSimple"     , 1   ),
    ( "AvgMelSpec"        , 40  ),
    ( "AvgMFCC"           , 13  ),
    ( "AvgPitch"          , 1   ),
    ( "AvgSpec"           , 513 ),
    ( "AvgSpecCentroid"   , 1   ),
    ( "AvgSpecFlatness"   , 1   ),
    ( "AvgTonalCentroid"  , 6   ),
    ( "ChunkLength"       , 1   ),
    ( "ChunkStartTime"    , 1   ),
    ( "Entropy"           , 1   ),
    ( "RMSAmplitude"      , 1   ),
    ( "SpectralStability" , 1   )
    ]

mkFeatures :: [String] -> [Feature]
mkFeatures fs = zipWith3 Feature fs ds cs
    where
        ds = fromJust $ mapM (flip lookup featureDegrees) fs
        cs = scanl (+) 0 ds

meapFeatureData :: Meap.MEAP -> [Vector Double]
meapFeatureData m = map (toU.drop 2.Meap.frame_l m) [0..Meap.n_frames m - 1]

readUnits :: FilePath -> IO [((Double, Double), Vector Double)]
readUnits path = do
    res <- Meap.read_meap path
    case res of
        Left err -> fail err
        Right m  -> let ss = Meap.segments_l m
                        vs = meapFeatureData m
                    in if length ss /= length vs
                        then fail "Ooops."
                        else return $ zip ss vs

featuresFromFile :: FilePath -> IO FeatureMap
featuresFromFile path = do
    fs <- (mkFeatures.lines) `fmap` readFile path
    return $ Map.fromList $ zip (map f_name fs) fs

zipWithIds :: [a] -> IO [(Unique.Id, a)]
zipWithIds = mapM (\a -> flip (,) a `fmap` Random.randomIO)

open :: FilePath -> IO Database
open path = do
    features <- featuresFromFile $ joinPath [path, "meap.db"]
    files <- Find.find
                Find.always
                (fmap (== ".feat_beats") Find.extension)
                path
    assocs <- zipWithIds files >>= mapM (uncurry newSourceFile)
    let sourceFiles = map fst assocs
        units       = concat $ map snd assocs
    return $ Database features sourceFiles units
    where
        newSourceFile i f = do
            let sfPath = dropExtension f
            info <- SF.getInfo sfPath
            let sf = SourceFile.SourceFile
                        i sfPath SourceFile.noHash
                        (SF.channels info)
                        (fromIntegral $ SF.samplerate info)
                        (SF.frames info)
            assocs <- readUnits f
            specs <- zipWithIds (zip (repeat sf) assocs)
            return (sf, map mkUnit specs)
        mkUnit (i, (sf, ((o, d), vs))) = Unit.Unit i sf o d

query :: Query -> Database -> [Unit]
query (Query q) = filter q . units

{-
import Data.Binary
import qualified Database.HDBC		    as DB
import qualified Database.HDBC.Sqlite3	as DB
import Data.List                		(groupBy, intersperse)
import Data.Map                 		(Map)
import qualified Data.Map       		as Map
import Data.Maybe               		(fromJust, mapMaybe)
import Control.Monad
import Control.Exception                ()
import Control.Monad.Reader 			(ReaderT, asks, runReaderT)
import Text.Printf              		(printf)

-- import Mescaline.Database.Feature                       (FeatureDescriptor(..), Feature(..), Vector)
-- import Mescaline.Database.FeatureData                   (FeatureData(..))
import Mescaline.Database.SqlRow                        (SqlRow(..))
import Mescaline.Database.Unit                          (Unit(..))
import qualified Mescaline.Database.Unit                as Unit

import Mescaline.Database.FeatureDescriptor             (FeatureDescriptor)
import qualified Mescaline.Database.FeatureDescriptor   as FeatureDescriptor

import Mescaline.Database.SourceFile                    (SourceFile(..))
import qualified Mescaline.Database.SourceFile          as SourceFile
import Mescaline.Database.Unique                        as Unique

type Pattern = String
type Handle = DB.Connection

data Env = Env {
	handle :: Handle,
    featureDescriptors :: FeatureDescriptorMap
}

newtype DB a = DB (ReaderT Env IO a)
				deriving (Monad)

type FeatureDescriptorMap = Map String FeatureDescriptor
-- type SourceFileMap        = Map (FilePath, SourceFile)

withHandle :: (Env -> IO a) -> String -> IO a
withHandle f path = do
    h <- DB.connectSqlite3 path
    fds <- getFeatureDescriptors h
    res <- f (Env h (Map.fromList (zip (map FeatureDescriptor.name fds) fds)))
    DB.disconnect h -- TODO: `finally`
    return res

getSourceFiles :: Maybe Pattern -> Env -> IO [SourceFile]
getSourceFiles pattern e = do
        rows <- DB.quickQuery' (handle e) query []
        return (map fromSqlRow rows)
    where
        query = "select * from source_file"
                    ++ case pattern of
                        Just p  -> " " ++ p
                        Nothing -> ""

getUnits :: Maybe Pattern -> Env -> SourceFile -> IO [Unit]
getUnits pattern env sourceFile = do
        rows <- DB.quickQuery' (handle env) query []
        mapM mkUnit rows
    where
        query = printf "select * from unit where source_file_id = %d"
                        (SourceFile.id sourceFile)
                    ++ case pattern of
                        Just p  -> " and " ++ p
                        Nothing -> ""
        mkUnit row@(uid:_) = do
            -- features <- DB.quickQuery' (handle env) "select * from unit_feature where unit_id = ?" [uid]
            return (Unit.unitFromSqlRow sourceFile row)
            
getFeatureDescriptor :: String -> Env -> FeatureDescriptor
getFeatureDescriptor name env =
    case Map.lookup name (featureDescriptors env) of
        Nothing -> error ("getFeatureDescriptor: invalid feature descriptor " ++ name)
        Just fd -> fd

getFeatureDescriptors :: Handle -> IO [FeatureDescriptor]
getFeatureDescriptors h = do
        rows <- DB.quickQuery' h "select * from feature_descriptor" []
        return (map fromSqlRow rows)
    where fromSqlRow [_id, _name, _desc] = FeatureDescriptor.FeatureDescriptor
                                                (DB.fromSql _id)
                                                (DB.fromSql _name)
                                                (DB.fromSql _desc)
-}

-----------------------------------------------------------------------------

{-
data ScalarType =
    TInt
  | TDouble
  | TString

data FeatureType =
    TScalar ScalarType
  | TVector Int

class ReadFeatures a where
    featureNames :: a -> [String]
    featureTypes :: a -> [FeatureType]
    readFeatures :: [[SqlValue]] -> a

getFeatureData :: Connection -> [Int] -> [Int] -> IO [FeatureData]
getFeatureData dbh sf_id f_id =
    do
        res <- quickQuery' dbh (printf "select sf.id as source_file_id, \
        \u.id, u.onset, u.duration, uf.feature_id as feature_id, \
        \uf.intval, uf.realval, uf.textval, uf.arrayval \
        \from source_file sf, unit u, unit_feature uf \
        \where source_file_id=u.sfid and sf.id = ? \
        \and uf.unit_id=u.id and feature_id in (%s) order by u.id asc" f_ids) [toSql (head sf_id)]
        return $ map getDetail res
    where 
        f_ids = concat (intersperse "," (map show f_id))
        getDetail [sfid,uid,onset_time,chunck_length,feature_id, intval, realval, textval, arrayval] =
            FeatureData {
                sourceFileId = fromSql sfid,
                unitId       = fromSql uid,
                onset_time   = fromSql onset_time,
                chunk_length = fromSql chunck_length,
                featureId    = fromSql feature_id,
                intval       = fromSql intval,
                realval      = fromSql realval,
                textval      = fromSql textval,
                arrayval     = doubleVectorFromByteString `fmap` fromSql arrayval
            }

getUnits :: (ReadFeatures a) => Connection -> [SourceFile] -> [Unit a]
getUnits dbh sourceFiles = do
    fdMap <- Unique.mapFromList `fmap` getFeatureDescriptors dbh
    
    fdata <- getFeatureData dbh sfids fids
    return (mapMaybe (unitFromFeatureData sfMap fdMap)
                     (groupBy (\x1 x2 -> unitId x1 == unitId x2)
                     fdata))

featureFromFeatureData :: FeatureDescriptor -> FeatureData -> Maybe Feature
featureFromFeatureData (FeatureDescriptor _ "AvgChroma")        = fmap AvgChroma        . arrayval
featureFromFeatureData (FeatureDescriptor _ "AvgChromaScalar")  = fmap AvgChromaScalar  . realval 
featureFromFeatureData (FeatureDescriptor _ "AvgChunkPower")    = fmap AvgChunkPower    . realval 
featureFromFeatureData (FeatureDescriptor _ "AvgFreqSimple")    = fmap AvgFreqSimple    . realval 
featureFromFeatureData (FeatureDescriptor _ "AvgMelSpec")       = fmap AvgMelSpec       . arrayval 
featureFromFeatureData (FeatureDescriptor _ "AvgMFCC")          = fmap AvgMFCC          . arrayval 
featureFromFeatureData (FeatureDescriptor _ "AvgPitchSimple")   = fmap AvgPitchSimple   . realval 
featureFromFeatureData (FeatureDescriptor _ "AvgSpecCentroid")  = fmap AvgSpecCentroid  . realval 
featureFromFeatureData (FeatureDescriptor _ "AvgSpecFlatness")  = fmap AvgSpecFlatness  . realval 

unitFromRow :: Map Int SourceFile -> Map Int FeatureDescriptor -> [FeatureData] -> Maybe Unit
unitFromRow _ _ [] = Nothing
unitFromRow sfMap fMap fds@(fd:_) = do
    sf    <- Map.lookup (sourceFileId fd) sfMap
    fdesc <- Map.lookup (featureId fd)    fMap
    return $ Unit {
        Unit.id     = unitId fd,
        sourceFile  = sf,
        onsetTime   = onset_time fd,
        chunkLength = chunk_length fd,
        features    = mapMaybe (featureFromFeatureData fdesc) fds
    }

getFeatureData :: Connection -> [Int] -> [Int] -> IO [FeatureData]
getFeatureData dbh sf_id f_id =
    do
        res <- quickQuery' dbh (printf "select sf.id as source_file_id, \
        \u.id, u.onset, u.duration, uf.feature_id as feature_id, \
        \uf.intval, uf.realval, uf.textval, uf.arrayval \
        \from source_file sf, unit u, unit_feature uf \
        \where source_file_id=u.sfid and sf.id = ? \
        \and uf.unit_id=u.id and feature_id in (%s) order by u.id asc" f_ids) [toSql (head sf_id)]
        return $ map getDetail res
    where 
        f_ids = concat (intersperse "," (map show f_id))
        getDetail [sfid,uid,onset_time,chunck_length,feature_id, intval, realval, textval, arrayval] =
            FeatureData {
                sourceFileId = fromSql sfid,
                unitId       = fromSql uid,
                onset_time   = fromSql onset_time,
                chunk_length = fromSql chunck_length,
                featureId    = fromSql feature_id,
                intval       = fromSql intval,
                realval      = fromSql realval,
                textval      = fromSql textval,
                arrayval     = doubleVectorFromByteString `fmap` fromSql arrayval
            }

getUnits :: Connection -> [Int] -> [Int] -> IO [Unit]
getUnits dbh sfids fids =
    do
        sfMap <- Unique.mapFromList `fmap` getSourceFiles dbh
        fdMap <- Unique.mapFromList `fmap` getFeatures dbh
        fdata <- getFeatureData dbh sfids fids
        return (mapMaybe (unitFromFeatureData sfMap fdMap)
                         (groupBy (\x1 x2 -> unitId x1 == unitId x2)
                         fdata))

getUnitsByName :: Connection -> [Pattern] -> IO ([SourceFile], [Unit])
getUnitsByName dbh patterns  =
    do
        sfMap <- Unique.mapFromList `fmap` concat `fmap` mapM (getSourceFilesByName dbh) patterns
        fdMap <- Unique.mapFromList `fmap` getFeatures dbh
        fdata <- getFeatureData dbh (Map.keys sfMap) (Map.keys fdMap)
        return (Map.elems sfMap, (mapMaybe (unitFromFeatureData sfMap fdMap)
                                     (groupBy (\x1 x2 -> unitId x1 == unitId x2)
                                      fdata)))
-}
