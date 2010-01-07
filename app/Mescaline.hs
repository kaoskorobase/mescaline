module Main where

import Data.Binary
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Int (Int64)
import Data.List (groupBy, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Exception
import Control.Monad.Trans
import Sound.OpenSoundControl.Byte (decode_f64)
import System.Environment
import Text.Printf (printf)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Mescaline.Database.Feature (FeatureDescriptor(..), Feature(..), Vector)
import Mescaline.Database.FeatureData (FeatureData(..))
import Mescaline.Database.Unit (Unit(..))
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Feature as Feature
import Mescaline.Database.SourceFile (SourceFile(..))
import qualified Mescaline.Database.SourceFile as SourceFile
import Mescaline.Database.Unique as Unique

instance SqlType (B.ByteString) where
    toSql _ = undefined
    --fromSql (SqlByteString x) =  B.pack (BS.unpack x)
    fromSql (SqlString s) =  B.pack (map (toEnum . fromEnum) s)
    fromSql _ = error "fromSql: cannot convert to ByteString"


connect :: String -> IO Connection 
connect p = do 
    dbh <- connectSqlite3 p
    setBusyTimeout dbh 5000
    return dbh

withConnection :: (Connection -> IO a) -> String -> IO a
withConnection f p = do
    dbh <- connect p
    res <- f dbh
    disconnect dbh
    return res

getSourceFiles :: Connection -> IO [SourceFile]
getSourceFiles dbh =
        map get `fmap` quickQuery' dbh "select * from source_file" []
    where 
        get [sfid, path, hash] =
            SourceFile {
                SourceFile.id = fromSql sfid,
                path = fromSql path,
                hash = fromSql hash
            }

getFeatures :: Connection -> IO [FeatureDescriptor]
getFeatures dbh =
        map get `fmap` quickQuery' dbh "select * from feature" []
    where 
        get [fid,name] =
            FeatureDescriptor {
                Feature.id = fromSql fid,
                name = fromSql name
            }

segment :: Int64 -> B.ByteString -> [B.ByteString]
segment n b =
    if B.null b
        then []
        else B.take n b : segment n (B.drop n b)

doubleVectorFromByteString :: B.ByteString -> Vector Double
doubleVectorFromByteString = map decode_f64 . segment 8

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

unitFromFeatureData :: Map Int SourceFile -> Map Int FeatureDescriptor -> [FeatureData] -> Maybe Unit
unitFromFeatureData _ _ [] = Nothing
unitFromFeatureData sfMap fMap fds@(fd:_) = do
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
        res <- quickQuery' dbh (printf "select sf.id as sfid, \
        \u.id, u.onset_time, u.chunk_length, uf.feature_id as feature_id, \
        \uf.intval, uf.realval, uf.textval, uf.arrayval \
        \from source_file sf, unit u, unit_feature uf \
        \where sfid=u.sfid and sf.id = ? \
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
      
main :: IO ()
main = do
    [dbPath] <- getArgs
    -- dbh <- connect dbPath
    -- getUnits dbh [1] [1,2,3] >>= print
    -- disconnect dbh
    withConnection (\dbh -> getUnits dbh [1] [1,2,3]) dbPath >>= print
