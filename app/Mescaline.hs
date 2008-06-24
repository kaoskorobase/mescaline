-- sample code, doesn't necessarily compile
module Main where

import Data.Binary
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Exception
import Control.Monad.Trans
import Sound.OpenSoundControl.Byte (decode_f64)
import System.Environment

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Mescaline.Database.Feature (FeatureDescriptor(..), Feature(..), FeatureData(..), Vector)
import qualified Mescaline.Database.Feature as Feature
--import qualified Mescaline.Database.FeatureData as FeatureData
import Mescaline.Database.SourceFile (SourceFile(..))
import qualified Mescaline.Database.SourceFile as SourceFile


instance SqlType (B.ByteString) where
    toSql _ = undefined
    --fromSql (SqlByteString x) =  B.pack (BS.unpack x)
    fromSql (SqlString s) =  B.pack (map (toEnum . fromEnum) s)
    fromSql _ = error "fromSql: cannot convert to ByteString"


connect :: String -> IO Connection 
connect p = handleSqlError $
    do 
		let 
       		dbh <- connectSqlite3 p
       		setBusyTimeout dbh 5000
       		prepDB dbh
       		--liftIO (print  "DB preparation complete") 
       		return dbh

prepDB dbh =
    do tables <- getTables dbh
       evaluate (length tables)
       getSourcefiles dbh
       getFeatures dbh
       -- example für feature unit call für ein bestimmtest soundfile mit einem bestimmten feature
       getFeaturedetail dbh 1 [1,2,3] >>= print

getSourcefiles :: Connection -> IO [SourceFile]
getSourcefiles dbh =
    do  
        res <- quickQuery dbh "select * from source_file" []
        return $ map getDetail res 
    where 
        getDetail [sfid, path, hash] =
            SourceFile {
                SourceFile.id = fromSql sfid,
                path = fromSql path,
                hash = fromSql hash
            }

getFeatures :: Connection -> IO [FeatureDescriptor]
getFeatures dbh =
    do
        res <- quickQuery dbh "select * from feature" []
        return $ map getDetail res 
    where 
        getDetail [fid,name] =
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

-- unitFromFeatureData :: Map Int SourceFile -> Map Int Feature -> FeatureData -> Maybe Unit
-- unitFromFeatureData sfMap fMap fd = do
--     sf    <- Map.lookup (sfid fd) sfMap
--     fdesc <- Map.lookup (fid fd) fMap
--     Unit {
--         id = uid fd,
--         sourceFile = sf,
--         onsetTime = onset_time fd,
--         chunkLength = chunk_length fd,
--         features = fs
--     }

getFeaturedetail :: Connection -> Int -> [Int] -> IO [FeatureData]
getFeaturedetail dbh sf_id f_id =
    do
        res <- quickQuery dbh "select sf.id as sfid, \
        \u.id, u.onset_time, u.chunk_length, uf.feature_id as feature_id, \
        \uf.intval, uf.realval, uf.textval, uf.arrayval \
        \from source_file sf, unit u, unit_feature uf \
        \where sfid=u.sfid and sf.id = ? \
        \and uf.unit_id=u.id and feature_id in (?)" [toSql sf_id, toSql f_id]
        return $ map getDetail res 
    where 
        getDetail [sfid,uid,onset_time,chunck_length,feature_id, intval, realval, textval, arrayval] =
            FeatureData {
                sfid = fromSql sfid,
                uid = fromSql uid,
                onset_time = fromSql onset_time,
                chunck_length = fromSql chunck_length,
                feature_id = fromSql feature_id,
                intval = fromSql intval,
                realval = fromSql realval,
                textval = fromSql textval,
                arrayval = doubleVectorFromByteString `fmap` fromSql arrayval
            }
            
--main :: IO ()
main = do
    [dbPath] <- getArgs
    connect dbPath
