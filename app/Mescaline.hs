-- sample code, doesn't necessarily compile
module Main where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Exception
import Control.Monad.Trans
import System.Environment
import Data.Binary
import Mescaline.Database.Feature (FeatureDescriptor(..), Feature(..))
import qualified Mescaline.Database.Feature as Feature
import Mescaline.Database.SourceFile (SourceFile(..))
import qualified Mescaline.Database.SourceFile as SourceFile

connect :: IO Connection 
connect = handleSqlError $
    do 
		let 
			fp = "/Users/z/work/mescaline/trunk/tools/mescaline.db"
       		dbh <- connectSqlite3 fp
       		setBusyTimeout dbh 5000
       		prepDB dbh
       		--liftIO (print  "DB preparation complete") 
       		return dbh

prepDB dbh =
    do tables <- getTables dbh
       evaluate (length tables)
       getSourcefiles dbh
       getFeatures dbh
       getFeaturedetail dbh 2 1
    
getSourcefiles dbh =
    do  
        res <- quickQuery dbh "select * from source_file" []
        sourcefiles <- return $ map getDetail res 
        let sf = sourcefiles
        liftIO (print "sf" )
    where 
        getDetail [sfid, path, hash] =
            SourceFile {
                SourceFile.id = fromSql sfid,
                path = fromSql path,
                hash = fromSql hash
            }

getFeatures dbh =
    do
        res <- quickQuery dbh "select * from feature" []
        features <- return $ map getDetail res 
        let f = features
        liftIO (print "f" )
    where 
        getDetail [fid,name] =
            FeatureDescriptor {
                Feature.id = fromSql fid,
                name = fromSql name
            }

getFeaturedetail :: Connection -> Int -> Int -> IO ()
getFeaturedetail dbh sf_id f_id =
    do
        res <- quickQuery dbh "select sf.id as sfid, \
        \u.id, u.onset_time, u.chunk_length, uf.feature_id as feature_id, \
        \uf.intval, uf.realval, uf.textval, uf.arrayval \
        \from source_file sf, unit u, unit_feature uf \
        \where sfid=u.sfid and sfid = ? \
        \and uf.unit_id=u.id and feature_id=?" [toSql sf_id, toSql f_id]
        liftIO (print res)
            
--main :: IO ()
main = do
    [dbPath] <- getArgs
    connect 
