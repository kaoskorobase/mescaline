-- sample code, doesn't necessarily compile
module Main where


import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Control.Exception

import Control.Monad.Trans
--import Database.Sqlite.Enumerator
--import Database.Enumerator
import System.Environment
--import Data.Binary


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
       		liftIO (print  "DB preparation complete") 
       		return dbh

prepDB dbh =
    do tables <- getTables dbh
       evaluate (length tables)
       getSourcefiles dbh
       getFeatures dbh

    
getSourcefiles dbh =
    do
        res <- quickQuery dbh "select * from source_file" []
        sourcefiles <- return $ map getDetail res 
        liftIO (print sourcefiles )
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
        liftIO (print features )
    where 
        getDetail [fid,name] =
            FeatureDescriptor {
                Feature.id = fromSql fid,
                name = fromSql name
            }
        
--main :: IO ()
main = do
    [dbPath] <- getArgs
    --fid <- getLine >>= return . read
    connect 
