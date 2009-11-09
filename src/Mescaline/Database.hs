module Mescaline.Database (
    withDatabase
  , DB.Connection
) where

import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB

withDatabase :: (DB.Connection -> IO ()) -> FilePath -> IO ()
withDatabase io path = do
    c <- DB.connectSqlite3 path
    io c
    DB.commit c
    DB.disconnect c
