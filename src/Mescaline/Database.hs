module Mescaline.Database (
    Connection
  , withDatabase
) where

import qualified Database.HDBC as DB
import           Database.HDBC.Sqlite3 (Connection)
import qualified Database.HDBC.Sqlite3 as DB

withDatabase :: (Connection -> IO a) -> FilePath -> IO a
withDatabase action path = do
    conn <- DB.connectSqlite3 path
    result <- action conn
    DB.commit conn
    DB.disconnect conn
    return result
