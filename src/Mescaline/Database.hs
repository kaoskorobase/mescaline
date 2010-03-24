module Mescaline.Database (
    Connection
  , withDatabase
  , DB.commit
) where

import           Control.Exception (bracket)
import qualified Database.HDBC as DB
import           Database.HDBC.Sqlite3 (Connection)
import qualified Database.HDBC.Sqlite3 as DB

withDatabase :: FilePath -> (Connection -> IO a) -> IO a
withDatabase path = bracket (DB.connectSqlite3 path) DB.disconnect
