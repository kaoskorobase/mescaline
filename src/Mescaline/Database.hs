module Mescaline.Database (
    withDatabase
  , DB.commit
  , withDatabaseP
) where

import           Control.Exception (bracket)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import           Database.Persist.Sqlite

withDatabase :: FilePath -> (DB.Connection -> IO a) -> IO a
withDatabase path = bracket (DB.connectSqlite3 path) DB.disconnect

-- withDatabase :: FilePath ->
withDatabaseP path = withSqliteConn path . runSqlConn