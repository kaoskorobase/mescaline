module Mescaline.Database.SqlRow (
    SqlRow(..),
    SqlType(..)
) where

import Database.HDBC		(SqlValue, SqlType(..))
-- import Mescaline.Database    (DB)

class SqlRow a where
    -- sqlQuery    :: a -> String
    -- toSqlRow    :: a -> [SqlValue]
    fromSqlRow  :: [SqlValue] -> a

-- class SqlGet a where
--  getSql :: DB a
-- 
-- class SqlPut a where
--     putSql :: a -> DB ()
