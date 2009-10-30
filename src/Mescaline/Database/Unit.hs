module Mescaline.Database.Unit where

import Database.HDBC		            (SqlValue, fromSql)
import Data.List                        (find)
-- import Mescaline.Database.Feature       (Feature(..), Vector)
import Mescaline.Database.SourceFile    (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import Mescaline.Database.SqlRow        (SqlRow(..), fromSql, toSql)
import Mescaline.Database.Unique        as Unique
import Prelude hiding                   (id)

type Time  = Double
type DTime = Double

data Unit = Unit {
    id          :: Id,
    sourceFile  :: SourceFile,
    onset       :: Time,
    duration    :: DTime
} deriving (Show)

instance Unique Unit where
    uid = id

instance Eq Unit where
    a == b = id a == id b

unitFromSqlRow :: SourceFile -> [SqlValue] -> Unit
unitFromSqlRow sourceFile
            (  _id
             : _
             : _onset
             : _duration
             : [] ) =
                 Unit
                    (fromSql _id)
                    sourceFile
                    (fromSql _onset)
                    (fromSql _duration)
unitFromSqlRow _ _ = error "SqlRow (SourceFile) conversion failure"
