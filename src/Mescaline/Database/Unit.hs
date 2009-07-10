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

-- avgSpecCentroid :: Unit -> Double
-- avgSpecCentroid u = maybe 0 v (find p (features u))
--     where
--         p (AvgSpecCentroid _) = True
--         p _                   = False
--         v (AvgSpecCentroid x) = x
--         v _                   = error "fuck you bitch"
-- 
-- avgChroma :: Unit -> Vector Double
-- avgChroma u = maybe [] v (find p (features u))
--     where
--         p (AvgChroma _) = True
--         p _             = False
--         v (AvgChroma x) = x
--         v _             = error "fuck you bitch"

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
