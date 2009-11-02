module Mescaline.Database.Unit where

import           Data.List (find)
import           Database.HDBC (SqlValue, fromSql)

import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Data.Array.Vector
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.SqlRow (SqlRow(..), fromSql, toSql)
import           Mescaline.Database.Unique as Unique

import           Prelude hiding(id)

type Time  = Double
type DTime = Double

data Unit = Unit {
    id            :: Id,
    sourceFile    :: SourceFile,
    onset         :: Time,
    duration      :: DTime,
    featureValues :: Vector Double
} deriving (Show)

instance Unique Unit where
    uid = id

instance Eq Unit where
    a == b = id a == id b

feature :: Feature -> Unit -> Double
feature f u = indexU (featureValues u) (Feature.column f)

featureVector :: Feature -> Unit -> Vector Double
featureVector f u = sliceU (featureValues u) (Feature.column f) (Feature.degree f)

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
                    undefined
unitFromSqlRow _ _ = error "SqlRow (SourceFile) conversion failure"
