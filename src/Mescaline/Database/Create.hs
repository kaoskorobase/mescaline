module Mescaline.Database.Create where

import Data.List                        (intersperse, intercalate)
import Data.Maybe                       (maybeToList)
import Text.Printf                      (printf)
import qualified Mescaline.Database     as DB
import qualified Database.HDBC          as DB
import qualified Database.HDBC.Sqlite3  as DB

type SqlTableName = String
type SqlColumnName = String
type SqlColumnType = String

data SqlDrop = SqlDrop SqlTableName
data SqlCreate =
    SqlCreateTable SqlTableName [(SqlColumnName, SqlColumnType)]
  | SqlCreateIndex (SqlTableName -> String) SqlTableName [SqlColumnName]

class SqlQuery a where
    toSqlQuery :: a -> String
    
instance SqlQuery (SqlDrop) where
    toSqlQuery (SqlDrop name) = printf "drop table if exists %s;" name

instance SqlQuery (SqlCreate) where
    toSqlQuery (SqlCreateTable name cols) =
        printf "create table %s (%s);" name (argList cols)
        where argList = intercalate ", " . map (\(c, t) -> c ++ " " ++ t)
    toSqlQuery (SqlCreateIndex f name cols) =
        printf "create index %s on %s(%s);" (f name) name (intercalate ", " cols)

createTable :: SqlTableName -> [(SqlColumnName, SqlColumnType)] -> [SqlColumnName] -> [String]
createTable name cols idxs =
    [ toSqlQuery (SqlDrop name),
      toSqlQuery (SqlCreateTable name cols) ]
    ++ if null idxs
       then []
       else [ toSqlQuery (SqlCreateIndex ("idx_"++) name idxs) ]

maxVectorSize :: Int
maxVectorSize = 40

schema :: [String]
schema =
    createTable "source_file"           [ ("id"  ,              "integer primary key"),
                                          ("path",              "varchar(256)"),
                                          ("hash",              "varchar(40)") ]
                                        [ "id" ]
 ++ createTable "unit"                  [ ("id"            ,    "integer primary key"),
                                          ("source_file_id",    "int"),
                                          ("onset"         ,    "float8"),
                                          ("duration"      ,    "float8") ]
                                        [ "id",                 "source_file_id" ]
 ++ createTable "feature_descriptor"    [ ("id"         ,       "integer primary key"),
                                          ("name"       ,       "varchar(32)"),
                                          ("description",       "text")]
                                        [ "id" ]

 ++ createTable "unit_feature"          ([ ("unit_id"   ,       "int"),
                                           ("feature_id",       "int"),
                                           ("int_value" ,       "int4 default NULL"),
                                           ("text_value",       "text") ]
                                        ++ map (\i -> (printf "'%d'" i, "float8 default NULL")) [0..maxVectorSize-1])
                                        [ "unit_id", "feature_id" ]

 ++ createTable "unit_feature_power"    [ ("unit_id"    ,       "int"),
                                           ("value"     ,       "float8") ]
                                        [ "unit_id" ]
 ++ createTable "unit_feature_mfcc"     [ ("unit_id"    ,       "int"),
                                           ("value0"    ,       "float8"),
                                           ("value1"    ,       "float8"),
                                           ("value2"    ,       "float8"),
                                           ("value3"    ,       "float8"),
                                           ("value4"    ,       "float8"),
                                           ("value5"    ,       "float8"),
                                           ("value6"    ,       "float8"),
                                           ("value7"    ,       "float8"),
                                           ("value8"    ,       "float8"),
                                           ("value9"    ,       "float8"),
                                           ("value10"   ,       "float8"),
                                           ("value11"   ,       "float8"),
                                           ("value12"   ,       "float8") ]
                                        [ "unit_id" ]
 ++ createTable "unit_feature_spectral_centroid"
                                        [ ("unit_id"    ,       "int"),
                                           ("value"     ,       "float8") ]
                                        [ "unit_id" ]
                                        
createFeatureDescriptors :: DB.Connection -> IO ()
createFeatureDescriptors h = do
    mapM_ f ["Chroma",
             "ChromaCentroid",
             "Power",
             "SpectralPeakFrequency",
             "MelSpectrum",
             "MFCC",
             "SpectralCentroid",
             "SpectralFlatness"]
    where f name = DB.run h "insert into feature_descriptor values(null,?,null)" [DB.toSql name]

createDatabase :: FilePath -> IO ()
createDatabase path = do
    h <- DB.connectSqlite3 path
    mapM_ (flip (DB.run h) []) schema
    createFeatureDescriptors h
    DB.commit h
    DB.disconnect h
