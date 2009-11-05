module Mescaline.Database.Create (
    newDatabase
  , getFeatures
  , getFeature
  , hasFeature
  , addFeature
) where

import           Data.List (intercalate)
import           Control.Monad (unless)
import           Text.Printf (printf)

import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Data.Array.Vector

import           Database.HDBC (IConnection)
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
        printf "create table if not exists %s (%s);" name (argList cols)
        where argList = intercalate ", " . map (\(c, t) -> c ++ " " ++ t)
    toSqlQuery (SqlCreateIndex f name cols) =
        printf "create index %s on %s(%s);" (f name) name (intercalate ", " cols)

table :: SqlTableName -> [(SqlColumnName, SqlColumnType)] -> [SqlColumnName] -> [String]
table name cols idxs =
    [ toSqlQuery (SqlDrop name)
    , toSqlQuery (SqlCreateTable name cols) ]
    ++ if null idxs
       then []
       else [ toSqlQuery (SqlCreateIndex ("idx_"++) name idxs) ]

featureTable :: Feature.Descriptor -> [String]
featureTable f =
    [ toSqlQuery (SqlDrop name)
    , toSqlQuery (SqlCreateTable name cols) ]
    where
        name = Feature.sqlTableName f
        cols = map (\i -> ("value_" ++ show i, "float8")) [0..Feature.degree f - 1]

schema :: [String]
schema =
    table "source_file"
        [ ("id"   , "integer primary key")
        , ("path" , "varchar(256)")
        , ("hash" , "varchar(40)") ]
        [ "id" ]
 ++ table "unit"
        [ ("id"             , "integer primary key")
        , ("source_file_id" , "integer")
        , ("onset"          , "float8")
        , ("duration"       , "float8") ]
        [ "id", "source_file_id" ]
 ++ table "feature"
        [ ("name"   , "varchar(32)")
        , ("degree" , "integer")
        ]
        [ ]

run' :: IConnection c => c -> String -> IO Integer
run' c = flip (DB.run c) []

newDatabase :: FilePath -> IO ()
newDatabase path = do
    c <- DB.connectSqlite3 path
    mapM_ (run' c) schema
    DB.commit c
    DB.disconnect c

getFeatures :: IConnection c => c -> IO [Feature.Descriptor]
getFeatures _ = return []

getFeature :: IConnection c => String -> c -> IO (Maybe Feature.Descriptor)
getFeature s c = do
    fs <- getFeatures c
    return $ lookup s $ zip (map Feature.name fs) fs

hasFeature :: IConnection c => String -> c -> IO Bool
hasFeature s c = getFeatures c >>= (return . any ((s==).Feature.name))

addFeature :: IConnection c => Feature.Descriptor -> c -> IO ()
addFeature f = flip DB.withTransaction action
    where
        action c = hasFeature (Feature.name f) c >>= flip unless (do
                    DB.run c "insert into feature values(?,?)" [DB.toSql (Feature.name f), DB.toSql (Feature.degree f)]
                    mapM_ (run' c) (featureTable f))

