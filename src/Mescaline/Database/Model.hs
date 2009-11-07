{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Mescaline.Database.Model where

import           Database.HDBC
import           Database.HDBC          as DB

import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Data.Array.Vector

import           Data.List (intercalate)
import           Control.Monad (unless)
import           Text.Printf (printf)

data SqlTable = SqlTable {
    tableName    :: String
  , tableColumns :: [(String, String)]
  , tableIndexes :: [String]
}

class SqlModel a env where
    sqlTable   :: a -> [SqlTable]
    --sqlRow    :: a -> [DB.SqlValue]
    sqlCreate  :: IConnection c => c -> a -> IO ()
    sqlDelete  :: IConnection c => c -> a -> IO ()
    sqlQuery   :: IConnection c => c -> env -> IO [a]

run' :: IConnection c => c -> String -> [SqlValue] -> IO ()
run' c s xs = run c s xs >> return ()

instance SqlModel (SourceFile.SourceFile) b where
    sqlTable _ = [ SqlTable "source_file"
                    [ ("id"   , "integer primary key")
                    , ("url"  , "text")
                    , ("hash" , "blob") ]
                    [ "id" ] ]
    sqlCreate c sf = run' c "insert into source_file values (null,?,?)" (tail $ sqlRow sf)
        where sqlRow sf = [(DB.toSql.SourceFile.id)sf,(DB.toSql.SourceFile.path)sf,(DB.toSql.SourceFile.hash)sf] 
    sqlQuery c _ = quickQuery' c "select * from source_file" [] >>= mapM f
        where
            f [i, u, h] = return $ SourceFile.SourceFile (DB.fromSql i) (DB.fromSql u) (DB.fromSql h) 0 0 0
            f _         = fail "WTF"
 
instance SqlModel Unit.Unit [SourceFile] where
    sqlTable _ = [ SqlTable "unit"
                    [ ("id"          , "integer primary key")
                    , ("source_file" , "integer")
                    , ("onset"       , "real")
                    , ("duration"    , "real") ]
                    [ "id", "source_file" ] ]
    sqlCreate c u = run' c "insert into unit values (null,?,?,?)" (tail $ sqlRow u)
        where sqlRow u = [(DB.toSql.Unit.id)u, (DB.toSql.SourceFile.id.Unit.sourceFile)u,(DB.toSql.Unit.onset)u,(DB.toSql.Unit.duration)u]
    --sqlQuery c env = quickQuery' c "select * from source_file where source_file="
    --    where 
    --        f [i, si, o, d] = return $ Unit.Unit (DB.fromSql i) 

instance SqlModel Feature.Descriptor b where
    sqlTable _ = [ SqlTable "feature_descriptor"
                    [ ("id"     , "integer primary key")
                    , ("name"   , "text")
                    , ("degree" , "integer") ]
                    [] ]
    sqlCreate c f = run' c "insert into feature_descriptor values (null,?,?)" (tail $ sqlRow f)
        where sqlRow f = [undefined, (DB.toSql.Feature.name)f, (DB.toSql.Feature.degree)f]
    sqlQuery c _ = quickQuery' c "select * from feature_descriptor" [] >>= mapM f
        where
            f [x1,x2,x3] = return $ Feature.mkDescriptor (DB.fromSql x1) (DB.fromSql x2) (DB.fromSql x3)
            f _          = fail "WTF"

maxFeatureDegree :: Int
maxFeatureDegree = 40

instance SqlModel Unit.FeatureTable ([SourceFile],[Feature.Descriptor]) where
    sqlTable t = map mktable (Unit.descriptors t)
        where mktable f = SqlTable (Feature.sqlTableName f)
                                   ([ ("unit", "integer") ]
                                    ++ zip (map (("value_"++).show) (Feature.indices f)) (repeat "real"))
                                   []
    --sqlTable t = [ SqlTable "feature_values"
    --                ([ ("unit"              , "integer")
    --                 , ("feature_descriptor", "integer") ] ++ zip (map (("value_"++).show) [0..maxFeatureDegree-1]) (repeat "real"))
    --                [] ]
    sqlCreate c t = mapM_ (uncurry (run' c)) $ concat $ map (insert (Unit.features t)) (Unit.table t)
        where
            insert fs (u, v) = 
                flip map fs $ \f ->
                    (printf "insert into %s values(?,?,%s)"
                       ((Feature.sqlTableName.Feature.descriptor) f)
                       (intercalate "," $ replicate ((Feature.degree.Feature.descriptor)f) "?")
                    , [DB.toSql $ Unit.id u] ++ map DB.toSql (fromU (uncurry (sliceU v) (Feature.slice f))))

