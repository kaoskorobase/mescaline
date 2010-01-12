{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ExistentialQuantification, TypeFamilies, TypeSynonymInstances #-}

module Mescaline.Database.Model where

-- import           Database.HDBC
import           Database.HDBC (IConnection, SqlType(..), SqlValue)
import qualified Database.HDBC          as DB
import qualified Database.HDBC.Sqlite3  as DB

-- import           Mescaline.Data.ListReader (ListReader, runListReader)
import qualified Mescaline.Data.ListReader as ListReader
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Table (ColumnType(..), Table, Model(..))
import qualified Mescaline.Database.Table as Table
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Sql (SqlAccessor, sqlAccessor, SqlExpression(..), SqlRow(..))
import           Mescaline.Data.Array.Vector

import           Data.List (intercalate)
import           Control.Monad (unless)
import           Text.Printf (printf)

import           Data.Accessor
import           Data.Accessor.Tuple


-- sqlQuery :: (SqlModel a, IConnection c) => c -> a -> BelongsTo a -> IO [a]
-- sqlQuery = undefined

-- ====================================================================
-- SqlRow

instance SqlRow Unique.Id where
    fromSqlRow = fromSql `fmap` ListReader.head
instance SqlRow SourceFile.Hash where
    fromSqlRow = fromSql `fmap` ListReader.head

-- ====================================================================
-- SourceFile

instance SqlType SourceFile.SourceFile where
    toSql = toSql . SourceFile.id
    fromSql = error "Cannot convert single SqlValue to record SourceFile"

instance SqlRow SourceFile.SourceFile where
    fromSqlRow = do
        i <- fromSqlRow
        u <- fromSqlRow
        h <- fromSqlRow
        return $ SourceFile.unsafeCons i u h 0 0 0

instance Model SourceFile.SourceFile where
    -- type BelongsTo SourceFile.SourceFile = ()
    toTable _ = Table.table "source_file"
                    [ ("id"   , PrimaryKey "text" , sqlAccessor SourceFile.id   )
                    , ("url"  , Type "text"       , sqlAccessor SourceFile.url  )
                    , ("hash" , Type "text"       , sqlAccessor SourceFile.hash ) ]
                    [ "id" ]
    -- sqlCreate c sf = run' c "insert into source_file values (null,?,?)" (tail $ sqlRow sf)
    --     where sqlRow sf = [(DB.toSql.SourceFile.id)sf,(DB.toSql.SourceFile.path)sf,(DB.toSql.SourceFile.hash)sf] 
    -- sqlCreate c = run' c "insert into source_file values (null,?,?)" . toSqlRow
        -- where sqlRow sf = [(DB.toSql.SourceFile.id)sf,(DB.toSql.SourceFile.path)sf,(DB.toSql.SourceFile.hash)sf] 
    -- sqlQuery c _ = quickQuery' c "select * from source_file" [] >>= mapM f
    --     where
    --         f [i, u, h] = return $ SourceFile.SourceFile (DB.fromSql i) (DB.fromSql u) (DB.fromSql h) 0 0 0
    --         f _         = fail "WTF"
 
instance SqlType Unit.Unit where
    toSql = toSql . Unit.id
    fromSql = error "Cannot convert single SqlValue to record Unit"

instance SqlRow Unit.Segmentation where
    fromSqlRow = (read.fromSql) `fmap` ListReader.head

instance SqlRow Unit.Unit where
    fromSqlRow = do
        i  <- fromSqlRow
        sf <- fromSqlRow
        s  <- fromSqlRow
        o  <- fromSqlRow
        d  <- fromSqlRow
        return $ Unit.unsafeCons i sf s o d
    
instance Model Unit.Unit where
    -- type BelongsTo Unit.Unit = SourceFile
    toTable _ = Table.table "unit"
                    [ ("id"          , PrimaryKey "text"                          , sqlAccessor Unit.id                  )
                    , ("source_file" , LinksTo (undefined::SourceFile.SourceFile) , sqlAccessor Unit.sourceFile          )
                    , ("segmentation", Type "text"                                , sqlAccessor (show.Unit.segmentation) )
                    , ("onset"       , Type "real"                                , sqlAccessor Unit.onset               )
                    , ("duration"    , Type "real"                                , sqlAccessor Unit.duration            ) ]
                    [ "id", "source_file" ]
--     sqlCreate c u = run' c "insert into unit values (null,?,?,?)" (tail $ sqlRow u)
--         where sqlRow u = [(DB.toSql.Unit.id)u, (DB.toSql.SourceFile.id.Unit.sourceFile)u,(DB.toSql.Unit.onset)u,(DB.toSql.Unit.duration)u]
--     --sqlQuery c env = quickQuery' c "select * from source_file where source_file="
--     --    where 
--     --        f [i, si, o, d] = return $ Unit.Unit (DB.fromSql i) 

instance SqlType Feature.Descriptor where
    toSql   = toSql . Feature.id
    fromSql = error "Cannot convert single SqlValue to record Unit"

instance SqlRow Feature.Descriptor where
    fromSqlRow = do
        i <- fromSqlRow
        n <- fromSqlRow
        d <- fromSqlRow
        return $ Feature.mkDescriptor i n d

instance Model Feature.Descriptor where
    -- type BelongsTo Feature.Descriptor = ()
    toTable _ = Table.table "feature_descriptor"
                    [ ("id"     , PrimaryKey "text" , sqlAccessor Feature.id    )
                    , ("name"   , Type "text"       , sqlAccessor Feature.name  )
                    , ("degree" , Type "integer"    , sqlAccessor Feature.degree) ]
                    []
--     sqlCreate c f = run' c "insert into feature_descriptor values (null,?,?)" (tail $ sqlRow f)
--         where sqlRow f = [undefined, (DB.toSql.Feature.name)f, (DB.toSql.Feature.degree)f]
--     sqlQuery c _ = quickQuery' c "select * from feature_descriptor" [] >>= mapM f
--         where
--             f [x1,x2,x3] = return $ Feature.mkDescriptor (DB.fromSql x1) (DB.fromSql x2) (DB.fromSql x3)
--             f _          = fail "WTF"

vectorAccessor i = accessor (flip indexU i) (const $ error "Vector accessor setter not implemented")

instance SqlRow Feature.Feature where
    fromSqlRow = do
        u <- fromSqlRow
        d <- fromSqlRow
        v <- fromSqlRow
        return $ Feature.mkFeature u d v

instance SqlRow Feature.Value where
    fromSqlRow = undefined

instance Model Feature.Feature where
    -- type BelongsTo Feature.Feature = ()
    toTable f = Table.table (Feature.sqlTableName (Feature.descriptor f))
                   ([ ("unit", LinksTo (undefined :: Unit.Unit), sqlAccessor Feature.unit) ]
                    ++ map (\i -> ("value_"++show i, Type "real", sqlAccessor (flip indexU i . Feature.value))) (Feature.indices (Feature.descriptor f)))
                   []

-- instance SqlModel Unit.FeatureTable where
--     type BelongsTo Unit.FeatureTable = ([SourceFile], [Feature.Descriptor])
--     sqlTable t = map mktable (Unit.descriptors t)
--         where mktable f = table (Feature.sqlTableName f)
--                                    ([ ("unit", "integer") ]
--                                     ++ zip (map (("value_"++).show) (Feature.indices f)) (repeat "real"))
--                                    []
--     --sqlTable t = [ SqlTable "feature_values"
--     --                ([ ("unit"              , "integer")
--     --                 , ("feature_descriptor", "integer") ] ++ zip (map (("value_"++).show) [0..maxFeatureDegree-1]) (repeat "real"))
--     --                [] ]
--     sqlCreate c t = mapM_ (uncurry (run' c)) $ concat $ map (insert (Unit.features t)) (Unit.table t)
--         where
--             insert fs (u, v) = 
--                 flip map fs $ \f ->
--                     (printf "insert into %s values(?,?,%s)"
--                        ((Feature.sqlTableName.Feature.descriptor) f)
--                        (intercalate "," $ replicate ((Feature.degree.Feature.descriptor)f) "?")
--                     , [DB.toSql $ Unit.id u] ++ map DB.toSql (fromU (uncurry (sliceU v) (Feature.slice f))))
-- 
