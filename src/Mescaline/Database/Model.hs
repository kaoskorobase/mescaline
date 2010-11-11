{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ExistentialQuantification, TypeFamilies, TypeSynonymInstances #-}

module Mescaline.Database.Model () where

import qualified Data.Vector.Generic as V

import           Database.HDBC (SqlType(..), SqlValue)
import qualified Mescaline.Data.ListReader as ListReader
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Table (ColumnType(..), Model(..))
import qualified Mescaline.Database.Table as Table
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Sql (SqlRow(..))
import qualified Mescaline.Database.Sql as Sql

-- ====================================================================
-- SqlRow

instance SqlRow Unique.Id where
    putRow = ListReader.put . toSql
    getRow = fmap fromSql ListReader.head
instance SqlRow SourceFile.Hash where
    putRow = ListReader.put . toSql
    getRow = fmap fromSql ListReader.head

-- ====================================================================
-- SourceFile

instance SqlType SourceFile.SourceFile where
    toSql = toSql . SourceFile.id
    fromSql = error "Cannot convert single SqlValue to record SourceFile"

instance SqlRow SourceFile.SourceFile where
    putRow x = do
        putRow $ SourceFile.id          x
        putRow $ SourceFile.url         x
        putRow $ SourceFile.hash        x
        putRow $ SourceFile.numChannels x
        putRow $ SourceFile.sampleRate  x
        putRow $ SourceFile.frames      x
    getRow = do
        i <- getRow
        u <- getRow
        h <- getRow
        nc <- getRow
        sr <- getRow
        nf <- getRow
        return $ SourceFile.unsafeCons i u h nc sr nf

instance Model SourceFile.SourceFile where
    toTable _ = Table.table "source_file"
                    [ ("id"   ,      PrimaryKey "text")
                    , ("url"  ,      Type "text"      )
                    , ("hash" ,      Type "text"      )
                    , ("channels",   Type "integer"   )
                    , ("sampleRate", Type "real"      )
                    , ("frames",     Type "integer"   ) ]
                    [ "id" ]
 
instance SqlType Unit.Unit where
    toSql   = toSql . Unit.id
    fromSql = error "Cannot convert single SqlValue to record Unit"

instance SqlType Unit.Segmentation where
	fromSql = read . fromSql
	toSql   = toSql . show

instance SqlRow Unit.Segmentation where
    putRow = ListReader.put . toSql
    getRow = fmap fromSql ListReader.head

instance SqlRow Unit.Unit where
    putRow x = do
        putRow $ Unit.id                         x
        putRow $ SourceFile.id.Unit.sourceFile $ x
        putRow $ Unit.segmentation               x
        putRow $ Unit.onset                      x
        putRow $ Unit.duration                   x
    getRow = do
        sf <- getRow
        i  <- getRow
        sfid <- getRow :: ListReader.ListReader SqlValue Unique.Id
        s  <- getRow
        o  <- getRow
        d  <- getRow
        return $ Unit.unsafeCons i sf s o d
    
instance Model Unit.Unit where
    toTable _ = Table.table "unit"
                    [ ("id"          , PrimaryKey "text"                         )
                    , ("source_file" , LinksTo (undefined::SourceFile.SourceFile))
                    , ("segmentation", Type "text"                               )
                    , ("onset"       , Type "real"                               )
                    , ("duration"    , Type "real"                               ) ]
                    [ "id", "source_file" ]

instance SqlType Feature.Descriptor where
    toSql   = toSql . Feature.id
    fromSql = error "Cannot convert single SqlValue to record Unit"

instance SqlRow Feature.Descriptor where
    putRow x = do
        putRow $ Feature.id     x
        putRow $ Feature.name   x
        putRow $ Feature.degree x
    getRow = do
        i <- getRow
        n <- getRow
        d <- getRow
        return $ Feature.mkDescriptor i n d

instance Model Feature.Descriptor where
    toTable _ = Table.table "feature_descriptor"
                    [ ("id"     , PrimaryKey "text")
                    , ("name"   , Type "text"      )
                    , ("degree" , Type "integer"   ) ]
                    []

instance Model Feature.FeatureOf where
    toTable (Feature.FeatureOf desc) =
        Table.table (Feature.sqlTableName desc)
            ([ ("unit", LinksTo (undefined :: Unit.Unit))
             , ("descriptor", LinksTo (undefined :: Feature.Descriptor))
             , ("length", Type "integer") ]
             ++ map (\i -> ("value_" ++ show i, Type "real")) (Feature.indices desc))
            []

instance SqlRow Feature.Value where
    putRow x = putRow (V.length x) >> mapM_ putRow (V.toList x)
    getRow = getRow >>= sequence . flip replicate getRow >>= return . V.fromList

instance SqlRow Feature.Feature where
    putRow x = do
        putRow $ Feature.unit                  $ x
        putRow $ Feature.id.Feature.descriptor $ x
        putRow $ Feature.value                   x
    getRow = do
        u <- getRow
        d <- getRow
        v <- getRow
        return $ Feature.mkFeature u d v

instance Model Feature.Feature where
    toTable f = Table.table (Feature.sqlTableName (Feature.descriptor f))
                   ([ ("unit", LinksTo (undefined :: Unit.Unit))
                    , ("descriptor", LinksTo (undefined :: Feature.Descriptor))
                    , ("length", Type "integer") ]
                    ++ map (\i -> ("value_"++show i, Type "real")) (Feature.indices (Feature.descriptor f)))
                   []
