{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances #-}

module Mescaline.Database.Sql (
    SqlAccessor
  , sqlAccessor
  , getSqlValue
  , SqlExpression(..)
  , PutSql
  , runPutSql
  , execPutSql
  , GetSql
  , runGetSql
  , execGetSql
  , SqlRow(..)
  , toRow
  , fromRow
  , SqlType(..)
  , SqlValue
) where

import           Database.HDBC (SqlType(..), SqlValue)
import           Mescaline.Data.ListReader (ListReader, ListWriter)
import qualified Mescaline.Data.ListReader as ListReader

data SqlAccessor a = forall b . SqlType b => SqlAccessor (a -> b)

sqlAccessor :: SqlType b => (a -> b) -> SqlAccessor a
sqlAccessor = SqlAccessor

getSqlValue :: SqlAccessor a -> a -> SqlValue
getSqlValue (SqlAccessor f) a = toSql (f a)

class SqlExpression a where
    toSqlExpression :: a -> [String]

type GetSql a = ListReader SqlValue a

runGetSql :: GetSql a -> [SqlValue] -> Either String (a, [SqlValue])
runGetSql = ListReader.runListReader

execGetSql :: GetSql a -> [SqlValue] -> Either String a
execGetSql = ListReader.execListReader

type PutSql a = ListWriter SqlValue a

runPutSql :: PutSql a -> (a, [SqlValue])
runPutSql = ListReader.runListWriter

execPutSql :: PutSql a -> [SqlValue]
execPutSql = ListReader.execListWriter

class SqlRow a where
    putRow :: a -> PutSql ()
    getRow :: GetSql a

putRow_SqlType :: (SqlType a) => a -> PutSql ()
putRow_SqlType = ListReader.put . toSql

getRow_SqlType :: (SqlType a) => GetSql a
getRow_SqlType = fmap fromSql ListReader.head

instance SqlRow String where
    putRow = putRow_SqlType
    getRow = getRow_SqlType

instance SqlRow Int where
    putRow = putRow_SqlType
    getRow = getRow_SqlType

instance SqlRow Double where
    putRow = putRow_SqlType
    getRow = getRow_SqlType

toRow :: SqlRow a => a -> [SqlValue]
toRow = execPutSql . putRow

fromRow :: SqlRow a => [SqlValue] -> Either String a
fromRow = execGetSql getRow
