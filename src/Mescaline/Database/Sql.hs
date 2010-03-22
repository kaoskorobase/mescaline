{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances #-}

module Mescaline.Database.Sql (
    SqlAccessor
  , sqlAccessor
  , getSqlValue
  , SqlExpression(..)
  , SqlRow(..)
) where

import           Database.HDBC (SqlType(..), SqlValue)
import           Mescaline.Data.ListReader (ListReader)
import qualified Mescaline.Data.ListReader as ListReader

data SqlAccessor a = forall b . SqlType b => SqlAccessor (a -> b)

sqlAccessor :: SqlType b => (a -> b) -> SqlAccessor a
sqlAccessor = SqlAccessor

getSqlValue :: SqlAccessor a -> a -> SqlValue
getSqlValue (SqlAccessor f) a = toSql (f a)


class SqlExpression a where
    toSqlExpression :: a -> [String]


class SqlRow a where
    fromSqlRow :: ListReader SqlValue a

fromSqlRow_SqlType :: (SqlType a, SqlRow a) => ListReader SqlValue a
fromSqlRow_SqlType = fromSql `fmap` ListReader.head

instance SqlRow String where
    fromSqlRow = fromSqlRow_SqlType
instance SqlRow Int where
    fromSqlRow = fromSqlRow_SqlType
instance SqlRow Double where
    fromSqlRow = fromSqlRow_SqlType
