{-# LANGUAGE ExistentialQuantification #-}

module Mescaline.Database.Table (
  -- *Table construction
    ColumnType(..)
  , Table
  , name, columns, indexes
  , table
  , accessors
  , create
  -- *Model access
  , Model(..)
  , insert
) where

import           Data.Accessor (Accessor)
import qualified Data.List as List
import           Database.HDBC (IConnection, SqlType, SqlValue, run)
import           Mescaline.Data.ListReader (runListReader)
import           Mescaline.Database.Sql (SqlAccessor, getSqlValue, SqlExpression(..), SqlRow(..))
import           Text.Printf (printf)

-- ====================================================================
-- Table

data ColumnType = Type String
                | PrimaryKey String
                | forall a . Model a => LinksTo a

instance SqlExpression ColumnType where
    toSqlExpression (Type s)       = s
    toSqlExpression (PrimaryKey s) = s ++ " primary key"
    toSqlExpression (LinksTo _)    = "blob"

data Column a = Column {
    col_name     :: String
  , col_type     :: ColumnType
  , col_accessor :: SqlAccessor a
}

data Table a = Table {
    name    :: String
  , columns :: [Column a]
  , indexes :: [String]
}

table :: String -> [(String, ColumnType, SqlAccessor a)] -> [String] -> Table a
table name cols idxs = Table name (map f cols) idxs
    where f (x1, x2, x3) = Column x1 x2 x3

accessors :: Table a -> [SqlAccessor a]
accessors = map col_accessor . columns

newtype CreateTable a = CreateTable (Table a)

instance SqlExpression (CreateTable a) where
    toSqlExpression (CreateTable t) =
        printf "create table if not exists %s (%s);" (name t) (args (columns t))
        -- printf "create index %s on %s(%s);" (f name) name (intercalate ", " cols)
        where
            args = List.intercalate ", " . map (\c -> col_name c ++ " " ++ toSqlExpression (col_type c))

create :: IConnection c => c -> Table a -> IO ()
create c t = run c (toSqlExpression (CreateTable t)) [] >> return ()

-- ====================================================================
-- Model

class SqlRow a => Model a where
    toTable :: a -> Table a

toRow :: Model a => a -> [SqlValue]
toRow a = map (flip getSqlValue a) $ accessors $ toTable a

fromRow :: SqlRow a => [SqlValue] -> Either String a
fromRow = runListReader fromSqlRow

args :: Model a => a -> String
args = List.intercalate "," . flip replicate "?" . length . columns . toTable

withSqlExpr :: Model a => (String -> String -> String) -> a -> String
withSqlExpr f a = f ((name.toTable)a) (args a)

-- | Insert instance of a mode into the corrsponding table.
insert :: (Model a, IConnection c) => c -> a -> IO ()
insert c a = do
    -- TODO: optimize
    create c (toTable a)
    run c (withSqlExpr (printf "insert into %s values (%s)") a) (toRow a)
    return ()

-- ensureTables :: (SqlModel a, IConnection c) => c -> [a] -> IO ()
-- ensureTables c = fst . foldl f (return (), []) . map sqlTable
--     where
--         f (a, l) t =
--             if elem (Table.name t) l
--             then (a, l)
--             else (a >> Table.create c t, (Table.name t) : l)
