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
  , isStored
  , insert
) where

import           Control.Monad
import           Data.Accessor (Accessor)
import qualified Data.List as List
import           Database.HDBC (IConnection, SqlType, SqlValue, handleSqlError, quickQuery, run, toSql)
import           Mescaline.Data.ListReader (runListReader)
import           Mescaline.Data.Unique (Unique, uuid)
import           Mescaline.Database.Sql (SqlAccessor, getSqlValue, SqlExpression(..), SqlRow(..))
import           Text.Printf (printf)

-- ====================================================================
-- Table

data ColumnType = Type String
                | PrimaryKey String
                | forall a . Model a => LinksTo a

isPrimaryKey :: ColumnType -> Bool
isPrimaryKey (PrimaryKey _) = True
isPrimaryKey _              = False

isLink :: ColumnType -> Bool
isLink (LinksTo _) = True
isLink _           = False

instance SqlExpression ColumnType where
    toSqlExpression (Type s)       = s
    toSqlExpression (PrimaryKey s) = s ++ " primary key"
    toSqlExpression (LinksTo _)    = "text"

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

class (SqlRow a, Unique a) => Model a where
    toTable :: a -> Table a

toRow :: Model a => a -> [SqlValue]
toRow a = map (flip getSqlValue a) $ accessors $ toTable a

fromRow :: SqlRow a => [SqlValue] -> Either String a
fromRow = runListReader fromSqlRow

primaryKeyColumn :: Model a => a -> Maybe (Column a)
primaryKeyColumn a = List.find (isPrimaryKey.col_type) cs `mplus` List.find (isLink.col_type) cs
    where cs = columns (toTable a)

colNames :: Model a => a -> String
colNames a = "(" ++ (List.intercalate "," . fmap col_name . columns . toTable) a ++ ")"

args :: Model a => a -> String
args = List.intercalate "," . flip replicate "?" . length . columns . toTable

withSqlExpr :: Model a => (String -> String -> String) -> a -> String
withSqlExpr f a = f ((name.toTable)a) (args a)

-- | Return true if the model is stored in the database.
isStored :: (Model a, IConnection c) => c -> a -> IO Bool
isStored c a = do
    create c t
    case primaryKeyColumn a of
                Nothing  -> return False
                Just col -> (not.null) `fmap` quickQuery c (printf "select * from %s where %s = ?" (name t) (col_name col)) [toSql (uuid a)]
    where
        t = toTable a

-- | Insert instance of a model into the corrsponding table.
insert :: (Model a, IConnection c) => c -> a -> IO ()
insert c a = do
    -- TODO: optimize
    create c t
    p <- isStored c a
    unless p $ do
        let expr = printf "insert into %s values (%s)" (name t) (args a)
        handleSqlError (run c expr (toRow a))
        return ()
    where
        t = toTable a

-- ensureTables :: (SqlModel a, IConnection c) => c -> [a] -> IO ()
-- ensureTables c = fst . foldl f (return (), []) . map sqlTable
--     where
--         f (a, l) t =
--             if elem (Table.name t) l
--             then (a, l)
--             else (a >> Table.create c t, (Table.name t) : l)
