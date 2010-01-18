module Mescaline.Database.SqlQuery where

import           Control.Arrow
import           Control.Monad.Writer
import qualified Data.Foldable as Seq
import           Data.List (intercalate)
import           Data.Sequence as Seq
import           Database.HDBC (SqlType(..), SqlValue)
import           Mescaline
import qualified Mescaline.Database.Model ()
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.SourceFile as SourceFile
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Database.Unit as Unit
import           Prelude hiding (and, not, or)
import           Text.Printf (printf)

data Field = Field String String

field :: Table.Model a => a -> String -> Field
field a = Field (Table.name (Table.toTable a))

data Model =
    SourceFile {
        uid :: Field
      , url :: Field
    }
  | Unit {
        uid :: Field
      , segmentation :: Field
      , onset :: Field
      , duration :: Field
    }

sourceFile :: Model
sourceFile = SourceFile {
    uid = field (undefined :: SourceFile.SourceFile) "id"
  , url = field (undefined :: SourceFile.SourceFile) "url" }

unit :: Model
unit = Unit {
    uid          = field (undefined :: Unit.Unit) "id"
  , segmentation = field (undefined :: Unit.Unit) "segmentation"
  , onset        = field (undefined :: Unit.Unit) "onset"
  , duration     = field (undefined :: Unit.Unit) "duration" }


type Operator = String

binop :: SqlType b => Operator -> Field -> b -> Query
binop o (Field t f) b = Query (t ++ "." ++ f ++ " " ++ o ++ " ?") [toSql b]

eq :: SqlType a => Field -> a -> Query
eq = binop "="

gt :: SqlType a => Field -> a -> Query
gt = binop ">"

lt :: SqlType a => Field -> a -> Query
lt = binop "<"

data Query = Query String [SqlValue]
           | And Query Query
           | Or Query Query
           | Not Query
           deriving (Show)

and :: Query -> Query -> Query
and = And

or :: Query -> Query -> Query
or = Or

not :: Query -> Query
not = Not

like :: Field -> String -> Query
like = binop "like"

ofSegmentation :: String -> Query
ofSegmentation = eq (segmentation unit)

ofSegmentations :: [String] -> Query
ofSegmentations = foldl1 and . map ofSegmentation

mkquery :: Query -> Writer (Seq SqlValue) String
mkquery (Query s as) = tell (Seq.fromList as) >> return s
mkquery (And q1 q2) = do
    q1' <- mkquery q1
    q2' <- mkquery q2
    return $ "(" ++ q1' ++ ") AND (" ++ q2' ++ ")"
mkquery (Or q1 q2) = do
    q1' <- mkquery q1
    q2' <- mkquery q2
    return $ "(" ++ q1' ++ ") OR (" ++ q2' ++ ")"
mkquery (Not q) = do
    q' <- mkquery q
    return $ "NOT (" ++ q' ++ ")"

queryToSql :: Query -> (String, [SqlValue])
queryToSql = second Seq.toList . runWriter . mkquery

tables :: [String]
tables = [ Table.name $ Table.toTable (undefined :: SourceFile.SourceFile)
         , Table.name $ Table.toTable (undefined :: Unit.Unit) ]

query :: Query -> (String, [SqlValue])
query q = (printf "SELECT * FROM %s WHERE %s" ts cond, args)
    where
        ts = intercalate ", " tables
        (cond, args) = queryToSql q
