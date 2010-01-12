module Mescaline.Database.SqlQuery where

import           Control.Arrow
import           Control.Monad.Writer
import           Data.Foldable as Seq
import           Data.Sequence as Seq
import           Database.HDBC (SqlType(..), SqlValue)
import           Mescaline
import qualified Mescaline.Database.Model ()
import qualified Mescaline.Database.SourceFile as SourceFile
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Database.Unit as Unit
import           Prelude hiding (and, not, or)
import           Text.Printf (printf)

data Query = Query String [SqlValue]
           | And Query Query
           | Or Query Query
           | Not Query
           deriving (Show)

-- Combinators

and :: Query -> Query -> Query
and = And

or :: Query -> Query -> Query
or = Or

not :: Query -> Query
not = Not

-- SourceFile

sourceFile :: SourceFile.SourceFile
sourceFile = undefined

sourceFile_url :: String
sourceFile_url = sourceFile `field` "url"

binop :: SqlType b => String -> String -> b -> Query
binop a o b = Query (a ++ " " ++ o ++ " ?") [toSql b]

field :: Table.Model a => a -> String -> String
field a fn = Table.name (Table.toTable a) ++ "." ++ fn

url :: SourceFile.URL -> Query
url = binop sourceFile_url "="

match :: String -> Query
match = binop sourceFile_url "like"

segmentations :: [Unit.Segmentation] -> Query
segmentations = undefined

segmentation :: Unit.Segmentation -> Query
segmentation s = segmentations [s]

onsetWithin :: Time -> Time -> Query
onsetWithin = undefined

onsetGT :: Time -> Query
onsetGT t = onsetWithin t (1/0)

onsetLT :: Time -> Query
onsetLT t = onsetWithin 0 t

durationWithin :: Duration -> Duration -> Query
durationWithin = undefined

durationGT :: Duration -> Query
durationGT t = durationWithin t (1/0)

durationLT :: Duration -> Query
durationLT t = durationWithin 0 t

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

query :: Query -> (String, [SqlValue])
query q = (printf "SELECT %s FROM %s WHERE %s" a b cond, args)
    where
        a = ""
        b = ""
        (cond, args) = queryToSql q
