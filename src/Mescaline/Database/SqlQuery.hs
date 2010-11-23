module Mescaline.Database.SqlQuery where

import           Control.Arrow
import qualified Control.Monad.Error as Error
import           Control.Monad.Writer
import qualified Control.Monad.State.Strict as State
import           Control.Seq
import qualified Data.Foldable as Seq
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Sequence as Seq
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.Model ()
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Sql as Sql
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Database.Unit as Unit
import           Prelude hiding (all, and, elem, not, or)
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

query :: String -> [SqlValue] -> Query
query = Query

query_ :: String -> Query
query_ = flip query []

type Operator = String

binop :: SqlType b => Operator -> Field -> b -> Query
binop o (Field t f) b = query (t ++ "." ++ f ++ " " ++ o ++ " ?") [toSql b]

all :: Query
all = query "1" []

eq :: SqlType a => Field -> a -> Query
eq = binop "="

elem :: SqlType a => Field -> [a] -> Query
elem f = foldl1 and . map (eq f)

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

queryCond :: Query -> (String, [SqlValue])
queryCond = second Seq.toList . runWriter . mkquery

tables :: [String]
tables = [ Table.name $ Table.toTable (undefined :: SourceFile.SourceFile)
         , Table.name $ Table.toTable (undefined :: Unit.Unit) ]

type SqlQueryFunc = String -> [SqlValue] -> IO [[SqlValue]]
type SourceFileMap = Unique.Map SourceFile.SourceFile

-- | Return an Sql query string with arguments for a 'Unit' query.
unitQueryString :: Query -> [Feature.Descriptor] -> (String, [SqlValue])
unitQueryString q ds = (printf "SELECT %s FROM %s JOIN %s ON %s WHERE %s"
                        (List.intercalate "," cols)
                        (Table.name sfTable)
                        (List.intercalate "," (Table.name unitTable : descTables))
                        (foldl (\acc dt -> acc ++ " and " ++ "(" ++ dt ++ "." ++ Table.name unitTable ++ " = " ++ Table.name unitTable ++ ".id" ++ ")")
                               (Table.name sfTable ++ ".id = " ++ Table.name unitTable ++ "." ++ Table.name sfTable)
                               descTables)
                        cond, args)
    where
        sfTable = Table.toTable (undefined :: SourceFile.SourceFile)
        unitTable = Table.toTable (undefined :: Unit.Unit)
        descTables = map Feature.sqlTableName ds
        cols = Table.prefixedColumnNames_ sfTable
            ++ Table.prefixedColumnNames_ unitTable
            ++ concatMap (\d -> map ((Feature.sqlTableName d ++ ".") ++) (Feature.sqlColumnNames d)) ds
        (cond, args) = queryCond q

-- getSourceFileMap :: SqlQueryFunc -> IO (Either String SourceFileMap)
-- getSourceFileMap action = do
--     rows <- action queryString []
--     List.foldl' (\row -> 
--     where
--         sfTable = Table.toTable (undefined :: SourceFile.SourceFile)
--         queryString = printf "SELECT * FROM %s" (Table.name sfTable)

unitQuery :: SqlQueryFunc -> Query -> [Feature.Descriptor] -> IO (Either String ([(Unit.Unit, [Feature.Feature])], SourceFileMap))
unitQuery action q ds = do
    -- print (unitQueryString q ds)
    rows <- uncurry action (unitQueryString q ds)
    -- print $ Prelude.length rows
    case State.runState (Error.runErrorT (mapM readUnit rows)) Map.empty of
        (Right us, sfMap) -> seqList (seqTuple2 rseq (seqList rseq)) us `seq` return (Right (us, sfMap))
        (Left e, _)       -> return (Left e)
    where
        -- readUnit :: [SqlValue] -> Error.ErrorT String (State.State SourceFileMap) Unit.Unit
        readUnit xs = do
            let get = do
                u <- Sql.getRow
                fs <- mapM (Feature.getSql (Unit.id u)) ds
                return (u, fs)
            case Sql.execGetSql get xs of
                Left e -> Error.throwError e
                Right (u, fs) -> do
                    sfMap <- State.lift State.get
                    let sf = Unit.sourceFile u
                        sfid = SourceFile.id sf
                    sf' <- case Map.lookup sfid sfMap of
                            Nothing -> do
                                let sfMap' = Map.insert sfid sf sfMap
                                seqMap rseq rseq sfMap' `seq` State.lift (State.put sfMap')
                                return sf
                            Just sf' ->
                                return sf'
                    return (Unit.unsafeCons
                                (Unit.id u) sf'
                                (Unit.segmentation u)
                                (Unit.onset u)
                                (Unit.duration u), fs)

unitQuery_ :: (String -> [SqlValue] -> IO [[SqlValue]]) -> Query -> IO (Either String ([Unit.Unit], SourceFileMap))
unitQuery_ f q = do
    res <- unitQuery f q []
    case res of
        Right (us, sfMap) -> return $ Right (map fst us, sfMap)
        Left e            -> return $ Left e
    