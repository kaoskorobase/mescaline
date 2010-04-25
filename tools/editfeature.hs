import           Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database as DB
import           Mescaline.Database.SqlQuery
import           Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Table as Table
import           Mescaline.Meap.Import as Meap
import           Database.HDBC (quickQuery')
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf (printf)
import           Prelude hiding (and)

cmd_query :: FilePath -> Segmentation -> String -> [String] -> IO ()
cmd_query dbFile seg pattern features = do
    (units, sfMap) <- withDatabase dbFile $ \c -> do
        unitQuery (quickQuery' c)
              ((url sourceFile `like` pattern) `and` (segmentation unit `eq` seg))
              (map (fromJust . flip lookup Meap.features) features)
    case units of
        Left e -> fail e
        Right us -> let ls = map (\(u, fs) -> Unique.toString (Unit.id u) : map show (concatMap (V.toList.Feature.value) fs)) us
                    in putStr $ unlines (map unwords ls)

cmd_insert :: FilePath -> Segmentation -> String -> Int -> FilePath -> IO ()
cmd_insert dbFile seg name degree file = do
    rows <- if file == "-" then read_dlm stdin else withFile file ReadMode read_dlm
    let units = map head rows
        rowData = map (map read . take degree . tail) rows :: [[Double]]
        desc = Feature.consDescriptor name degree
        features = zipWith (\u r -> Feature.cons (Unique.unsafeFromString u) desc (V.fromList r)) units rowData
        table = Table.toTable (Feature.FeatureOf desc)
    putStrLn $ printf "Feature %s into table `%s'" (show desc) (Table.name table)
    DB.withDatabase dbFile $ \c -> do
        DB.handleSqlError $ DB.run c (printf "drop table %s" (Table.name table)) []
        mapM_ (Table.insert c) features
        DB.commit c
    where
        read_dlm = fmap (map words . lines) . hGetContents

cmd_delete = undefined

main = do
    (dbFile:cmd:args) <- getArgs
    case cmd of
        "query" -> do
            case args of
                (seg:pattern:features) -> cmd_query dbFile (read seg :: Segmentation) pattern features
                otherwise              -> putStrLn "Usage: editfeature DBFILE query PATTERN SEGMENTATION FEATURE..."
        "insert" -> do
            case args of
                (seg:name:degree:file:[]) -> cmd_insert dbFile (read seg :: Segmentation) name (read degree) file
                otherwise                 -> putStrLn "Usage: editfeature DBFILE insert SEGMENTATION NAME DEGREE {FILE|-}"
        otherwise -> putStrLn "Usage: editfeature DBFILE {query,insert,delete} ... ARGS"
