import           Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database as DB
import           Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Analysis.Meap as Meap
import qualified Mescaline.Synth.Database.Model as DB
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf (printf)
import           Prelude hiding (and)

-- | Analyse a directory recursively, writing the results to a database.
cmd_import :: FilePath -> [FilePath] -> IO ()
cmd_import dbFile paths = do
    DB.importPaths dbFile paths
    DB.transformFeature dbFile DB.PCA (Feature.consDescriptor "es.globero.mescaline.spectral" 2)
                                      [fromJust (Meap.lookupFeature "com.meapsoft.AvgMFCC")]

cmd_query :: FilePath -> Segmentation -> String -> [String] -> IO ()
cmd_query dbFile seg pattern features = do
    units <- DB.query dbFile seg pattern (map (fromJust . Meap.lookupFeature) features)
    case units of
        Left e -> fail e
        Right (us, _) -> let ls = map (\(u, fs) -> Unique.toString (Unit.id u) : map show (concatMap (V.toList.Feature.value) fs)) us
                         in putStr $ unlines (map unwords ls)

cmd_insert :: FilePath -> Segmentation -> String -> Int -> FilePath -> IO ()
cmd_insert dbFile _ name degree file = do
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

cmd_delete :: FilePath -> IO ()
cmd_delete _ = return ()

cmd_transform :: FilePath -> String -> String -> [String] -> IO ()
cmd_transform dbFile transName dstFeature srcFeatures = do
    trans <- case transName of
                "pca" -> return DB.PCA
                _     -> fail $ "Unknown transform: " ++ transName
    DB.handleSqlError $ DB.transformFeature
                            dbFile trans
                            (Feature.consDescriptor dstFeature 2)
                            (map (fromJust . Meap.lookupFeature) srcFeatures)

usage :: String -> String
usage = ("Usage: mkdb " ++)

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        (cmd:args) ->
            case cmd of
                "delete" -> do
                    case args of
                        (dbFile:[]) -> cmd_delete dbFile
                        _ -> putStrLn $ usage "delete DBFILE"
                "import" -> do
                    case args of
                        (dbFile:paths) -> cmd_import dbFile paths
                        _ -> putStrLn $ usage "import DBFILE DIRECTORY"
                "insert" -> do
                    case args of
                        (dbFile:seg:name:degree:file:[]) -> cmd_insert dbFile (read seg :: Segmentation) name (read degree) file
                        _ -> putStrLn $ usage "insert DBFILE SEGMENTATION NAME DEGREE {FILE|-}"
                "query" -> do
                    case args of
                        (dbFile:seg:pattern:features) -> cmd_query dbFile (read seg :: Segmentation) pattern features
                        _ -> putStrLn $ usage "query DBFILE PATTERN SEGMENTATION FEATURE..."
                "transform" -> do
                    case args of
                        (dbFile:transform:dstFeature:srcFeatures) -> cmd_transform dbFile transform dstFeature srcFeatures
                        _ -> putStrLn $ usage "transform DBFILE TYPE DST_FEATURE SRC_FEATURE..."
                _ -> putStrLn cmdUsage
        _ -> putStrLn cmdUsage
    where
        cmdUsage = usage "{delete,import,insert,query} ARGS..."
