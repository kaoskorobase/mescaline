import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Ord (comparing)
import qualified Data.Vector.Generic as V
import qualified Mescaline.Analysis as Analysis
import qualified Mescaline.Analysis.Meap as Meap
import qualified Mescaline.Analysis.SonicAnnotator as SonicAnnotator
import qualified Mescaline.Database as DB
import           System.Environment (getArgs)
import           System.IO
import           Prelude hiding (and)

data Analyser a = Analyser {
    analyser :: a
  , mfccFeatureName :: String
}

-- sonicAnnotator 
-- analyser = SonicAnnotator.analyser
-- mfccFeatureName = "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc"

-- defaultAnalyser :: Analyser
defaultAnalyser = Analyser (SonicAnnotator.analyser (SonicAnnotator.Fixed 0.6))
                           "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc"
-- defaultAnalyser = Analyser (Meap.analyser "resources/meap/2.0") "com.meapsoft.AvgMFCC"

-- | Analyse a directory recursively, writing the results to a database.
cmd_import :: FilePath -> [FilePath] -> IO ()
cmd_import dbFile paths = do
    Analysis.importPaths (analyser defaultAnalyser) Nothing dbFile paths
    DB.withDatabase dbFile $
        DB.transformFeature
            (DB.PCA 2)
            "es.globero.mescaline.spectral"
            [mfccFeatureName defaultAnalyser]

cmd_query :: FilePath -> String -> [String] -> IO ()
cmd_query dbFile pattern features = do
    (_, us) <- DB.withDatabase dbFile $ DB.query pattern features
    -- case units of
    --     Left e -> fail e
    --     Right (us, _) -> let ls = map (\(u, fs) -> Unique.toString (Unit.id u) : map show (concatMap (V.toList.Feature.value) fs)) us
    --                      in putStr $ unlines (map unwords ls)
    mapM_ print (Map.toList us)

cmd_dump :: FilePath -> String -> [String] -> IO ()
cmd_dump dbFile pattern features = do
    (_, unitMap) <- DB.withDatabase dbFile $ DB.query pattern features
    let units = List.sortBy (comparing fst) (Map.assocs unitMap)
    mapM_ (uncurry dump) units
    where
        dump uid (u, fs) =
            putStrLn
                $ List.intercalate ","
                $ [show (DB.hashUnitId uid), show (DB.unitOnset u), show (DB.unitDuration u)]
                    ++ concatMap (map show.V.toList.DB.toVector.DB.featureValue) fs

cmd_insert :: FilePath -> String -> Int -> FilePath -> IO ()
cmd_insert _ _ _ _ = return ()
-- cmd_insert dbFile _ name degree file = do
--     rows <- if file == "-" then read_dlm stdin else withFile file ReadMode read_dlm
--     let units = map head rows
--         rowData = map (map read . take degree . tail) rows :: [[Double]]
--         desc = Feature.consDescriptor name degree
--         features = zipWith (\u r -> Feature.cons (Unique.unsafeFromString u) desc (V.fromList r)) units rowData
--         table = Table.toTable (Feature.FeatureOf desc)
--     putStrLn $ printf "Feature %s into table `%s'" (show desc) (Table.name table)
--     DB.withDatabase dbFile $ \c -> do
--         DB.handleSqlError $ DB.run c (printf "drop table %s" (Table.name table)) []
--         mapM_ (Table.insert c) features
--         DB.commit c
--     where
--         read_dlm = fmap (map words . lines) . hGetContents

cmd_delete :: FilePath -> IO ()
cmd_delete _ = return ()

cmd_transform :: FilePath -> String -> String -> [String] -> IO ()
cmd_transform dbFile transName dstFeature srcFeatures = do
    trans <- case transName of
                "pca" -> return (DB.PCA 2)
                _     -> fail $ "Unknown transform: " ++ transName
    DB.withDatabase dbFile $
        DB.transformFeature
            trans
            dstFeature
            srcFeatures

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
                "dump" -> do
                    case args of
                        (dbFile:pattern:features) -> cmd_dump dbFile pattern features
                        _ -> putStrLn $ usage "dump DBFILE PATTERN FEATURE..."
                "import" -> do
                    case args of
                        (dbFile:paths) -> cmd_import dbFile paths
                        _ -> putStrLn $ usage "import DBFILE DIRECTORY"
                "insert" -> do
                    case args of
                        (dbFile:name:degree:file:[]) -> cmd_insert dbFile name (read degree) file
                        _ -> putStrLn $ usage "insert DBFILE NAME DEGREE {FILE|-}"
                "query" -> do
                    case args of
                        (dbFile:pattern:features) -> cmd_query dbFile pattern features
                        _ -> putStrLn $ usage "query DBFILE PATTERN FEATURE..."
                "transform" -> do
                    case args of
                        (dbFile:transform:dstFeature:srcFeatures) -> cmd_transform dbFile transform dstFeature srcFeatures
                        _ -> putStrLn $ usage "transform DBFILE TYPE DST_FEATURE SRC_FEATURE..."
                _ -> putStrLn cmdUsage
        _ -> putStrLn cmdUsage
    where
        cmdUsage = usage "{delete,dump,import,insert,query,transform} ARGS..."
