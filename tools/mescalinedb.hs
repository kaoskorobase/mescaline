import           Control.Arrow (first)
import           Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import qualified GHC.Conc as GHC
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database as DB
import           Mescaline.Database.SqlQuery as Sql
import           Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Meap.Import as Meap
import qualified Mescaline.Statistics.PCA as PCA
import           Numeric.LinearAlgebra as H
import           Database.HDBC (quickQuery')
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf (printf)
import           Prelude hiding (and)

featureTable :: Feature.Feature -> Table.Table
featureTable = Table.toTable . Feature.FeatureOf . Feature.descriptor

deleteFeature :: DB.IConnection c => c -> Feature.Feature -> IO ()
deleteFeature c f = do
    DB.run c (printf "delete from %s where unit=? and descriptor=?" (Table.name (featureTable f)))
        [DB.toSql (Feature.unit f), DB.toSql (Feature.descriptor f)]
    return ()

transformFeature :: ([[Feature.Feature]] -> [Feature.Feature]) -> FilePath -> [String] -> IO ()
transformFeature func dbFile featureNames = do
    DB.withDatabase dbFile $ \c -> do
        DB.withTransaction c $ \_ -> do
            (units, sfMap) <- unitQuery
                (DB.quickQuery' c)
                    -- (segmentation unit `eq` seg)
                    Sql.all
                    (map (fromJust . flip lookup Meap.features) featureNames)
            case units of
                Left e   -> fail e
                Right us -> do
                    case func (map snd us) of
                        [] -> return ()
                        features -> do
                            mapM_ (\f -> Table.insert c (Feature.descriptor f) >> Table.create c (featureTable f) >> deleteFeature c f >> Table.insert c f) features
                            DB.commit c

-- | Map a list of vectors to a target range.
linearMap :: Vector Double -> Vector Double -> [Vector Double] -> [Vector Double]
linearMap dstMin dstMax xs = map (\v -> (v `sub` srcMin) `mul` srcScale) xs
    where
        (srcMin, srcMax) = first fromList $ unzip $ map (\c -> (vectorMin c, vectorMax c)) (toColumns $ fromRows xs)
        srcScale         = recip (fromList srcMax `sub` srcMin)
        dstScale         = dstMax `sub` dstMin

featurePCA :: Feature.Descriptor -> [[Feature.Feature]] -> [Feature.Feature]
featurePCA d fs = zipWith (\(f:_) x -> Feature.cons (Feature.unit f) d x) fs encoded
    where
        observations = map (foldl (V.++) V.empty . map Feature.value) fs
        (encode, _)  = PCA.pca (Feature.degree d) (H.fromRows observations)
        encoded      = linearMap 0 1 (map encode observations)

-- | Analyse a directory recursively, writing the results to a database.
cmd_import :: FilePath -> FilePath -> IO ()
cmd_import dbFile dir = DB.handleSqlError
                   $ DB.withDatabase dbFile
                   $ Meap.importDirectory GHC.numCapabilities dir

cmd_query :: FilePath -> Segmentation -> String -> [String] -> IO ()
cmd_query dbFile seg pattern features = do
    (units, sfMap) <- DB.withDatabase dbFile $ \c -> do
        unitQuery (DB.quickQuery' c)
              ((url sourceFile `like` pattern) `and` (segmentation unit `eq` seg))
              (map (fromJust . flip lookup Meap.features) features)
    case units of
        Left e -> fail e
        Right us -> let ls = map (\(u, fs) -> Unique.toString (Unit.id u) : map show (concatMap (V.toList.Feature.value) fs)) us
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
cmd_transform dbFile transform dstFeature srcFeatures = DB.handleSqlError $ transformFeature func dbFile srcFeatures
    where
        func = case transform of
                "pca"     -> featurePCA (Feature.consDescriptor dstFeature 2)
                otherwise -> const []

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
                        otherwise -> putStrLn $ usage "delete DBFILE"
                "import" -> do
                    case args of
                        (dbFile:dir:[]) -> cmd_import dbFile dir
                        otherwise -> putStrLn $ usage "import DBFILE DIRECTORY"
                "insert" -> do
                    case args of
                        (dbFile:seg:name:degree:file:[]) -> cmd_insert dbFile (read seg :: Segmentation) name (read degree) file
                        otherwise -> putStrLn $ usage "insert DBFILE SEGMENTATION NAME DEGREE {FILE|-}"
                "query" -> do
                    case args of
                        (dbFile:seg:pattern:features) -> cmd_query dbFile (read seg :: Segmentation) pattern features
                        otherwise -> putStrLn $ usage "query DBFILE PATTERN SEGMENTATION FEATURE..."
                "transform" -> do
                    case args of
                        (dbFile:transform:dstFeature:srcFeatures) -> cmd_transform dbFile transform dstFeature srcFeatures
                        otherwise -> putStrLn $ usage "transform DBFILE TYPE DST_FEATURE SRC_FEATURE..."
                otherwise -> putStrLn cmdUsage
        _ -> putStrLn cmdUsage
    where
        cmdUsage = usage "{delete,import,insert,query} ARGS..."
