module Mescaline.Synth.Database.Model (
    importPaths
  , query
  , Transform(..)
  , transformFeature
) where

import           Control.Arrow (first)
import           Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import qualified GHC.Conc as GHC
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database as DB
import           Mescaline.Database.SqlQuery (and, eq, like, segmentation, sourceFile, unit, url)
import qualified Mescaline.Database.SqlQuery as Sql
import           Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Meap.Import as Meap
import qualified Mescaline.Statistics.PCA as PCA
import           Numeric.LinearAlgebra as H
import           Database.HDBC (quickQuery')
import           System.Environment (getArgs)
import           System.IO
import           Text.Printf (printf)
import           Prelude hiding (and)

-- | Initialize tables before trying to use them.
--
-- Automatic initialization is only performed for inserts, not for selects.
initTables :: [Feature.Descriptor] -> DB.Connection -> IO ()
initTables fs c = do
    Table.create c (Table.toTable (undefined::SourceFile.SourceFile))
    Table.create c (Table.toTable (undefined::Unit.Unit))
    Table.create c (Table.toTable (undefined::Feature.Descriptor))
    mapM_ (Table.create c . Table.toTable . Feature.FeatureOf) fs
    DB.commit c

featureTable :: Feature.Feature -> Table.Table
featureTable = Table.toTable . Feature.FeatureOf . Feature.descriptor

deleteFeature :: DB.IConnection c => c -> Feature.Feature -> IO ()
deleteFeature c f = do
    DB.run c (printf "delete from %s where unit=? and descriptor=?" (Table.name (featureTable f)))
        [DB.toSql (Feature.unit f), DB.toSql (Feature.descriptor f)]
    return ()

transformFeature' :: ([[Feature.Feature]] -> [Feature.Feature]) -> FilePath -> [Feature.Descriptor] -> IO ()
transformFeature' func dbFile features = do
    DB.withDatabase dbFile $ \c -> do
        initTables features c
        DB.withTransaction c $ \_ -> do
            res <- Sql.unitQuery
                (DB.quickQuery' c)
                    -- (segmentation unit `eq` seg)
                    Sql.all
                    features
            case res of
                Left e -> fail e
                Right (us, _) -> do
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
importPaths :: FilePath -> [FilePath] -> IO ()
importPaths dbFile paths = DB.handleSqlError
                           $ DB.withDatabase dbFile
                           $ Meap.importPaths Nothing paths

query :: FilePath -> Segmentation -> String -> [Feature.Descriptor] -> IO (Either String ([(Unit.Unit, [Feature.Feature])], Sql.SourceFileMap))
query dbFile seg pattern features = do
    DB.withDatabase dbFile $ \c -> do
        initTables features c
        Sql.unitQuery (DB.quickQuery c)
              ((url sourceFile `like` pattern) `and` (segmentation unit `eq` seg))
              features

-- cmd_insert :: FilePath -> Segmentation -> String -> Int -> FilePath -> IO ()
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

-- cmd_delete :: FilePath -> IO ()
-- cmd_delete _ = return ()

data Transform = PCA deriving (Eq, Show)

transformFeature :: FilePath -> Transform -> Feature.Descriptor -> [Feature.Descriptor] -> IO ()
transformFeature dbFile transform dstFeature srcFeatures = DB.handleSqlError $ transformFeature' func dbFile srcFeatures
    where
        func = case transform of
                PCA -> featurePCA dstFeature
