module Mescaline.Synth.Database.Model (
    importPaths
  , query
  , Transform(..)
  , transformFeature
) where

import           Control.Arrow (first)
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import           Database.Persist.Sqlite
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Vector.Storable as SV
import qualified Mescaline.Database as DB
import           Mescaline.Database.SqlQuery (and, eq, like, segmentation, sourceFile, unit, url)
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Entity as DB
import qualified Database.Persist as DB
import           Mescaline.Database.Unit (Segmentation(..))
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import qualified Mescaline.Database.Table as Table
import qualified Mescaline.Analysis as Analysis
import qualified Mescaline.Analysis.Meap as Meap
import qualified Mescaline.Analysis.SonicAnnotator as SonicAnnotator
import qualified Mescaline.Statistics.PCA as PCA
import           Numeric.LinearAlgebra as H
import           System.IO
import           Text.Printf (printf)
import           Text.Regex
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
    _ <- DB.run c (printf "delete from %s where unit=? and descriptor=?" (Table.name (featureTable f)))
                  [DB.toSql (Feature.unit f), DB.toSql (Feature.descriptor f)]
    return ()

getDescriptorId :: PersistBackend m => String -> m DB.DescriptorId
getDescriptorId name = do
    x <- getBy (DB.UniqueDescriptor name)
    case x of
        Nothing -> fail $ "Descriptor " ++ name ++ " not found"
        Just (d, _) -> return d

deleteFeatureP :: PersistBackend m => DB.DescriptorId -> m ()
deleteFeatureP d = deleteWhere [DB.FeatureDescriptorEq d]

getFeatures :: PersistBackend m => [DB.DescriptorId] -> m [[DB.Feature]]
getFeatures ds = do
    fs <- liftM L.transpose $ mapM (\d -> E.run_ $ DB.select [DB.FeatureDescriptorEq d] [DB.FeatureUnitAsc] 0 0 $$ E.consume) ds
    return (map (map snd) fs)

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
                    let xs0 = map (foldl (V.++) V.empty . map Feature.value) (map snd us)
                        (encode, _) = PCA.pca 2 (H.fromRows xs0)
                        xs = map encode xs0
                        dstMin = V.fromList [0, 0]
                        dstMax = V.fromList [1, 1]
                        (srcMin, srcMax) = first fromList $ unzip $ map (\c -> (minElement c, maxElement c)) (toColumns $ fromRows xs)
                        srcScale         = recip (fromList srcMax `sub` srcMin)
                        dstScale         = dstMax `sub` dstMin
                        rescale          = dstScale `mul` srcScale
                    print (srcMin, srcMax, srcScale, dstScale, rescale)
                    case func (map snd us) of
                        [] -> return ()
                        fs -> do
                            mapM_ (\f -> do {
                                Table.insert c (Feature.descriptor f)
                              ; Table.create c (featureTable f)
                              ; deleteFeature c f
                              ; Table.insert c f })
                              fs
                            DB.commit c

transformFeatureP ::
    (PersistBackend m, MonadIO m) =>
    ([SV.Vector Double] -> [SV.Vector Double])
 -> [DB.DescriptorId]
 -> Analysis.Descriptor
 -> m ()
transformFeatureP func srcFeatures dstFeature = do
    -- fs <- liftM L.transpose $ mapM (\d -> E.run_ $ DB.select [DB.FeatureDescriptorEq d] [DB.FeatureUnitAsc] 0 0 $$ E.consume) srcFeatures
    -- liftIO $ print fs
    d <- DB.getDescriptor (Analysis.name dstFeature) (Analysis.degree dstFeature)
    -- liftIO $ print d
    deleteFeatureP d
    fs <- getFeatures srcFeatures
    -- liftIO $ print fs
    let xs = map (foldl (V.++) V.empty . map (DB.toVector . DB.featureValue)) fs
        xs' = func xs
    -- liftIO $ print xs'
    -- liftIO $ print d
    zipWithM_ (\u x -> insert (DB.Feature u d (DB.fromVector x))) (map (DB.featureUnit . head) fs) xs'
    return ()
    -- case res of
    --     Left e -> fail e
    --     Right (us, _) -> do
    --         let xs0 = map (foldl (V.++) V.empty . map Feature.value) (map snd us)
    --             (encode, _) = PCA.pca 2 (H.fromRows xs0)
    --             xs = map encode xs0
    --             dstMin = V.fromList [0, 0]
    --             dstMax = V.fromList [1, 1]
    --             (srcMin, srcMax) = first fromList $ unzip $ map (\c -> (minElement c, maxElement c)) (toColumns $ fromRows xs)
    --             srcScale         = recip (fromList srcMax `sub` srcMin)
    --             dstScale         = dstMax `sub` dstMin
    --             rescale          = dstScale `mul` srcScale
    --         print (srcMin, srcMax, srcScale, dstScale, rescale)
    --         case func (map snd us) of
    --             [] -> return ()
    --             fs -> do
    --                 mapM_ (\f -> do {
    --                     Table.insert c (Feature.descriptor f)
    --                   ; Table.create c (featureTable f)
    --                   ; deleteFeature c f
    --                   ; Table.insert c f })
    --                   fs
    --                 DB.commit c

-- | Map a list of vectors to a target range.
linearMap :: Vector Double -> Vector Double -> [Vector Double] -> [Vector Double]
linearMap dstMin dstMax xs = map (\v -> dstMin `add` ((v `sub` srcMin) `mul` rescale)) xs
    where
        (srcMin, srcMax) = first fromList $ unzip $ map (\c -> (minElement c, maxElement c)) (toColumns $ fromRows xs)
        srcScale         = recip (fromList srcMax `sub` srcMin)
        dstScale         = dstMax `sub` dstMin
        rescale          = dstScale `mul` srcScale

featurePCA :: Int -> [SV.Vector Double] -> [SV.Vector Double]
featurePCA m rows = encoded
    where
        (encode, _)  = PCA.pca m (H.fromRows rows)
        encoded      = linearMap (V.replicate m 0) (V.replicate m 1) (map encode rows)

-- | Analyse a directory recursively, writing the results to a database.
importPaths :: FilePath -> [FilePath] -> IO ()
importPaths dbFile paths = DB.handleSqlError
                           $ DB.withDatabaseP dbFile
                           $ DB.migrate >> Analysis.importPaths Nothing paths SonicAnnotator.analyser

queryP :: (MonadIO m, PersistBackend m) => String -> [String] -> m (DB.SourceFileMap, [(DB.UnitId, DB.Unit, [DB.Feature])])
queryP pattern features = do
    let regex = mkRegex pattern
    -- sfs <- liftM (Map.fromList . filter (\(_, sf) -> maybe False (const True) (matchRegex regex (DB.sourceFileUrl sf)))) (selectList [] [] 0 0)
    sfs <- selectList [] [] 0 0
    -- liftIO $ print sfs
    us <- liftM concat $ mapM (\(sf, _) -> selectList [DB.UnitSourceFileEq sf] [] 0 0) sfs
    -- liftIO $ print us
    ds <- mapM getDescriptorId features
    -- liftIO $ print ds
    fs <- liftM concat $ mapM (\d -> mapM (\(u, _) -> selectList [DB.FeatureUnitEq u, DB.FeatureDescriptorEq d] [] 0 0) us) ds
    -- liftIO $ print fs
    liftIO $ putStrLn "queryP: done"
    return (Map.fromList sfs, zipWith (\(i, u) fs -> (i, u, map snd fs)) us fs)
    -- return (Map.fromList sfs, [])

query :: FilePath -> String -> [String] -> IO (DB.SourceFileMap, [(DB.UnitId, DB.Unit, [DB.Feature])])
query dbFile pattern features = DB.withDatabaseP dbFile (DB.migrate >> queryP pattern features)

-- query dbFile features pattern = do
    -- DB.withDatabase dbFile $ \c -> do
    --     initTables features c
    --     Sql.unitQuery (DB.quickQuery' c)
    --           ((url sourceFile `like` pattern) `and` (segmentation unit `eq` seg))
    --           features

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

data Transform = PCA Int deriving (Eq, Show)

transformFeature :: FilePath -> Transform -> String -> [String] -> IO ()
-- transformFeature dbFile transform dstFeature srcFeatures = DB.handleSqlError $ transformFeature' func dbFile srcFeatures
--     where
--         func = case transform of
--                 PCA -> featurePCA dstFeature
transformFeature dbFile transform dstFeature srcFeatures = do
    DB.withDatabaseP dbFile $ do
        ds <- mapM getDescriptorId srcFeatures
        case transform of
            PCA m -> transformFeatureP (featurePCA m) ds (Analysis.Descriptor dstFeature m)
