{-# LANGUAGE CPP, FlexibleContexts #-}
module Mescaline.Database (
    module Mescaline.Database.Entity
  , module Mescaline.Database.Vector
  , withDatabase
  , descriptorMap
  , getDescriptor
  , getDescriptorId
  , deleteFeature
  , query
#if USE_ANALYSIS
  , Transform(..)
  , transformFeature
#endif
) where

import           Control.Arrow (first)
import           Control.Monad as M
import           Control.Monad.Trans (MonadIO, lift, liftIO)
import qualified Data.Vector.Generic as V
import           Database.Persist.Sqlite
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Vector.Storable as SV
-- import qualified Mescaline.Database as DB
import           Mescaline.Database.Entity
import           Mescaline.Database.Entity as Entity
import           Mescaline.Database.Vector
import           Mescaline.Database.Vector as Vector
import           Database.Persist.Sqlite as DB
#if USE_ANALYSIS
import qualified Mescaline.Analysis.Types as Analysis
import qualified Mescaline.Statistics.PCA as PCA
import           Numeric.LinearAlgebra as H
#endif
import           System.IO
import           Text.Regex
import           Prelude hiding (and)

withDatabase :: String -> SqlPersist IO a -> IO a
withDatabase path action = withSqliteConn path (runSqlConn (runMigration Entity.migrateAll >> action))

entityMapI :: (Monad m, Ord (Key v), PersistEntity v) => E.Iteratee (Key v, v) m (Map.Map (Key v) v)
entityMapI = go Map.empty
    where
        go xs = do
            m <- E.head
            case m of
                Nothing -> return xs
                Just (k, v) -> go (Map.insert k v xs)

descriptorMap :: PersistBackend m => m (Map.Map DescriptorId Descriptor)
descriptorMap = E.run_ $ select [] [] 0 0 $$ entityMapI

getDescriptor :: PersistBackend m => String -> Int -> m DescriptorId
getDescriptor name degree = do
    x <- getBy (UniqueDescriptor name)
    case x of
        Nothing -> insert (Descriptor name degree)
        Just (d, _) -> return d

getDescriptorId :: PersistBackend m => String -> m DescriptorId
getDescriptorId name = do
    x <- getBy (UniqueDescriptor name)
    case x of
        Nothing -> fail $ "Descriptor " ++ name ++ " not found"
        Just (d, _) -> return d

deleteFeature :: PersistBackend m => DescriptorId -> m ()
deleteFeature d = deleteWhere [FeatureDescriptorEq d]

getFeatures :: PersistBackend m => [DescriptorId] -> m [[Feature]]
getFeatures ds = do
    fs <- liftM L.transpose $ mapM (\d -> DB.selectList [FeatureDescriptorEq d] [FeatureUnitAsc] 0 0) ds
    return (map (map snd) fs)

featuresI :: Monad m => E.Iteratee (FeatureId, Feature) m (Map.Map DescriptorId Feature)
featuresI = go Map.empty
    where
        go fs = do
            m <- E.head
            case m of
                Nothing -> return fs
                Just (_, f) -> go (Map.insert (featureDescriptor f) f fs)

unitsI :: PersistBackend m => [DescriptorId] -> Map.Map UnitId (Unit, [Feature]) -> E.Iteratee (UnitId, Unit) m (Map.Map UnitId (Unit, [Feature]))
unitsI features = go
    where
        go us = do
            m <- E.head
            case m of
                Nothing -> return us
                Just (ui, u) -> do
                    fm <- lift (E.run_ $ select [FeatureUnitEq ui] [] 0 0 $$ featuresI)
                    let fs = [ x | Just x <- map (flip Map.lookup fm) features ]
                    go (Map.insert ui (u, fs) us)

sourceFileI :: PersistBackend m => String -> [DescriptorId] -> E.Iteratee (SourceFileId, SourceFile) m (SourceFileMap, Map.Map UnitId (Unit, [Feature]))
sourceFileI pattern features = go Map.empty Map.empty
    where
        regex = mkRegex pattern
        isMatch = maybe False (const True) . matchRegex regex . sourceFileUrl
        go sfs us = do
            m <- E.head
            case m of
                Nothing -> return (sfs, us)
                Just (sfi, sf) ->
                    if isMatch sf
                        then do
                            let sfs' = Map.insert sfi sf sfs
                            us' <- lift (E.run_ $ select [UnitSourceFileEq sfi] [] 0 0 $$ unitsI features us)
                            go sfs' us'
                        else go sfs us

query :: (MonadIO m, PersistBackend m) => String -> [String] -> m (SourceFileMap, Map.Map UnitId (Unit, [Feature]))
query pattern features = do
    -- liftIO $ putStrLn "query: begin"
    ds <- mapM getDescriptorId features
    r <- E.run_ $ select [] [] 0 0 $$ sourceFileI pattern ds
    -- liftIO $ putStrLn "query: done"
    return r

-- queryP :: (MonadIO m, PersistBackend m) => String -> [String] -> m (SourceFileMap, [(UnitId, Unit, [Feature])])
-- queryP pattern features = do
--     liftIO $ putStrLn "queryP: begin"
--     let regex = mkRegex pattern
--     -- sfs <- liftM (Map.fromList . filter (\(_, sf) -> maybe False (const True) (matchRegex regex (DB.sourceFileUrl sf)))) (selectList [] [] 0 0)
--     sfs <- selectList [] [] 0 0
--     liftIO $ putStrLn "queryP: selected soundfiles"
--     -- liftIO $ print sfs
--     us <- liftM concat $ mapM (\(sf, _) -> selectList [UnitSourceFileEq sf] [] 0 0) sfs
--     liftIO $ putStrLn "queryP: selected units"
--     -- liftIO $ print us
--     ds <- mapM getDescriptorId features
--     -- liftIO $ print ds
--     -- fs <- liftM concat $ mapM (\d -> mapM (\(ui, u) -> selectList [FeatureUnitEq ui, FeatureDescriptorEq d] [] 0 0) us) ds
--     fs <- mapM (\(ui, u) -> do { fs <- selectList [FeatureUnitEq ui] [] 0 0 ; return (ui, u, getFeatures ds (map snd fs)) }) us
--     -- liftIO $ print fs
--     liftIO $ putStrLn "queryP: done"
--     -- return (Map.fromList sfs, zipWith (\(i, u) fs -> (i, u, map snd fs)) us fs)
--     return (Map.fromList sfs, fs)
--     where
--         getFeatures ds fs = map (\d -> let Just f = L.find (\f -> featureDescriptor f == d) fs in f) ds

-- query :: FilePath -> String -> [String] -> IO (SourceFileMap, [(UnitId, Unit, [Feature])])
-- query dbFile pattern features = withDatabase dbFile (queryP pattern features)

#if USE_ANALYSIS
data Transform = PCA Int deriving (Eq, Show)

transformFeature :: (MonadIO m, PersistBackend m) => Transform -> String -> [String] -> m ()
transformFeature transform dstFeature srcFeatures = do
    ds <- mapM getDescriptorId srcFeatures
    case transform of
        PCA m -> transformFeatureP (featurePCA m) ds (Analysis.Descriptor dstFeature m)

transformFeatureP ::
    (PersistBackend m, MonadIO m) =>
    ([SV.Vector Double] -> [SV.Vector Double])
 -> [DescriptorId]
 -> Analysis.Descriptor
 -> m ()
transformFeatureP func srcFeatures dstFeature = do
    -- fs <- liftM L.transpose $ mapM (\d -> E.run_ $ DB.select [DB.FeatureDescriptorEq d] [DB.FeatureUnitAsc] 0 0 $$ E.consume) srcFeatures
    -- liftIO $ print fs
    d <- getDescriptor (Analysis.name dstFeature) (Analysis.degree dstFeature)
    -- liftIO $ print d
    deleteFeature d
    fs <- getFeatures srcFeatures
    -- liftIO $ print (map (map (V.length.Vector.toVector.featureValue)) fs)
    let xs = map (foldl (V.++) V.empty . map (Vector.toVector . featureValue)) fs
        xs' = func xs
    -- liftIO $ print xs'
    -- liftIO $ print d
    zipWithM_ (\u x -> insert (Feature u d (Vector.fromVector x))) (map (featureUnit . head) fs) xs'
    return ()

-- | Map a list of vectors to a target range.
linearMap :: H.Vector Double -> H.Vector Double -> [H.Vector Double] -> [H.Vector Double]
linearMap dstMin dstMax xs = map (\v -> dstMin `add` ((v `sub` srcMin) `mul` rescale)) xs
    where
        (srcMin, srcMax) = first H.fromList $ unzip $ map (\c -> (minElement c, maxElement c)) (toColumns $ fromRows xs)
        srcScale         = recip (H.fromList srcMax `sub` srcMin)
        dstScale         = dstMax `sub` dstMin
        rescale          = dstScale `mul` srcScale

featurePCA :: Int -> [H.Vector Double] -> [H.Vector Double]
featurePCA m rows = encoded
    where
        (encode, _)  = PCA.pca m (H.fromRows rows)
        encoded      = linearMap (V.replicate m 0) (V.replicate m 1) (map encode rows)
#endif -- USE_ANALYSIS