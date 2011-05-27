{-# LANGUAGE BangPatterns
           , CPP
           , FlexibleContexts
           , GeneralizedNewtypeDeriving #-}
module Mescaline.Database (
    SqlPersist
  , module Mescaline.Database.Entity
  , sourceFileDuration
  , Feature(..)
  , DescriptorMap
  , SourceFileMap
  , UnitMap
  , hashUnitId
  , withDatabase
  , descriptorMap
  , getDescriptor
  , getDescriptorId
  , insertFeature
  , deleteFeature
  , queryI
  , queryFeatures
  , query
#if USE_ANALYSIS
  , Transformation(..)
  , FeatureMap
  , Transform(..)
  , transformFeature
  , transformFeatureP
#endif
) where

import           Control.Arrow (first)
import           Control.Exception.Control as Exc
import           Control.Monad as M
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Control (MonadControlIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Ord (comparing)
import qualified Data.Vector.Generic as V
import           Database.Persist.Base (PersistValue(..))
import           Database.Persist.Sqlite
import           Database.Persist.Join (SelectOneMany(..))
import           Database.Persist.Join.Sql
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import           Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import           Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as SV
import           Mescaline.Database.Entity hiding (Feature(..))
import qualified Mescaline.Database.Entity as Entity
#if USE_ANALYSIS
import qualified Mescaline.Analysis.Types as Analysis
import qualified Mescaline.Statistics.PCA as PCA
import           Numeric.LinearAlgebra as H
#endif
import           Text.Regex
import           Prelude hiding (and)

-- | File duration in seconds.
sourceFileDuration :: SourceFile -> Double
sourceFileDuration sf = fromIntegral (sourceFileFrames sf) / sourceFileSampleRate sf

data Feature = Feature {
    featureUnit :: UnitId
  , featureDescriptor :: DescriptorId
  , featureValue :: SV.Vector Double
  } deriving (Show)

type DescriptorMap = Map.Map DescriptorId Descriptor
type SourceFileMap = Map.Map SourceFileId SourceFile
type UnitMap       = Map.Map UnitId (Unit, [Feature])

hashUnitId :: UnitId -> Int64
hashUnitId u = let PersistInt64 i = toPersistValue u in i

withDatabase :: MonadControlIO m => FilePath -> SqlPersist m a -> m a
withDatabase path action = withSqliteConn (Text.pack path) (runSqlConn (runMigration Entity.migrateAll >> action))

entityMapI :: (Monad m, Ord (Key v), PersistEntity v) => E.Iteratee (Key v, v) m (Map.Map (Key v) v)
entityMapI = go Map.empty
    where
        go xs = do
            m <- EL.head
            case m of
                Nothing -> return xs
                Just (k, v) -> go (Map.insert k v xs)

descriptorMap :: PersistBackend m => m DescriptorMap
descriptorMap = E.run_ $ selectEnum [] [] 0 0 $$ entityMapI

getDescriptor :: PersistBackend m => String -> Int -> m (DescriptorId, Descriptor)
getDescriptor name degree = do
    x <- getBy (UniqueDescriptor name)
    case x of
        Nothing -> do
            let d = Descriptor name degree
            di <- insert d
            return (di, d)
        Just d -> return d

getDescriptorId :: PersistBackend m => String -> m DescriptorId
getDescriptorId name = do
    x <- getBy (UniqueDescriptor name)
    case x of
        Nothing -> fail $ "Descriptor " ++ name ++ " not found"
        Just (d, _) -> return d

insertFeature :: PersistBackend m => Feature -> m FeatureId
insertFeature f = do
    fi <- insert $ Entity.Feature (featureUnit f) (featureDescriptor f)
    let v = featureValue f
    zipWithM_ (\i -> insert . Value fi i) [0..V.length v - 1] (V.toList v)
    return fi

deleteFeature :: PersistBackend m => DescriptorId -> m ()
deleteFeature d = deleteWhere [FeatureDescriptorEq d]

featureFromEntity :: Entity.Feature -> [(ValueId, Value)] -> Feature
featureFromEntity f = Feature (Entity.featureUnit f) (Entity.featureDescriptor f)
                    . V.fromList
                    . map valueValue
                    . List.sortBy (comparing valueIndex)
                    . map snd

featuresI :: PersistBackend m => E.Iteratee (Entity.FeatureId, Entity.Feature) m (Map.Map DescriptorId Feature)
featuresI = go Map.empty
    where
        go fs = do
            m <- EL.head
            case m of
                Nothing -> return fs
                Just (fi, f) -> do
                    v <- lift $ selectList [ValueFeatureEq fi] [ValueIndexAsc] 0 0
                    let f' = featureFromEntity f v
                    go (Map.insert (featureDescriptor f') f' fs)

unitsI :: (MonadControlIO m) => [DescriptorId] -> Map.Map UnitId (Unit, [Feature]) -> E.Iteratee (UnitId, Unit) (SqlPersist m) UnitMap
unitsI features = go
    where
        go us = do
            m <- EL.head
            case m of
                Nothing -> return us
                Just (ui, u) -> do
                    -- fm <- lift (E.run_ $ selectEnum [FeatureUnitEq ui] [] 0 0 $$ featuresI)
                    r <- lift $ runJoin $ SelectOneMany [FeatureUnitEq ui, FeatureDescriptorIn features]
                                                        [] []
                                                        [ValueIndexAsc] ValueFeatureIn -- TODO: Ordering might not be correct here!
                                                        valueFeature False
                    let fs = map (\((_, f), vs) -> featureFromEntity f vs) r
                    -- TODO: Properly check if feature is present!
                    -- let fs = [ x | Just x <- map (flip Map.lookup fm) features ]
                    go (Map.insert ui (u, fs) us)

type Pattern = String

matchPattern :: MonadControlIO m => Maybe Pattern -> String -> m Bool
matchPattern Nothing _ = return True
matchPattern (Just pattern) s = do
    r <- Exc.catch (evaluate (matchRegex (mkRegex pattern) s)) handler
    return $ isJust r
    where
        handler :: MonadControlIO m => SomeException -> m (Maybe a)
        handler = const (return Nothing)

queryI :: (MonadControlIO m) => ((SourceFileId, SourceFile) -> Bool) -> [DescriptorId] -> E.Iteratee (SourceFileId, SourceFile) (SqlPersist m) (SourceFileMap, UnitMap)
queryI matchFile features = go Map.empty Map.empty
    where
        minMaxUnitId :: (Int64, Int64) -> UnitId -> (Int64, Int64)
        minMaxUnitId (!curMin, !curMax) (UnitId (PersistInt64 cur)) =
            if cur < curMin
                then if cur > curMax
                     then (cur, cur)
                     else (cur, curMax)
                else if (cur > curMax)
                     then (curMin, cur)
                     else (curMin, curMax)
        minMaxUnitId _ _ = error $ "minMaxUnitId: Invalid type for UnitId"
        findMinMaxUnitId :: [(UnitId, Unit)] -> (UnitId, UnitId)
        findMinMaxUnitId us = let (uiMin, uiMax) = List.foldl' minMaxUnitId (maxBound :: Int64, minBound :: Int64) (map fst us)
                              in (UnitId (PersistInt64 uiMin), UnitId (PersistInt64 uiMax))
        mkFeatureMap :: [((FeatureId, Entity.Feature), [(ValueId, Value)])] -> Map.Map UnitId (Map.Map DescriptorId Feature)
        mkFeatureMap fs = List.foldl' (\m ((_, e), vs) -> let f = featureFromEntity e vs
                                                          in Map.insertWith Map.union
                                                                            (featureUnit f)
                                                                            (Map.singleton (featureDescriptor f) f)
                                                                            m)
                                      Map.empty
                                      fs
        lookupAll :: Ord k => [k] -> Map.Map k v -> [v]
        lookupAll [] _ = []
        lookupAll (k:ks) m = let v = m Map.! k in v `seq` v : lookupAll ks m
        updateUnitMap :: [(UnitId, Unit)] -> Map.Map UnitId (Map.Map DescriptorId Feature) -> UnitMap -> UnitMap
        updateUnitMap us fs um = List.foldl' (\m (ui, u) -> Map.insert ui (u, (lookupAll features (fs Map.! ui))) m) um us
        go sfs um = do
            m <- EL.head
            case m of
                Nothing -> return (sfs, um)
                Just (sfi, sf) -> do
                    if curry matchFile sfi sf
                        then do
                            let sfs' = Map.insert sfi sf sfs
                            us <- lift $ selectList [UnitSourceFileEq sfi] [] 0 0
                            let (uiMin, uiMax) = findMinMaxUnitId us
                                select = SelectOneMany [ FeatureUnitGe uiMin
                                                       , FeatureUnitLe uiMax
                                                       , FeatureDescriptorIn features ]
                                                       [] [] [{- FIXME: ValueIndexAsc produces only the first vector value! -}]
                                                       ValueFeatureIn valueFeature False
                            fs <- liftM mkFeatureMap $ lift $ runJoin select
                            go sfs' (updateUnitMap us fs um)
                        else go sfs um

queryFeatures :: (MonadControlIO m) => ((SourceFileId, SourceFile) -> Bool) -> [DescriptorId] -> SqlPersist m (SourceFileMap, UnitMap)
queryFeatures matchFile ds = E.run_ $ selectEnum [] [] 0 0 $$ queryI matchFile ds

query :: (MonadControlIO m) => Pattern -> [String] -> SqlPersist m (SourceFileMap, UnitMap)
query pattern features = do
    -- liftIO $ putStrLn "query: begin"
    ds <- mapM getDescriptorId features
    regex <- evaluate $ mkRegex pattern
    r <- queryFeatures (isJust . matchRegex regex . sourceFileUrl . snd) ds
    -- liftIO $ putStrLn "query: done"
    return r

#if USE_ANALYSIS
data Transform = PCA Int deriving (Eq, Show)

transformFeature :: (MonadControlIO m) => Transform -> String -> [String] -> SqlPersist m ()
transformFeature transform dstFeature srcFeatures = do
    ds <- mapM getDescriptorId srcFeatures
    case transform of
        PCA m -> transformFeatureP (Transformation (featurePCA m)) ds (Analysis.Descriptor dstFeature m)

newtype FeatureMap a = FeatureMap (Map.Map UnitId a) deriving (Foldable, Functor)

newtype Transformation = Transformation (FeatureMap (SV.Vector Double) -> FeatureMap (SV.Vector Double))

transformFeatureP ::
    (MonadControlIO m) =>
    Transformation
 -> [DescriptorId]
 -> Analysis.Descriptor
 -> SqlPersist m ()
transformFeatureP (Transformation func) srcFeatures dstFeature = do
    -- fs <- liftM L.transpose $ mapM (\d -> E.run_ $ DB.select [DB.FeatureDescriptorEq d] [DB.FeatureUnitAsc] 0 0 $$ E.consume) srcFeatures
    -- liftIO $ print fs
    (d, _) <- getDescriptor (Analysis.name dstFeature) (Analysis.degree dstFeature)
    -- liftIO $ print d
    deleteFeature d
    (_, fs) <- queryFeatures (const True) srcFeatures
    -- liftIO $ print (map (map (V.length.Vector.toVector.featureValue)) fs)
    let FeatureMap fs' = func (FeatureMap (fmap (V.concat . fmap featureValue . snd) fs))
    -- liftIO $ print xs'
    -- liftIO $ print d
    -- zipWithM_ (insertFeature d) (map (featureUnit . head) fs) xs'
    Fold.sequence_ $ Map.mapWithKey (\u -> insertFeature . Feature u d) fs'

-- | Map a list of vectors to a target range.
linearMap :: (Functor f, Foldable f) => H.Vector Double -> H.Vector Double -> f (H.Vector Double) -> f (H.Vector Double)
linearMap dstMin dstMax xs = fmap (\v -> dstMin `add` ((v `sub` srcMin) `mul` rescale)) xs
    where
        (srcMin, srcMax) = first H.fromList . unzip . map (\c -> (minElement c, maxElement c)) $ toColumns . fromRows . Fold.toList $ xs
        srcScale         = recip (H.fromList srcMax `sub` srcMin)
        dstScale         = dstMax `sub` dstMin
        rescale          = dstScale `mul` srcScale

featurePCA :: (Functor f, Foldable f) => Int -> f (H.Vector Double) -> f (H.Vector Double)
featurePCA m rows = encoded
    where
        (encode, _)  = PCA.pca m (H.fromRows (Fold.toList rows))
        encoded      = linearMap (V.replicate m 0) (V.replicate m 1) (fmap encode rows)
#endif -- USE_ANALYSIS
