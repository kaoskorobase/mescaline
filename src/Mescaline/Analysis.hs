{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Analysis (
    module Mescaline.Analysis.Types
  , importPaths
  , importPathsDefault
) where

import           Control.ThreadPool (threadPoolIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import qualified Data.Foldable as Fold
import           Database.Persist.Sqlite
import qualified GHC.Conc as GHC
import qualified Mescaline.Application.Logger as Log
import           Mescaline.Analysis.Types
import qualified Mescaline.Analysis.SonicAnnotator as SonicAnnotator
import qualified Mescaline.Database as DB
import           Mescaline.Util (findFiles)

insertSourceFile :: PersistBackend m => SoundFile -> m DB.SourceFileId
insertSourceFile sf =
    insert $ DB.SourceFile
                (path sf)
                (hash sf)
                (numChannels sf)
                (sampleRate sf)
                (fromIntegral (frames sf))

insertDescriptor :: PersistBackend m => Descriptor -> m (DB.DescriptorId)
insertDescriptor d = DB.getDescriptor (name d) (degree d)

insertFeature :: PersistBackend m => DB.UnitId -> Feature -> m DB.FeatureId
insertFeature u f = do
    let d = descriptor f
    x <- getBy (DB.UniqueDescriptor (name d))
    di <- case x of
            Just (di, _) -> return di -- TODO: Check degree
            Nothing -> insertDescriptor d
    insert $ DB.Feature u di (DB.fromList (value f))

insertUnit :: PersistBackend m => DB.SourceFileId -> Unit -> [Feature] -> m DB.UnitId
insertUnit sf u fs = do
    ui <- insert $ DB.Unit sf (onset u) (duration u)
    mapM_ (insertFeature ui) fs
    return ui

-- | Insert a single file and its analysis data into the database.
--
-- Currently, concurrent database accesses don't seem to be possible.
insertAnalysis :: (PersistBackend m, MonadIO m) => Analysis -> m ()
insertAnalysis analysis = do
    case validate analysis of
        (e:_) -> fail e
        _     -> return ()
    m <- getBy (DB.UniqueSourceFile (hash (soundFile analysis)))
    when (m == Nothing) $ do
        sf <- insertSourceFile (soundFile analysis)
        liftIO $ Log.noticeM "Database" ("insertFile: " ++ show sf ++ " " ++ show (soundFile analysis))
        Fold.mapM_ insertDescriptor (descriptorMap analysis)
        mapM_ (uncurry (insertUnit sf)) (units analysis)

-- | Run the segmenter and extractor chain on a list of files.
--
-- The results are returned as a lazy list of pairs of file names and analysis results.
mapFiles :: Analyser a => Int -> a -> [FilePath] -> IO [(FilePath, Either String Analysis)]
mapFiles np a paths = do
    (ichan, ochan) <- threadPoolIO np $ \path -> do
        e <- try (a `analyse` path >>= evaluate)
        case e of
            Left (ex :: SomeException) -> return (path, Left (show ex))
            Right a -> return (path, Right a)
    -- Push jobs to input channel
    mapM_ (Chan.writeChan ichan) paths
    -- Pull results from output channel as a lazy list
    Chan.getChanContents ochan >>= return . take (length paths)

-- | Import a file or directory into the database.
importPaths :: (Analyser a) => a -> Maybe Int -> FilePath -> [FilePath] -> IO ()
importPaths a np dbFile ps = DB.withDatabase dbFile $ do
    -- files <- liftIO $ findFiles (fileExtensions a) ps
    -- FIXME: Avoid computing the source file hash twice.
    -- files' <- filterM (\p -> do { sf <- SourceFile.newLocal p ; fmap not $ Table.isStored c sf }) files
    liftIO (mapFiles (maybe GHC.numCapabilities id np) a =<< findFiles (fileExtensions a) ps)
    >>= mapM_ (\(p, e) ->
            either (\s -> liftIO $ Log.errorM "Database" (p ++ ": " ++ s))
                   insertAnalysis
                   e)

-- | Import a file or directory into the database using the default analyser.
importPathsDefault :: Maybe Int -> FilePath -> [FilePath] -> IO ()
importPathsDefault = importPaths SonicAnnotator.analyser
