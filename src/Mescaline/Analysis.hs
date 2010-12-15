{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Analysis (
    SoundFile(..)
  , Unit(..)
  , getUnit
  , Descriptor(..)
  , Feature(..)
  , getFeature
  , Analysis(..)
  , newAnalysis
  , descriptors
  , validate
  , Analyser(..)
  , importPaths
) where

import           Control.ThreadPool (threadPoolIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lex.Double as Lex
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T
import qualified Data.Vector.Generic as V
import           Database.Persist.Sqlite
import qualified GHC.Conc as GHC
-- import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Data.ListReader as L
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.Entity as DB
import qualified Mescaline.Database.Hash as Hash
import           Mescaline.Util (findFiles)
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Util as SF

data SoundFile = SoundFile {
    path          :: FilePath
  , hash          :: Hash.Hash
  , numChannels   :: Int
  , sampleRate    :: Double
  , frames        :: Integer
  } deriving (Eq, Show)

-- | Create a new SourceFile from a locally available sound file.
newSoundFile :: FilePath -> IO SoundFile
newSoundFile path = do
    info <- SF.getInfo path
    hash <- Hash.fromFile path
    return $ SoundFile path hash
                (SF.channels info)
                (fromIntegral (SF.samplerate info))
                (fromIntegral (SF.frames info))

data Unit = Unit {
    onset        :: Double
  , duration     :: Double
  } deriving (Eq, Show)

getDouble :: L.ListReader B.ByteString Double
getDouble = do
    s <- L.head
    case Lex.readDouble s of
        Nothing -> throwError $ "Error while parsing Double: `" ++ BC.unpack s ++ "'"
        Just (d, _) -> return d
    
getUnit :: L.ListReader B.ByteString Unit
getUnit = do
    o <- getDouble
    d <- getDouble
    return (Unit o d)

data Descriptor = Descriptor {
    name :: String
  , degree :: Int
  } deriving (Eq, Ord, Show)

data Feature = Feature {
    descriptor :: Descriptor
  , value :: [Double]
  } deriving (Eq, Show)

getFeature :: Descriptor -> L.ListReader B.ByteString Feature
getFeature desc = do
    xs <- replicateM (degree desc) getDouble
    return (Feature desc xs)

data Analysis = Analysis {
    soundFile :: SoundFile
  , units     :: [(Unit, [Feature])]
  } deriving (Eq, Show)

newAnalysis :: FilePath -> [(Unit, [Feature])] -> IO Analysis
newAnalysis path units = liftM (flip Analysis units) (newSoundFile path)

descriptors :: Analysis -> Set.Set Descriptor
descriptors = Set.fromList . map descriptor . concatMap snd . units

descriptorMap :: Analysis -> Map.Map String Descriptor
descriptorMap a = Map.fromList (zip (map name ds) ds)
    where ds = Set.toList (descriptors a)

validate :: Analysis -> [String]
validate a = Writer.execWriter $ do
    mapM_ (mapM_ checkFeature . snd) (units a)
    where
        checkFeature f =
            let d = descriptor f
                l = length (value f)
            in unless (degree d == l)
                (Writer.tell ["Invalid feature " ++ show d ++ " -> " ++ show l])

class Analyser a where
    analyse :: a -> FilePath -> IO Analysis
    fileExtensions :: a -> [String]

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
        -- liftIO $ Log.noticeM "Database" ("insertFile: " ++ show sf ++ " " ++ show (soundFile analysis))
        liftIO $ putStrLn ("insertFile: " ++ show sf ++ " " ++ show (soundFile analysis))
        ds <- T.mapM insertDescriptor (descriptorMap analysis)
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
importPaths :: (Analyser a, MonadIO m, PersistBackend m) => Maybe Int -> [FilePath] -> a -> m ()
importPaths np ps a = do
    -- files <- liftIO $ findFiles (fileExtensions a) ps
    -- FIXME: Avoid computing the source file hash twice.
    -- files' <- filterM (\p -> do { sf <- SourceFile.newLocal p ; fmap not $ Table.isStored c sf }) files
    liftIO (mapFiles (maybe GHC.numCapabilities id np) a =<< findFiles (fileExtensions a) ps)
    >>= mapM_ (\(p, e) ->
            either -- (\s -> Log.errorM "Database" (p ++ ": " ++ s))
                   fail
                   insertAnalysis
                   e)
