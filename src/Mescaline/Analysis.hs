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
import qualified Data.Set as Set
import qualified Data.Vector.Generic as V
import           Database.HDBC (IConnection)
import qualified GHC.Conc as GHC
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Data.ListReader as L
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Hash as Hash
import           Mescaline.Database.Model ()
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Sql (SqlRow)
import qualified Mescaline.Database.Table as Table
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

convSoundFile :: SoundFile -> SourceFile.SourceFile
convSoundFile sf = SourceFile.cons
                    (path sf)
                    (hash sf)
                    (numChannels sf)
                    (sampleRate sf)
                    (fromIntegral (frames sf))

convUnit :: SourceFile.SourceFile -> Unit.Segmentation -> Unit -> Unit.Unit
convUnit sf s u = Unit.cons sf s (onset u) (duration u)

convDescriptor :: Descriptor -> Feature.Descriptor
convDescriptor d = Feature.consDescriptor (name d) (degree d)

convFeature :: Unit.Unit -> Feature -> Feature.Feature
convFeature u f = Feature.cons (Unit.id u) (convDescriptor (descriptor f)) (V.fromList (value f))

insertModel :: (Table.Model a, SqlRow a, IConnection c) => c -> a -> IO a
insertModel c a = Table.insert c a >> return a

-- | Insert a single file and its analysis data into the database.
--
-- Currently, concurrent database accesses don't seem to be possible.
insertFile :: IConnection c
           => c
           -> FilePath
           -> Unit.Segmentation
           -> Analysis
           -> IO ()
insertFile conn path seg analysis = do
    print analysis
    print (validate analysis)
    let sf = convSoundFile (soundFile analysis)
    Log.noticeM "Database" ("insertFile: " ++ path ++ " " ++ show sf)
    Table.insert conn sf
    mapM_ (insertModel conn . convDescriptor) (Set.toList (descriptors analysis))
    us <- mapM (insertModel conn . convUnit sf seg) (map fst (units analysis))
    zipWithM_ (\u -> insertModel conn . convFeature u) us (concatMap snd (units analysis))
    DB.commit conn

-- | Import a file or directory into the database.
importPaths :: (Analyser a, IConnection c) => Maybe Int -> [FilePath] -> a -> c -> IO ()
importPaths np ps a c = do
    files <- findFiles (fileExtensions a) ps
    -- FIXME: Avoid computing the source file hash twice.
    files' <- filterM (\p -> do { sf <- SourceFile.newLocal p ; fmap not $ Table.isStored c sf }) files
    mapFiles (maybe GHC.numCapabilities id np) a files'
    >>= mapM_ (\(p, e) ->
            either (\s -> Log.errorM "Database" (p ++ ": " ++ s))
                   (insertFile c p seg)
                   e)
    where seg = Unit.Onset
