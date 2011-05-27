{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Mescaline.Analysis.Types (
    SoundFile(..)
  , fileDuration
  , newSoundFile
  , Unit(..)
  , getUnit
  , Descriptor(..)
  , Feature(..)
  , getFeature
  , Analysis(..)
  , newAnalysis
  , descriptors
  , descriptorMap
  , validate
  , Analyser(..)
  , Error
) where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lex.Double as Lex
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import qualified Mescaline.Database.Hash as Hash
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Util as SF

data SoundFile = SoundFile {
    path          :: FilePath
  , hash          :: Hash.Hash
  , numChannels   :: Int
  , sampleRate    :: Double
  , frames        :: Integer
  } deriving (Eq, Show)

fileDuration :: SoundFile -> Double
fileDuration f = fromIntegral (frames f) / sampleRate f

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

data Error = ParseError String
    deriving (Show, Typeable)

instance Exception Error

next :: Monad m => String -> E.Iteratee b m b
next context = do
    m <- EL.head
    case m of
        Nothing -> E.throwError $ ParseError $ "EOF while parsing " ++ context
        Just x -> return x

getDouble :: Monad m => E.Iteratee B.ByteString m Double
getDouble = do
    s <- next "Double"
    case Lex.readDouble s of
        Nothing -> E.throwError $ ParseError $ "Error while parsing Double: `" ++ BC.unpack s ++ "'"
        Just (d, _) -> return d

getUnit :: Monad m => E.Iteratee B.ByteString m Unit
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

getFeature :: Monad m => Descriptor -> E.Iteratee B.ByteString m Feature
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
    mapM_ checkFeatures (units a)
    where
        checkFeatures (u, fs) = do
            let n = Set.size (descriptors a)
                m = length fs
            when (m /= n) (Writer.tell ["Invalid number of features: " ++ show u ++ " " ++ show fs])
            mapM_ checkFeature fs
        checkFeature f =
            let d = descriptor f
                l = length (value f)
            in unless (degree d == l)
                (Writer.tell ["Invalid feature " ++ show d ++ " -> " ++ show l])

class Analyser a where
    analyse :: a -> FilePath -> IO Analysis
    fileExtensions :: a -> [String]
