module Mescaline.Meap.Chain (
    Options(..)
  , defaultOptions
  , run
  , mapDirectory
) where

import qualified Data.ByteString as ByteString
import           Data.Char (toUpper)
import           Control.ThreadPool (threadPoolIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception (evaluate)

import qualified Mescaline.Meap.Extractor as Extractor
import qualified Mescaline.Meap.Feature as Feature
import           Mescaline.Meap.Process (withTempFile)
import qualified Mescaline.Meap.Segmenter as Segmenter

import           Sound.Analysis.Meapsoft (MEAP)
import qualified Sound.Analysis.Meapsoft as Meap

import           System.Exit (ExitCode(..))
import qualified System.FilePath.Find as Find
import           System.IO (hClose)

-- Processing chain:
-- run segmenter
-- analyse segment file (tempo statistics)
--   if beat_analysis
--     if heuristic == no prominent beat
--       reanalyse with onset_analysis
--     else if heuristic == improbable tempo
--       reanalyse with adapted tempo scale
--   else if onset_analysis
--     if heuristic == too many onsets
--       reanalyse with higher density
-- run feature extractor
-- schwurbel features into database

data Options = Options {
    segmenter :: Segmenter.Options
  , extractor :: Extractor.Options
  , useCache  :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    segmenter = Segmenter.defaultOptions
  , extractor = Extractor.defaultOptions
  , useCache  = True
}

runSegmenter :: Segmenter.Options -> FilePath -> FilePath -> IO ExitCode
runSegmenter = Segmenter.run -- TODO: heuristics

runExtractor :: Extractor.Options -> FilePath -> FilePath -> IO ExitCode
runExtractor = Extractor.run

segmenterExtension :: String
segmenterExtension = ".seg_beats"

extractorExtension :: String
extractorExtension = ".feat_beats"

-- | Run the segmenter\/extractor chain with the given options on a sound file.
--
-- The result is either an error string or the analysis result.
run :: Options -> FilePath -> IO (Either String MEAP)
run opts audioFile =
    withTempFile "es.globero.mescaline.segmenter.XXXX.seg" $
        \segFile segHandle -> do
            hClose segHandle
            withTempFile "es.globero.mescaline.extractor.XXXX.seg" $
                \featFile featHandle -> do
                    hClose featHandle
                    e <- runSegmenter (segmenter opts) audioFile segFile
                    case e of
                        ExitFailure i ->
                            return $ Left $ "Segmenter failed with exit code " ++ show i
                        ExitSuccess -> do
                            e <- runExtractor (extractor opts) segFile featFile
                            case e of
                                ExitFailure i ->
                                    return $ Left $ "Extractor failed with exit code " ++ show i
                                ExitSuccess -> do
                                    Meap.read_meap featFile

-- | List of audio file extensions Meap can handle at the moment.
audioFileExtensions :: [String]
audioFileExtensions = map ("."++) (xs ++ (map (map toUpper) xs))
    where xs = ["aif", "aiff", "mp3", "wav"]

-- | Run the segmenter and extractor chain on each sound file found in a directory.
--
-- The results are returned as a lazy list of pairs of file names and analysis results.
mapDirectory :: Int -> Options -> FilePath -> IO [(FilePath, Either String MEAP)]
mapDirectory np opts path = do
    (ichan, ochan) <- threadPoolIO np (\path -> run opts path >>= evaluate >>= return . (,) path)
    files <- Find.find
                Find.always
                (fmap (flip elem audioFileExtensions) Find.extension)
                path
    -- Push jobs to input channel
    mapM_ (Chan.writeChan ichan) files
    -- Pull results from output channel as a lazy list
    Chan.getChanContents ochan >>= return . take (length files)
