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

import qualified Mescaline.Meap.Extractor as Extractor
import qualified Mescaline.Meap.Feature as Feature
import           Mescaline.Meap.Process (withTempFile)
import qualified Mescaline.Meap.Segmenter as Segmenter

import           Sound.Analysis.Meapsoft (MEAP)
import qualified Sound.Analysis.Meapsoft as Meap

import           System.Directory (doesFileExist)
import           System.Exit (ExitCode)
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

eitherToIO :: Either String a -> IO a
eitherToIO e = case e of
                Left err -> fail err
                Right x  -> return x

segmenterExtension :: String
segmenterExtension = ".seg_beats"

extractorExtension :: String
extractorExtension = ".feat_beats"

run :: Options -> FilePath -> IO Meap.MEAP
run opts audioFile =
    withTempFile
        "es.globero.mescaline.segmenter.XXXX.seg"
        (\segFile segHandle -> do
            hClose segHandle
            withTempFile
                "es.globero.mescaline.extractor.XXXX.seg"
                (\featFile featHandle -> do
                    hClose featHandle
                    runSegmenter (segmenter opts) audioFile segFile
                    -- TODO: error handling
                    runExtractor (extractor opts) segFile featFile
                    -- TODO: error handling
                    Meap.read_meap featFile >>= eitherToIO))

audioFileExtensions :: [String]
audioFileExtensions = map ("."++) (xs ++ (map (map toUpper) xs))
    where xs = ["aif", "aiff", "mp3", "wav"]

mapDirectory :: Int -> (FilePath -> MEAP -> IO ()) -> Options -> FilePath -> IO ()
mapDirectory np f opts path = do
    (ichan, ochan) <- threadPoolIO np proc
    files <- Find.find
                Find.always
                (fmap (flip elem audioFileExtensions) Find.extension)
                path
    -- Push jobs to input channel
    mapM_ (Chan.writeChan ichan) files
    -- Pull results from output channel (and discard them)
    mapM_ (\_ -> Chan.readChan ochan >>= consume) [1..length files]
    where
        proc path = run opts path >>= f path
        consume a = a `seq` return a

