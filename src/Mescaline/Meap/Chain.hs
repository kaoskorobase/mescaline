module Mescaline.Meap.Chain (
    Options(..)
  , defaultOptions
  , run
  , mapDirectory
) where

import qualified Data.ByteString                    as ByteString
import Data.Char                                    (toUpper)
import qualified Mescaline.Control.Concurrent.Pool  as Pool
import Control.ThreadPool                           (threadPoolIO)
import qualified Control.Concurrent.Chan            as Chan
import qualified Mescaline.Meap.Extractor           as Extractor
import qualified Mescaline.Meap.Feature             as Feature
import qualified Mescaline.Meap.File                as File
import Mescaline.Meap.Process                       (withTempFile)
import qualified Mescaline.Meap.Segmenter           as Segmenter
import System.Exit                                  (ExitCode)
import qualified System.FilePath.Find               as Find
import System.IO                                    (hClose)

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
    segmenter :: Segmenter.Options,
    extractor :: Extractor.Options
}

defaultOptions :: Options
defaultOptions = Options Segmenter.defaultOptions Extractor.defaultOptions

runSegmenter :: Segmenter.Options -> FilePath -> FilePath -> IO ExitCode
runSegmenter = Segmenter.run -- TODO: heuristics

runExtractor :: Extractor.Options -> FilePath -> FilePath -> IO ExitCode
runExtractor = Extractor.run

run :: Options -> FilePath -> IO (Maybe File.Content)
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
                    ByteString.readFile featFile >>= return . File.parse))

audioFileExtensions :: [String]
audioFileExtensions = map ("."++) (xs ++ (map (map toUpper) xs))
    where xs = ["aif", "aiff", "mp3", "wav"]

mapDirectoryParOld :: (FilePath -> File.Content -> IO ()) -> Int -> Options -> FilePath -> IO ()
mapDirectoryParOld f np opts path = do
    pool <- Pool.new np
    files <- Find.find
                Find.always
                (fmap (flip elem audioFileExtensions) Find.extension)
                path
    mapM_ (Pool.schedule pool . proc) files
    Pool.close pool
    where
        proc path = run opts path >>= maybe (return ()) (f path)

mapDirectoryPar :: Int -> (FilePath -> File.Content -> IO ()) -> Options -> FilePath -> IO ()
mapDirectoryPar np f opts path = do
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
        proc path = run opts path >>= maybe (return ()) (f path)
        consume a = a `seq` return a

mapDirectorySeq :: (FilePath -> File.Content -> IO ()) -> Options -> FilePath -> IO ()
mapDirectorySeq f opts path = do
    files <- Find.find
                Find.always
                (fmap (flip elem audioFileExtensions) Find.extension)
                path
    mapM_ proc files
    where
        proc path = run opts path >>= maybe (return ()) (f path)

mapDirectory :: Int -> (FilePath -> File.Content -> IO ()) -> Options -> FilePath -> IO ()
mapDirectory np | np <= 1 = mapDirectorySeq
mapDirectory np           = mapDirectoryPar np