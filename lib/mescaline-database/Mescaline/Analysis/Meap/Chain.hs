module Mescaline.Analysis.Meap.Chain (
    Options(..)
  , defaultOptions
  , run
) where

import qualified Mescaline.Analysis.Meap.Extractor as Extractor
import qualified Mescaline.Analysis.Meap.Segmenter as Segmenter
import           Mescaline.Util (withTempFile)
import           Sound.Analysis.Meapsoft (MEAP)
import qualified Sound.Analysis.Meapsoft as Meap
import           System.Exit (ExitCode(..))
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
