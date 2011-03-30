module Mescaline.Analysis.Meap.Segmenter (
    Segmentation(..)
  , Options(..)
  , defaultOptions
  , run
) where

import Mescaline.Analysis.Meap.Process (ClassPath, OutputHandler, defaultOutputHandler, runMeap)
import System.Exit (ExitCode)

data Segmentation = Onset | Beat

data Options = Options {
    classPath       :: ClassPath
  , outputHandler   :: OutputHandler
  , tempoScale      :: Double
  , smoothingWindow :: Double
  , segmentation    :: Segmentation
  , initialOnset    :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    classPath       = []
  , outputHandler   = defaultOutputHandler
  , tempoScale      = 1
  , smoothingWindow = 0.1
  , segmentation    = Onset
  , initialOnset    = False
}

run :: Options -> FilePath -> FilePath -> IO ExitCode
run opts infile outfile = runMeap (classPath opts) (outputHandler opts) "com.meapsoft.Segmenter" olist
    where
        olist = [
            "-o", outfile,
            "-t", show (tempoScale opts),
            "-s", show (smoothingWindow opts)]
            ++ (case segmentation opts of
                    Onset -> ["-d"]
                    Beat  -> [])
            ++ (if initialOnset opts
                    then ["-0"]
                    else [])
            ++ [infile]
