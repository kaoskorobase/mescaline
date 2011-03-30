module Mescaline.Analysis.Meap.Extractor (
    Options(..)
  , defaultOptions
  , run
) where

import Mescaline.Analysis.Meap.Process (ClassPath, OutputHandler, defaultOutputHandler, runMeap)
import System.Exit (ExitCode)

data Options = Options {
    classPath     :: ClassPath
  , outputHandler :: OutputHandler
  , windowSize    :: Int
  , hopSize       :: Int
  , features      :: [String]
  }

defaultOptions :: Options
defaultOptions = Options {
    classPath     = []
  , outputHandler = defaultOutputHandler
  , windowSize    = 2048
  , hopSize       = 512
  , features      = []
  }

run :: Options -> FilePath -> FilePath -> IO ExitCode
run opts infile outfile = runMeap (classPath opts) (outputHandler opts) "com.meapsoft.FeatExtractor" olist
    where
        olist = [
            "-o", outfile,
            "-w", show (windowSize opts),
            "-h", show (hopSize opts)]
            ++ map ("-f"++) (features opts)
            ++ [infile]
