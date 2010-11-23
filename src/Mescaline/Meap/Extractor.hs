module Mescaline.Meap.Extractor (
    Options(..)
  , defaultOptions
  , run
) where

import Mescaline.Meap.Process (OutputHandler, defaultOutputHandler, runMeap)
import System.Exit (ExitCode)

data UnitBoundary = Onset | Beat

data Options = Options {
    outputHandler :: OutputHandler
  , windowSize    :: Int
  , hopSize       :: Int
  , features      :: [String]
  }

defaultOptions :: Options
defaultOptions = Options {
    outputHandler = defaultOutputHandler
  , windowSize    = 2048
  , hopSize       = 512
  , features      = []
  }

run :: Options -> FilePath -> FilePath -> IO ExitCode
run opts infile outfile = runMeap (outputHandler opts) "com.meapsoft.FeatExtractor" olist
    where
        olist = [
            "-o", outfile,
            "-w", show (windowSize opts),
            "-h", show (hopSize opts)]
            ++ map ("-f"++) (features opts)
            ++ [infile]
