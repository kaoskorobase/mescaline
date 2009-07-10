module Mescaline.Meap.Extractor (
    Options(..),
    defaultOptions,
    run
) where

import Data.List                (intersperse)
import Mescaline.Meap.Feature   (Type)
import Mescaline.Meap.Process   (runMeap, withTempFile)
import System.Exit              (ExitCode)

data UnitBoundary = Onset | Beat

data Options = Options {
    windowSize  :: Int,
    hopSize     :: Int,
    features    :: [Type]
}

defaultOptions :: Options
defaultOptions = Options {
    windowSize  = 2048,
    hopSize     = 512,
    features    = []
}

run :: Options -> FilePath -> FilePath -> IO ExitCode
run opts infile outfile = runMeap "com.meapsoft.FeatExtractor" olist
    where
        olist = [
            "-o", outfile,
            "-w", show (windowSize opts),
            "-h", show (hopSize opts)]
            ++ map (("-f"++).show) (features opts)
            ++ [infile]
