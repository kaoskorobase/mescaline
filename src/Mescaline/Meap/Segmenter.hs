module Mescaline.Meap.Segmenter (
    Options(..),
    defaultOptions,
    run
) where

import Mescaline.Meap.Process   (runMeap, withTempFile)
import System.Exit              (ExitCode)

data UnitBoundary = Onset | Beat

data Options = Options {
    tempoScale      :: Double,
    smoothingWindow :: Double,
    unitBoundary    :: UnitBoundary,
    initialOnset    :: Bool
}

defaultOptions :: Options
defaultOptions = Options {
    tempoScale      = 1,
    smoothingWindow = 0.1,
    unitBoundary    = Onset,
    initialOnset    = False
}

run :: Options -> FilePath -> FilePath -> IO ExitCode
run opts infile outfile = runMeap "com.meapsoft.Segmenter" olist
    where
        olist = [
            "-o", outfile,
            "-t", show (tempoScale opts),
            "-s", show (smoothingWindow opts)]
            ++ (case unitBoundary opts of
                    Onset -> ["-d"]
                    Beat  -> [])
            ++ (if initialOnset opts
                    then ["-0"]
                    else [])
            ++ [infile]
