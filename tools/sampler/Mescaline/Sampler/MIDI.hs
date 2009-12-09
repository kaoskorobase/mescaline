module Mescaline.Sampler.MIDI (
    module System.MIDI
  , findSources
  , findSource
) where

import Control.Exception (PatternMatchFail(..), catch, evaluate)
import Control.Monad (filterM)
import Prelude hiding (catch)
import System.MIDI

findSources :: (String -> String -> String -> Bool) -> IO [Source]
findSources f = filterM p =<< enumerateSources
    where
        p s = do
            s1 <- getManufacturer s
            s2 <- getModel s
            s3 <- getName s
            evaluate (f s1 s2 s3) `catch` (\(PatternMatchFail _) -> return False)

findSource :: (String -> String -> String -> Bool) -> IO (Maybe Source)
findSource f = do
    l <- findSources f
    case l of
        []    -> return Nothing
        (s:_) -> return (Just s)
