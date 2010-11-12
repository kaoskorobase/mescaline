module Mescaline.Util (
    readMaybe
  , findFiles
) where

import Data.Char
import System.Directory
import System.FilePath

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(a, _)] -> return a
                _        -> Nothing

findFiles :: [String] -> [FilePath] -> IO [FilePath]
findFiles exts paths = loop paths
    where
        exts' = map ("."++) exts
        fileMatch = flip elem exts' . map toLower . takeExtension
        loop [] = return []
        loop (p:ps) = do
            d <- doesDirectoryExist p
            if d
                then do
                    ps' <- fmap (filter (\p -> not (null p || head p == '.')))
                                (getDirectoryContents p)
                    loop (map (p </>) ps' ++ ps)
                else do
                    ps' <- loop ps
                    f <- doesFileExist p
                    if f && fileMatch p
                        then return (p : ps')
                        else return ps'
