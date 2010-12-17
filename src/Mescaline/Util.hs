module Mescaline.Util (
    readMaybe
  , findFiles
) where

import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Enumerator (($$), (>>==))
import qualified Data.Enumerator as E
import           Data.Char
import           System.Directory
import           System.FilePath

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(a, _)] -> return a
                _        -> Nothing

findFilesEnum :: MonadIO m => [String] -> [FilePath] -> E.Enumerator FilePath m b
findFilesEnum exts paths = loop paths
    where
        exts' = map ("."++) exts
        fileMatch = flip elem exts' . map toLower . takeExtension
        loop paths (E.Continue k) = do
            case paths of
                [] -> E.continue k
                (p:ps) -> do
                    d <- liftIO $ doesDirectoryExist p
                    if d
                        then do
                            ps' <- liftM (filter (\p -> not (null p || head p == '.')))
                                         (liftIO $ getDirectoryContents p)
                            loop (map (p </>) ps' ++ ps) (E.Continue k)
                        else do
                            f <- liftIO $ doesFileExist p
                            if f && fileMatch p
                                then k (E.Chunks [p]) >>== loop ps
                                else loop ps (E.Continue k)
        loop _ step = E.returnI step

findFiles :: [String] -> [FilePath] -> IO [FilePath]
findFiles exts paths = E.run_ (findFilesEnum exts paths $$ E.consume)
