{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Application (
    OS(..)
  , Arch(..)
  , Platform(..)
  , buildOS
  , buildArch
  , buildPlatform
  , name
  , getArgs
  , getResourceDirectory
  , getResourcePath
  , getResourceExecutable
  , findExecutable
  , getUserDataDirectory
  , getUserDataPath
) where

import           Control.Exception
import           Data.List (isPrefixOf)
import           Distribution.System(OS(..), Arch(..), Platform(..), buildOS, buildArch, buildPlatform)
import           Prelude hiding (catch)
import qualified System.Directory as Dir
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import qualified System.Environment as Env

-- | The name of this application.
name :: String
name = "Mescaline"

getArgs :: IO [String]
getArgs =
    if buildOS == OSX
        -- Get rid of Process Serial Number that is passed to an application bundle binary.
        then filter (not . isPrefixOf "-psn") `fmap` Env.getArgs
        else Env.getArgs

-- | Returns the directory where application resources can be found.
getResourceDirectory :: IO FilePath
getResourceDirectory = do
    p <- getProgPath
    if buildOS == OSX
        then return $ takeDirectory p </> "Resources"
        else if buildOS == Linux
             then return $ takeDirectory p </> "lib" </> name
             else getUserDataDirectory

getResourcePath :: FilePath -> IO FilePath
getResourcePath f = do
    d <- getResourceDirectory
    return $ d </> f

-- | Get an executable path in the resource directory.
getResourceExecutable :: FilePath -> IO (Maybe FilePath)
getResourceExecutable f = do
    p <- getResourcePath f
    e <- catch (Dir.executable `fmap` Dir.getPermissions p) (\(_ :: IOError) -> return False)
    return $ if e then Just p else Nothing

-- | Search an executable, first in the resource directory, then in PATH.
--
-- The executable can be a path into the resource directory, for searching PATH only the filename component will be used.
findExecutable :: FilePath -> IO (Maybe FilePath)
findExecutable path = do
    resPath <- getResourceExecutable path
    case resPath of
        Nothing -> Dir.findExecutable (takeFileName path)
        _       -> return resPath

-- getSystemDataDirectory :: IO FilePath
-- getSystemDataDirectory =
-- #if darwin_HOST_OS == 1
--     return $ "/Library/Application Support" </> name
-- #elif mingw32_HOST_OS == 1
--     getResourceDirectory
-- #else
--     getResourceDirectory
-- #endif

getUserDataDirectory :: IO FilePath
getUserDataDirectory = do
    if buildOS == OSX
        then do
            h <- Dir.getHomeDirectory
            return $ h </> "Library/Application Support" </> name
        else
            Dir.getAppUserDataDirectory name

getUserDataPath :: FilePath -> IO FilePath
getUserDataPath f = do
    d <- getUserDataDirectory
    return $ d </> f
