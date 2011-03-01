{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Application (
    OS(..)
  , Arch(..)
  , Platform(..)
  , buildOS
  , buildArch
  , buildPlatform
  , name
  , version
  , getArgs
  , getProgramDirectory
  , getDataDirectory
  , getResourceDirectory
  , getResourcePath
  , getResourceExecutable
  , findExecutable
  , getUserDataDirectory
  , getUserDataPath
) where

import           Control.Exception
import           Data.List (isPrefixOf)
import           Data.Version(Version(..))
import           Distribution.System(OS(..), Arch(..), Platform(..), buildOS, buildArch, buildPlatform)
import qualified Paths_mescaline as Paths
import           Prelude hiding (catch)
import qualified System.Directory as Dir
import qualified System.Environment.FindBin as FindBin
import           System.FilePath
import qualified System.Environment as Env

-- | The name of this application.
name :: String
name = "Mescaline"

-- | The version of this application.
version :: Version
version = Paths.version

-- | Cross-platform version of 'System.Environment.getArgs' that filters out
--   arguments passed by the window system.
getArgs :: IO [String]
getArgs =
    if buildOS == OSX
        -- Get rid of the Process Serial Number that is passed to an application
        -- bundle binary.
        then filter (not . isPrefixOf "-psn") `fmap` Env.getArgs
        else Env.getArgs

-- | Get the containing directory of the running program.
getProgramDirectory :: IO FilePath
getProgramDirectory = FindBin.getProgPath

-- | Get the application's data directory.
--
-- When running the installed program (crude check), return the data directory from
-- the Paths_mescaline module. Otherwise, return @resources@ in the current
-- directory.
getDataDirectory :: IO FilePath
getDataDirectory = do
    progDir <- getProgramDirectory >>= Dir.canonicalizePath
    binDir  <- Paths.getBinDir >>= Dir.canonicalizePath
    if progDir `equalFilePath` binDir
        then Paths.getDataDir
        else do
            dir <- Dir.getCurrentDirectory
            return $ dir </> "resources"

-- | Returns the directory where application resources can be found.
getResourceDirectory :: IO FilePath
getResourceDirectory = do
    if buildOS == OSX
        then do
            -- When running from the .app bundle, return the bundle's resource
            -- directory, otherwise the default.
            progDir <- getProgramDirectory
            if takeFileName progDir == "MacOS"
                then return $ takeDirectory progDir </> "Resources"
                else getDataDirectory
        else if buildOS == Linux
             then getDataDirectory
             else Dir.getAppUserDataDirectory name

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
-- The executable can be a path into the resource directory, for searching PATH only
-- the filename component will be used.
findExecutable :: FilePath -> IO (Maybe FilePath)
findExecutable path = do
    resPath <- getResourceExecutable path
    case resPath of
        Nothing -> Dir.findExecutable (takeFileName path)
        _       -> return resPath

-- | Get the directory where user application data is stored, such as config files.
getUserDataDirectory :: IO FilePath
getUserDataDirectory = do
    d <- if buildOS == OSX
            then do
                h <- Dir.getHomeDirectory
                return $ h </> "Library/Application Support" </> name
            else
                Dir.getAppUserDataDirectory name
    Dir.createDirectoryIfMissing True d
    return d

-- | Get a path in the user application data directory.
getUserDataPath :: FilePath -> IO FilePath
getUserDataPath f = do
    d <- getUserDataDirectory
    return $ d </> f
