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
  , getUserDataDirectory
  , getUserDataPath
) where

import           Data.List (isPrefixOf)
import           Distribution.System(OS(..), Arch(..), Platform(..), buildOS, buildArch, buildPlatform)
import           System.Directory
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
-- #elif mingw32_HOST_OS == 1
--     return p
-- #else
--     return $ takeDirectory p </> "lib" </> map toLower name
        else getUserDataDirectory

getResourcePath :: FilePath -> IO FilePath
getResourcePath f = do
    d <- getResourceDirectory
    return $ d </> f

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
            h <- getHomeDirectory
            return $ h </> "Library/Application Support" </> name
        else
            getAppUserDataDirectory name

getUserDataPath :: FilePath -> IO FilePath
getUserDataPath f = do
    d <- getUserDataDirectory
    return $ d </> f
