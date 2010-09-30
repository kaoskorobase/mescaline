{-# LANGUAGE CPP #-}
module Mescaline.Application (
    name
  , getArgs
  , getResourceDirectory
  , getResourcePath
  , getUserDataDirectory
  , getUserDataPath
) where

import           Data.List (isPrefixOf)
import           System.Directory
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import qualified System.Environment as Env

-- | The name of this application.
name :: String
name = "Mescaline"

getArgs :: IO [String]
getArgs =
#if darwin_HOST_OS == 1
    -- Get rid of Process Serial Number that is passed to an application bundle binary.
    filter (not . isPrefixOf "-psn") `fmap` Env.getArgs
#else
    Env.getArgs
#endif

-- | Returns the directory where application resources can be found.
getResourceDirectory :: IO FilePath
getResourceDirectory = do
    p <- getProgPath
#if darwin_HOST_OS == 1
    return $ takeDirectory p </> "Resources"
-- #elif mingw32_HOST_OS == 1
--     return p
-- #else
--     return $ takeDirectory p </> "lib" </> map toLower name
#else
    getUserDataDirectory
#endif

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
#if darwin_HOST_OS == 1
    h <- getHomeDirectory
    return $ h </> "Library/Application Support" </> name
#elif mingw32_HOST_OS == 1
    getAppUserDataDirectory name
#else
    getAppUserDataDirectory $ map toLower name
#endif

getUserDataPath :: FilePath -> IO FilePath
getUserDataPath f = do
    d <- getUserDataDirectory
    return $ d </> f
