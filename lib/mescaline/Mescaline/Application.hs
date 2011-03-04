{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Mescaline.Application (
    OS(..)
  , Arch(..)
  , Platform(..)
  , buildOS
  , buildArch
  , buildPlatform
  , App
  , Version(..)
  , mkApp
  , AppT
  , runAppT
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

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Peel (MonadPeelIO)
import           Control.Monad.Reader (MonadReader, ReaderT(..), asks, runReaderT)
import           Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import           Data.List (isPrefixOf)
import           Data.Version(Version(..))
import           Distribution.System(OS(..), Arch(..), Platform(..), buildOS, buildArch, buildPlatform)
-- import qualified Paths_mescaline as Paths
import           Prelude hiding (catch)
import qualified System.Directory as Dir
import qualified System.Environment.FindBin as FindBin
import qualified System.Environment as Env
import           System.FilePath

data App = App {
    _name :: String
  , _version :: Version
  , _progDir :: FilePath
  , _binDir :: FilePath
  , _dataDir :: FilePath
  } deriving (Eq, Read, Show)

newtype AppT m a = AppT (ReaderT App m a)
    deriving (Applicative, Functor, Monad, MonadReader App, MonadIO, MonadPeelIO, MonadTrans)

mkApp :: String -> Version -> IO FilePath -> IO FilePath -> IO App
mkApp name version getBinDir getDataDir = do
    pd <- FindBin.getProgPath
    bd <- getBinDir
    dd <- getDataDir
    return $ App name version pd bd dd

runAppT :: AppT m a -> App -> m a
runAppT (AppT r) = runReaderT r

-- | The name of this application.
name :: Monad m => AppT m String
name = return "Mescaline"

-- | The version of this application.
version :: Monad m => AppT m Version
version = asks _version

-- | Cross-platform version of 'System.Environment.getArgs' that filters out
--   arguments passed by the window system.
getArgs :: MonadIO m => AppT m [String]
getArgs =
    if buildOS == OSX
        -- Get rid of the Process Serial Number that is passed to an application
        -- bundle binary.
        then liftIO $ filter (not . isPrefixOf "-psn") `fmap` Env.getArgs
        else liftIO Env.getArgs

-- | Get the containing directory of the running program.
getProgramDirectory :: MonadIO m => AppT m FilePath
getProgramDirectory = pathA _progDir

-- | Get the application's data directory.
--
-- When running the installed program (crude check), return the data directory from
-- the Paths_mescaline module. Otherwise, return @resources@ in the current
-- directory.
getDataDirectory :: MonadIO m => AppT m FilePath
getDataDirectory = do
    d1 <- getProgramDirectory >>= liftIO . Dir.canonicalizePath
    d2 <- pathA _binDir >>= liftIO . Dir.canonicalizePath
    if d1 `equalFilePath` d2
        then pathA _dataDir
        else Dir.getCurrentDirectory `combineA` "resources"

-- | Returns the directory where application resources can be found.
getResourceDirectory :: MonadIO m => AppT m FilePath
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
             else liftIO . Dir.getAppUserDataDirectory =<< name

getResourcePath :: MonadIO m => FilePath -> AppT m FilePath
getResourcePath f = liftM2 combine getResourceDirectory (return f)

-- | Get an executable path in the resource directory.
getResourceExecutable :: MonadIO m => FilePath -> AppT m (Maybe FilePath)
getResourceExecutable f = do
    p <- getResourcePath f
    e <- liftIO $ catch (Dir.executable `fmap` Dir.getPermissions p) (\(_ :: IOError) -> return False)
    return $ if e then Just p else Nothing

-- | Search an executable, first in the resource directory, then in PATH.
--
-- The executable can be a path into the resource directory, for searching PATH only
-- the filename component will be used.
findExecutable :: MonadIO m => FilePath -> AppT m (Maybe FilePath)
findExecutable path = do
    resPath <- getResourceExecutable path
    case resPath of
        Nothing -> liftIO $ Dir.findExecutable (takeFileName path)
        _       -> return resPath

-- | Get the directory where user application data is stored, such as config files.
getUserDataDirectory :: MonadIO m => AppT m FilePath
getUserDataDirectory = do
    d <- if buildOS == OSX
            then combineA Dir.getHomeDirectory . combine "Library/Application Support" =<< name
            else liftIO . Dir.getAppUserDataDirectory =<< name
    liftIO $ Dir.createDirectoryIfMissing True d
    return d

-- | Get a path in the user application data directory.
getUserDataPath :: MonadIO m => FilePath -> AppT m FilePath
getUserDataPath f = liftM2 combine getUserDataDirectory (return f)

-- {{{ Utilities

pathA :: Monad m => (App -> FilePath) -> AppT m FilePath
pathA = asks

combineA :: MonadIO m => IO FilePath -> FilePath -> AppT m FilePath
combineA a b = liftM2 combine (liftIO a) (return b)

-- }}}
