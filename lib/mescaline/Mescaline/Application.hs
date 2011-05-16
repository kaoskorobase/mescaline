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
  -- * Configuration
  , configFile
  , defaultConfigFiles
  , config
  -- * Logging
  , updateLogger
  , logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM
) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Control (MonadControlIO)
import           Control.Monad.Reader (MonadReader, ReaderT(..), asks, runReaderT)
import           Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import           Data.ConfigFile (Get_C, ConfigParser)
import qualified Data.ConfigFile as Config
import qualified Data.List as List
import           Data.Version(Version(..))
import           Distribution.System(OS(..), Arch(..), Platform(..), buildOS, buildArch, buildPlatform)
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Application.Logger as Log
import           Prelude hiding (catch)
import qualified Prelude as P
import qualified System.Directory as Dir
import qualified System.Environment.FindBin as FindBin
import qualified System.Environment as Env
import           System.FilePath
import           System.IO (stderr)
import qualified System.Log.Handler.Simple as Log
import           System.Time (ClockTime)
import qualified System.Time as Time

data ConfigFile = ConfigFile FilePath Directory
                    deriving (Eq, Show)

data Directory =
    ResourceDirectory
  | DataDirectory
  | DirectoryPath FilePath
  deriving (Eq, Read, Show)

data App = App {
    _name :: String
  , _version :: Version
  , _progDir :: FilePath
  , _binDir :: FilePath
  , _dataDir :: FilePath
  , _configFiles :: [ConfigFile]
  , _configState :: MVar (ConfigParser, [ClockTime])
  }

newtype AppT m a = AppT (ReaderT App m a)
    deriving (Applicative, Functor, Monad, MonadReader (App), MonadIO, MonadControlIO, MonadTrans)

defaultConfigFileName :: FilePath
defaultConfigFileName = "config"

configFile :: Directory -> ConfigFile
configFile = ConfigFile defaultConfigFileName

defaultConfigFiles :: [ConfigFile]
defaultConfigFiles = [configFile ResourceDirectory, configFile DataDirectory]

mkApp :: String -> Version -> IO FilePath -> IO FilePath -> [ConfigFile] -> IO (App)
mkApp name version getBinDir getDataDir configFiles = do
    pd <- FindBin.getProgPath
    bd <- getBinDir
    dd <- getDataDir
    mv <- newMVar (Config.defaultConfig, replicate (length configFiles) defaultModificationTime)
    return $ App name version pd bd dd configFiles mv

runAppT :: MonadIO m => AppT m a -> App -> m a
runAppT (AppT r) = runReaderT (l >> r)
    where (AppT l) = initLoggers

-- | The name of this application.
name :: Monad m => AppT m String
name = asks _name

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
        then liftIO $ filter (not . List.isPrefixOf "-psn") `fmap` Env.getArgs
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

-- {{{ Configuration
configFilePath :: MonadIO m => ConfigFile -> AppT m FilePath
configFilePath (ConfigFile file ResourceDirectory) = getResourcePath file
configFilePath (ConfigFile file DataDirectory) = getUserDataPath file
configFilePath (ConfigFile file (DirectoryPath dir)) = return (dir </> file)

defaultModificationTime :: ClockTime
defaultModificationTime = Time.TOD 0 0

getModificationTime :: FilePath -> IO ClockTime
getModificationTime path = P.catch (Dir.getModificationTime path) (const (return defaultModificationTime))

getConfigParser :: (MonadIO m) => AppT m ConfigParser
getConfigParser = do
    fs <- mapM configFilePath =<< asks _configFiles
    mv <- asks _configState
    liftIO $ modifyMVar mv $ \(configParser, modTimes) -> do
        modTimes' <- mapM getModificationTime fs
        if modTimes == modTimes'
            then return ((configParser, modTimes), configParser)
            else do
                -- Apply config files to config
                configParser' <- Config.readConfigFiles Config.defaultConfig fs
                return ((configParser', modTimes'), configParser')

config :: (Get_C a, MonadIO m) => String -> String -> a -> AppT m a
config section option defaultValue = do
    cp <- getConfigParser
    case Config.get cp section option of
        Left _ -> return defaultValue
        Right a -> return a

-- config :: (Configurable c, MonadPeelIO m) => AppT m c
-- config = do
--     fs <- mapM configFilePath =<< asks _configFiles
--     c0 <- asks _initialConfig
--     mv <- asks _configState
--     liftIO $ modifyMVar mv $ \config@(Config c ts) -> do
--         ts' <- mapM getModificationTime fs
--         if ts == ts'
--             then return (config, c)
--             else do
--                 -- Apply config files to config
--                 c' <- Config.applyConfigFiles fs c0
--                 return (Config c' ts', c')
-- }}}

-- {{{ Logging

-- componentMap :: [(String, String)]
-- componentMap =
--     [ ("BufferCache"  , "logLevel")
--     , ("Database"     , "logLevel")
--     , ("FeatureSpace" , "logLevel")
--     , ("Hugs"         , "logLevel")
--     , ("Sequencer"    , "logLevel")
--     , ("Synth"        , "logLevel") ]
-- 
-- -- | Initialize logger priorities.
-- getComponents :: MonadIO m => AppT m [(String, Priority)]
-- getComponents = do
--     conf <- Config.getConfig
--     return $ map (\(logger, var) -> (logger, getLogLevel conf logger var))
--                  componentMap
-- 
-- initialize :: MonadIO m => AppT m ()
-- initialize = mapM_ (\(l,p) -> liftIO $ updateGlobalLogger l (setLevel p)) =<< getComponents

defaultLogLevel :: Log.Priority
defaultLogLevel = Log.WARNING

loggerName :: Monad m => String -> AppT m String
loggerName "" = name
loggerName component = do
    root <- name
    return $ root ++ "." ++ component

initLogComponent :: MonadIO m => String -> AppT m ()
initLogComponent component = do
    let section = case component of
                    "" -> "DEFAULT"
                    s  -> s
    priority <- config section "logLevel" defaultLogLevel
    liftIO $ print $ (component, section, priority)
    logger <- loggerName component
    liftIO $ Log.updateGlobalLogger logger (Log.setLevel priority)

logComponents :: MonadIO m => AppT m [String]
logComponents = liftM (("":) . Config.sections) getConfigParser

initLoggers :: MonadIO m => AppT m ()
initLoggers = do
    mapM_ initLogComponent =<< logComponents
    liftIO $ Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers ([] :: [Log.GenericHandler ()]))
    stderrLogger <- liftIO $ Log.streamHandler stderr Log.DEBUG
    updateLogger (Log.setHandlers ([stderrLogger])) ""

updateLogger :: MonadIO m => (Log.Logger -> Log.Logger) -> String -> AppT m ()
updateLogger f n = loggerName n >>= liftIO . flip Log.updateGlobalLogger f

logM :: MonadIO m => Log.Priority -> String -> String -> AppT m ()
logM priority component message = do
    logger <- loggerName component
    -- updateLogger logger
    liftIO $ Log.logM logger priority message

debugM :: MonadIO m => String -> String -> AppT m ()
debugM = logM Log.DEBUG

infoM :: MonadIO m => String -> String -> AppT m ()
infoM = logM Log.INFO

noticeM :: MonadIO m => String -> String -> AppT m ()
noticeM = logM Log.NOTICE

warningM :: MonadIO m => String -> String -> AppT m ()
warningM = logM Log.WARNING

errorM :: MonadIO m => String -> String -> AppT m ()
errorM = logM Log.ERROR

criticalM :: MonadIO m => String -> String -> AppT m ()
criticalM = logM Log.CRITICAL

alertM :: MonadIO m => String -> String -> AppT m ()
alertM = logM Log.ALERT

emergencyM :: MonadIO m => String -> String -> AppT m ()
emergencyM = logM Log.EMERGENCY

-- }}}

-- {{{ Utilities

pathA :: Monad m => (App -> FilePath) -> AppT m FilePath
pathA = asks

combineA :: MonadIO m => IO FilePath -> FilePath -> AppT m FilePath
combineA a b = liftM2 combine (liftIO a) (return b)

-- }}}
