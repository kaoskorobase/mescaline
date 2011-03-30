{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , ScopedTypeVariables #-}
module Mescaline.Application.Config (
    module Data.ConfigFile
  , defaultConfig
  , readConfigFiles
) where

import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.Error (throwError)
import           Data.ConfigFile
import           Data.Typeable
import           System.Directory
import           System.FilePath
import           Prelude hiding (catch)

instance Get_C a => Get_C (Maybe a) where
    get config section option =
        case get config section option of
            Left (NoSection _, _) -> return Nothing
            Left (NoOption _, _)  -> return Nothing
            Left e -> throwError e
            Right a -> return (Just a)

data ConfigParseError = ConfigParseError String
                            deriving (Eq, Show, Typeable)
instance Exception ConfigParseError

defaultConfig :: ConfigParser
defaultConfig = emptyCP { optionxform = id
                        , accessfunc = interpolatingAccess 16 }

readConfigFile :: ConfigParser -> FilePath -> IO ConfigParser
readConfigFile cp path = do
    exists <- doesFileExist path
    if exists
        then do
            result <- readfile cp path
            case result of
                Right cp' -> return cp'
                Left (e, _) -> failure (ConfigParseError (show e))
        else return cp

readConfigFiles :: ConfigParser -> [FilePath] -> IO ConfigParser
readConfigFiles = foldM readConfigFile
