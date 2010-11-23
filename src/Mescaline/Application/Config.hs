{-# LANGUAGE DeriveDataTypeable
           , ScopedTypeVariables #-}
module Mescaline.Application.Config (
    module Data.ConfigFile
  , ConfigParserError(..)
  , getIO
  , getIODefault
  , getColor
  , getConfig
) where

import           Control.Exception
import           Control.Monad
import           Data.ConfigFile
import           Data.Typeable
import           Mescaline.Application
import           Mescaline.Util (readMaybe)
import qualified Qtc.Classes.Qccs as Qt
import qualified Qtc.ClassTypes.Gui as Qt
import qualified Qtc.Gui.QColor as Qt
import           System.Directory
import           System.FilePath
import           Text.Regex
import           Prelude hiding (catch)

data ConfigParserError = ConfigParserError { cpError :: CPError } deriving (Show, Typeable)

instance Exception ConfigParserError

hexAlphaRegex :: Regex
hexAlphaRegex = mkRegex "^(#[A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9])([A-Ba-b0-9][A-Ba-b0-9])$"

alphaRegex :: Regex
alphaRegex = mkRegex "^([^*]+)\\*([0-9]+(\\.[0-9]+)?)$"

getIO :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> IO a
getIO config section option = do
    case get config section option of
        Left e  -> throw (ConfigParserError e)
        Right a -> return a

getIODefault :: Get_C a => a -> ConfigParser -> SectionSpec -> OptionSpec -> IO a
getIODefault a0 config section option = catch (getIO config section option)
                                              (\(_ :: ConfigParserError) -> return a0)

getColor :: ConfigParser -> SectionSpec -> OptionSpec -> IO (Qt.QColor ())
getColor config section option = do
    colorSpec <- getIO config section option
    case matchRegex hexAlphaRegex colorSpec of
        Just (rgb:alpha:_) -> do
            color <- Qt.qColor rgb
            case readMaybe ("0x" ++ alpha) of
                Nothing         -> return ()
                Just (a :: Int) -> Qt.setAlpha color a
            return color
        _ ->
            case matchRegex alphaRegex colorSpec of
                Just (name:alpha:_) -> do
                    color <- Qt.qColor name
                    case readMaybe alpha of
                        Nothing -> return ()
                        Just a  -> Qt.setAlphaF color a
                    return color
                _ -> Qt.qColor colorSpec

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
                Left e    -> throw (ConfigParserError e)
        else return cp

getConfig :: IO ConfigParser
getConfig = do
    defaultFile <- getResourcePath "config"
    userFile    <- getUserDataPath "config"
    foldM readConfigFile defaultConfig [defaultFile, userFile]
