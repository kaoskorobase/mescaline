module Mescaline.Application.Logger (
    module System.Log.Logger
  , module System.Log.Formatter
  , module System.Log.Handler.Simple
  , getComponents
) where

import           Control.Monad.Error
import           Data.Char (toUpper)
import qualified Mescaline.Application.Config as Config
import           Mescaline.Util (readMaybe)
import           System.Log.Formatter
import           System.Log.Handler.Simple
import           System.Log.Logger

instance Config.Get_C (Priority) where
    get config section option = do
        s <- Config.get config section option
        case readMaybe (map toUpper s) of
            Nothing -> throwError (Config.ParseError $ "Invalid logging priority " ++ s, "")
            Just p  -> return p

defaultLogLevel :: Priority
defaultLogLevel = NOTICE

getLogLevel :: Config.ConfigParser -> String -> String -> Priority
getLogLevel conf component variable =
    either (const defaultLogLevel)
           id
           (Config.get conf component variable)

componentMap :: [(String, String)]
componentMap =
    [ ("BufferCache"  , "logLevel")
    , ("Database"     , "logLevel")
    , ("FeatureSpace" , "logLevel")
    , ("Hugs"         , "logLevel")
    , ("Sequencer"    , "logLevel")
    , ("Synth"        , "logLevel") ]

components :: [String]
components = map fst componentMap

-- | Initialize logger priorities.
getComponents :: IO [(String, Priority)]
getComponents = do
    conf <- Config.getConfig
    return $ map (\(logger, var) -> (logger, getLogLevel conf logger var))
                 componentMap
