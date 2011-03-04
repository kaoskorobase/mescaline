module Mescaline.Application.Logger (
    module System.Log.Logger
  , module System.Log.Formatter
  , module System.Log.Handler.Simple
  , getComponents
  , initialize
) where

import           Control.Monad.Error
import           Control.Monad.Trans (MonadIO)
import           Data.Char (toUpper)
import           Mescaline.Application (AppT)
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

-- | Initialize logger priorities.
getComponents :: MonadIO m => AppT m [(String, Priority)]
getComponents = do
    conf <- Config.getConfig
    return $ map (\(logger, var) -> (logger, getLogLevel conf logger var))
                 componentMap

initialize :: MonadIO m => AppT m ()
initialize = mapM_ (\(l,p) -> liftIO $ updateGlobalLogger l (setLevel p)) =<< getComponents
