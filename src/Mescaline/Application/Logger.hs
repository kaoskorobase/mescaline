module Mescaline.Application.Logger (
    module System.Log.Logger
  , initialize
) where

import           Control.Monad.Error
import qualified Mescaline.Application.Config as Config
import           Mescaline.Util (readMaybe)
import           System.Log.Logger

instance Config.Get_C (Priority) where
    get config section option = do
        s <- Config.get config section option
        case readMaybe s of
            Nothing -> throwError (Config.ParseError $ "Invalid logging priority " ++ s, "")
            Just p  -> return p

initLogLevel :: Config.ConfigParser -> String -> String -> IO ()
initLogLevel conf component variable =
    either (const $ return ()) (updateGlobalLogger component . setLevel) (Config.get conf component variable)

-- | Initialize the application.
initialize :: IO ()
initialize = do
    conf <- Config.getConfig
    initLogLevel conf "BufferCache"  "logLevel"
    initLogLevel conf "FeatureSpace" "logLevel"
    initLogLevel conf "Sequencer"    "logLevel"
    initLogLevel conf "Synth"        "logLevel"
