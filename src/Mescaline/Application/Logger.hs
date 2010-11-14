module Mescaline.Application.Logger (
    module System.Log.Logger
  , module System.Log.Formatter
  , module System.Log.Handler.Simple
  , initialize
  , components
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

initLogLevel :: Config.ConfigParser -> String -> String -> IO ()
initLogLevel conf component variable =
    either (const $ return ()) (updateGlobalLogger component . setLevel) (Config.get conf component variable)

componentMap :: [(String, String)]
componentMap =
    [ ("BufferCache"  , "logLevel")
    , ("FeatureSpace" , "logLevel")
    , ("Hugs"         , "logLevel")
    , ("Sequencer"    , "logLevel")
    , ("Synth"        , "logLevel") ]

components :: [String]
components = map fst componentMap

-- | Initialize the application.
initialize :: IO ()
initialize = do
    conf <- Config.getConfig
    mapM_ (uncurry (initLogLevel conf)) componentMap
