module Mescaline.Application.Logger (
    module System.Log.Logger
  , module System.Log.Formatter
  , module System.Log.Handler.Simple
) where

import           Control.Monad.Error
import           Data.Char (toUpper)
import qualified Data.ConfigFile as Config
import qualified Mescaline.Application.Config as Config
import           Mescaline.Util (readMaybe)
import           System.Log.Formatter
import           System.Log.Handler.Simple
import           System.Log.Logger hiding (debugM, infoM, noticeM)

instance Config.Get_C (Priority) where
    get config section option = do
        s <- Config.get config section option
        case readMaybe (map toUpper s) of
            Nothing -> throwError (Config.ParseError $ "Invalid logging priority " ++ s, "")
            Just p  -> return p
