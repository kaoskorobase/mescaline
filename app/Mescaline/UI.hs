{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.UI (
  getColor
) where

import           Data.Bits
import qualified Mescaline.Application as App
import           Mescaline.Application.Config
import           Mescaline.Util (readMaybe)
import           Qtc.ClassTypes.Gui
import           Qtc.Gui.QColor
import           Qtc.Classes.Qccs
import           Text.Regex

hexAlphaRegex :: Regex
hexAlphaRegex = mkRegex "^(#[A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9][A-Ba-b0-9])([A-Ba-b0-9][A-Ba-b0-9])$"

alphaRegex :: Regex
alphaRegex = mkRegex "^([^*]+)\\*([0-9]+(\\.[0-9]+)?)$"

getColor :: ConfigParser -> SectionSpec -> OptionSpec -> IO (QColor ())
getColor config section option = do
    colorSpec <- getIO config section option
    case matchRegex hexAlphaRegex colorSpec of
        Just (rgb:alpha:_) -> do
            color <- qColor rgb
            case readMaybe ("0x" ++ alpha) of
                Nothing         -> return ()
                Just (a :: Int) -> setAlpha color a
            return color
        _ ->
            case matchRegex alphaRegex colorSpec of
                Just (name:alpha:_) -> do
                    color <- qColor name
                    case readMaybe alpha of
                        Nothing -> return ()
                        Just a  -> setAlphaF color a
                    return color
                _ -> qColor colorSpec
