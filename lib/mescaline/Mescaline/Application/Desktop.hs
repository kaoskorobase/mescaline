module Mescaline.Application.Desktop (
    openUrl
) where

import Mescaline.Application
import System.Process (system)
import System.Exit (ExitCode(..))

openUrl :: String -> IO ExitCode
openUrl url =
    let cmd = case buildOS of
                OSX     -> Just "open"
                Linux   -> Just "xdg-open"
                Windows -> Just "start"
                _ -> Nothing
    in maybe (return (ExitFailure 255)) (system . (++" "++url)) cmd
