{-# LANGUAGE ScopedTypeVariables #-}

import Mescaline.Synth.Concat
import System.Environment (getArgs)
import Control.Exception
import Sound.SC3
import Sound.OpenSoundControl.Transport
import Prelude hiding (catch)

main :: IO ()
main = do
    [path] <- getArgs
    s <- newSampler
    (playFile s path) `catch` \(ex :: AsyncException) -> (freeSampler s >> putStrLn "Servas.")
