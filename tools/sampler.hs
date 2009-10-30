{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import qualified  Mescaline.Database as DB
import Mescaline.Synth.Concat
import Mescaline.Synth.Pattern as P
import Mescaline.Synth.Pattern.Load as P

import System.Environment (getArgs)
import Control.Exception
import Sound.SC3
import Sound.OpenSoundControl.Transport
import System.FilePath
import Prelude hiding (catch)

main :: IO ()
main = do
    [dbDir, patternFile] <- getArgs
    db <- DB.open dbDir
    -- print db
    ps <- P.loadFile patternFile
    case ps of
        Left err ->
			case err of
				WontCompile es -> putStrLn $ unlines $ map errMsg es
				e              -> print e
        Right (P.PCons pfunc) -> do
            sampler <- newSampler
            let pattern = pfunc db
            (playPattern sampler pattern)
                `catch` \(ex :: AsyncException) ->
                    freeSampler sampler >> putStrLn "Servas."
