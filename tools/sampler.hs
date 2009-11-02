{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import qualified  Mescaline.Database as DB
import Mescaline.Database.SourceFile as SourceFile
import Mescaline.Synth.Concat
import Mescaline.Synth.Pattern as P
import Mescaline.Synth.Pattern.Load as P

import System.Environment (getArgs)
import Control.Exception
import Sound.SC3
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP (UDP)
import System.FilePath
import Prelude hiding (catch)

main' :: FilePath -> FilePath -> Sampler UDP -> IO ()
main' dbDir patternFile sampler = do
    db <- DB.open dbDir
    mapM_ putStrLn (map SourceFile.path $ DB.sourceFiles db)
    ps <- P.loadFile patternFile
    case ps of
        Left err ->
			case err of
				WontCompile es -> putStrLn $ unlines $ map errMsg es
				e              -> print e
        Right (P.PCons pfunc) -> do
            let pattern = pfunc db
            playPattern sampler pattern

main :: IO ()
main = do
    [dbDir, patternFile] <- getArgs
    bracket newSampler freeSampler (main' dbDir patternFile)
