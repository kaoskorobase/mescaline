{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Chan
import qualified Mescaline.Database.FlatFile as DB
import           Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Synth.Concat
import           Mescaline.Synth.Pattern as P
import           Mescaline.Synth.Pattern.Load as P

import qualified Mescaline.Sampler.GUI as GUI
import qualified Mescaline.Sampler.Keyboard as GUI
import qualified Mescaline.Sampler.Looper as Looper

import System.Environment (getArgs)
import Control.Exception
import Sound.SC3
import Sound.OpenSoundControl.Transport
import Sound.OpenSoundControl.Transport.UDP (UDP)
import System.FilePath
import Prelude hiding (catch)

patternFromFile :: DB.Database -> FilePath -> IO (Either String P.Pattern)
patternFromFile db patternFile = do
    ps <- P.loadFile patternFile
    case ps of
        Left err ->
			case err of
				WontCompile es -> return $ Left $ unlines $ map errMsg es
				e              -> return $ Left $ show e
        Right (P.PCons pfunc) -> return (Right $ pfunc db)

handlePattern :: Chan P.Pattern -> Either String P.Pattern -> IO ()
handlePattern _ (Left s)  = putStrLn s
handlePattern c (Right p) = writeChan c p

-- playSampler' :: DB.Database -> FilePath -> Sampler -> IO ()
-- playSampler' db patternFile sampler = do
--     ps <- P.loadFile patternFile
--     case ps of
--         Left err ->
--          case err of
--              WontCompile es -> putStrLn $ unlines $ map errMsg es
--              e              -> print e
--         Right (P.PCons pfunc) -> do
--             let pattern = pfunc db
--             playPattern sampler pattern

playSampler :: Chan P.Input -> IO ()
playSampler = bracket newSampler freeSampler . playPattern 0.001 (P.constant noEvent)

main :: IO ()
main = do
    [dbDir] <- getArgs
    db <- DB.open dbDir
    c <- newChan
    -- forkIO $ playSampler c
    playSampler c
    -- GUI.main (\p -> patternFromFile db p >>= handlePattern c)
