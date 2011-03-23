module Mescaline.Analysis.Meap.Process (
    OutputHandler(..)
  , defaultOutputHandler
  , ClassPath
  , makeClassPath
  , runMeap
) where

import           Control.Concurrent
import           Control.Monad
import           Data.List (intercalate)
-- import qualified Mescaline.Application as App
import           Mescaline.Util (findFiles, withTempFile)
import           System.Exit
import           System.IO
import           System.Process

-- | Handle output of @Meap@ processes.
data OutputHandler = OutputHandler {
    onPutString :: String -> IO ()     -- ^ Handle one line of normal output
  , onPutError  :: String -> IO ()     -- ^ Handle one line of error output
  }

-- | Default IO handler, writing to stdout and stderr, respectively.
defaultOutputHandler :: OutputHandler
defaultOutputHandler = OutputHandler {
    onPutString = hPutStrLn stdout
  , onPutError  = hPutStrLn stderr
  }

type ClassPath = [FilePath]

getJava :: IO String
getJava = return "java" -- get from config file

makeClassPath :: FilePath -> IO ClassPath
makeClassPath libDir = findFiles ["jar"] [libDir]

runMeap :: ClassPath -> OutputHandler -> String -> [String] -> IO ExitCode
runMeap classPath handler mainClass args = do
    java <- getJava
    let args' = [ "-mx1000m",
                  "-cp",
                  intercalate ":" classPath,
                  mainClass ] ++ args
    print args'
    (hIn, hOut, hErr, hProc) <- runInteractiveProcess java args' Nothing Nothing
    hClose hIn
    _ <- forkIO $ pipeOutput (onPutString handler) hOut
    _ <- forkIO $ pipeOutput (onPutError  handler) hErr
    waitForProcess hProc

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)
