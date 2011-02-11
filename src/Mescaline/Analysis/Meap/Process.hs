module Mescaline.Analysis.Meap.Process (
    OutputHandler(..)
  , defaultOutputHandler
  , runMeap
) where

import           Control.Concurrent
import           Control.Monad
import           Data.List (intercalate)
import qualified Mescaline.Application as App
import           Mescaline.Util (findFiles, withTempFile)
import           System.Exit
import           System.IO
import           System.Process

getJava :: IO String
getJava = return "java" -- get from config file

getClassPath :: IO [FilePath]
getClassPath = do
    libDir <- App.getResourcePath "meap/2.0"
    findFiles ["jar"] [libDir]

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

runMeap :: OutputHandler -> String -> [String] -> IO ExitCode
runMeap handler mainClass args = do
    java <- getJava
    classPath <- getClassPath
    let args' = [ "-mx1000m",
                  "-cp",
                  intercalate ":" classPath,
                  mainClass ] ++ args
    -- print args'
    (hIn, hOut, hErr, hProc) <- runInteractiveProcess java args' Nothing Nothing
    hClose hIn
    _ <- forkIO $ pipeOutput (onPutString handler) hOut
    _ <- forkIO $ pipeOutput (onPutError  handler) hErr
    waitForProcess hProc

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)
