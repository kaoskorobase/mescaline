module Mescaline.Meap.Process (
    OutputHandler(..)
  , defaultOutputHandler
  , runMeap
  , withTempFile
) where

import           Control.Concurrent
import           Control.Monad
import           Data.List (intercalate)
import           System.Directory (getHomeDirectory, getTemporaryDirectory)
import           System.Exit
import           System.FilePath (combine, joinPath)
import           System.FilePath.Glob (namesMatching)
import           System.IO
import           System.Process
import qualified Distribution.Simple.Utils  as Cabal

java :: String
java = "java" -- get from config file

getLibraryDirectory :: FilePath -> IO FilePath
getLibraryDirectory dir = do
    return (joinPath [dir, "lib", "meap-2.0"])

getClassPath :: IO [FilePath]
getClassPath = do
    localLibDir <- getLibraryDirectory "tools/meap"
    homeLibDir <- getHomeDirectory >>= getLibraryDirectory
    jars <- mapM (namesMatching . flip combine "*.jar") [localLibDir, homeLibDir]
    return $ concat jars

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
    classPath <- getClassPath
    let args' = [ "-mx1000m",
                  "-cp",
                  intercalate ":" classPath,
                  mainClass ] ++ args
    -- print args'
    (hIn, hOut, hErr, hProc) <- runInteractiveProcess java args' Nothing Nothing
    hClose hIn
    forkIO $ pipeOutput (onPutString handler) hOut
    forkIO $ pipeOutput (onPutError  handler) hErr
    waitForProcess hProc

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile s f = do
    tmpDir <- getTemporaryDirectory
    Cabal.withTempFile tmpDir s f

pipeOutput :: (String -> IO ()) -> Handle -> IO ()
pipeOutput f h = hIsEOF h >>= flip unless (hGetLine h >>= f >> pipeOutput f h)
