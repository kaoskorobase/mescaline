module Mescaline.Meap.Process (
    runMeap,
    withTempFile
) where

import Data.List                            (intercalate)
import System.Directory                     (getHomeDirectory, getTemporaryDirectory)
import System.Exit                          (ExitCode)
import System.FilePath                      (combine, joinPath)
import System.FilePath.Glob                 (namesMatching)
import System.IO                            (Handle)
import System.Process                       (runProcess, waitForProcess)
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

runMeap :: String -> [String] -> IO ExitCode
runMeap mainClass args = do
    classPath <- getClassPath
    let args' = [ "-mx1000m",
                  "-cp",
                  intercalate ":" classPath,
                  mainClass ] ++ args
    -- print args'
    runProcess java
        args'
        Nothing Nothing
        Nothing Nothing Nothing
        >>= waitForProcess
          
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile s f = do
    tmpDir <- getTemporaryDirectory
    Cabal.withTempFile tmpDir s f
