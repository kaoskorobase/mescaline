{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Engine where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
-- import           Data.Accessor
import           Data.Bits
import           Data.Char (ord)
import           Data.Function (fix)
import qualified Data.List as List
import           Data.Maybe
import           Data.Version (showVersion)
import           Mescaline (Time)
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Desktop as App
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.Process as DatabaseP
import qualified Mescaline.Pattern.Sequencer as Sequencer
import qualified Mescaline.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.FeatureSpace.Process as FeatureSpaceP
import qualified Mescaline.Pattern as Pattern
import qualified Mescaline.Pattern.Environment as Pattern
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Pattern.Patch as Patch
import qualified Mescaline.Pattern.Process as PatternP
import qualified Mescaline.Synth.OSCServer as OSCServer
import qualified Mescaline.Synth.Sampler.Process as SynthP
import           Mescaline.Util (findFiles)
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Process as Server
import qualified Sound.SC3.Server.Process.CommandLine as Server
import           System.Directory
import qualified System.Environment as Env
import           System.Environment.FindBin (getProgPath)
import           System.FilePath
import           System.IO
import qualified System.Random as Random

pipe :: (a -> IO b) -> Chan a -> Chan b -> IO ()
pipe f ichan ochan = do
    a <- readChan ichan
    b <- f a
    writeChan ochan b
    pipe f ichan ochan

-- ====================================================================
-- Logging to text view

-- chanLogger :: Log.Priority -> String -> Chan String -> IO () -> Log.GenericHandler (Chan String)
-- chanLogger prio fmt chan action =
--     Log.GenericHandler
--         prio
--         (Log.simpleLogFormatter fmt)
--         chan
--         (\chan msg -> writeChan chan msg >> action)
--         (const (return ()))
-- 
-- createLoggers :: MainWindow -> IO ()
-- createLoggers logWindow = do
--     textEdit <- Qt.findChild logWindow ("<QTextEdit*>", "textEdit") :: IO (Qt.QTextEdit ())
--     chan <- newChan
--     Qt.connectSlot logWindow "logMessage()" logWindow "logMessage()" $ logMessage chan textEdit
--     let fmt = "[$prio][$loggername] $msg\n"
--         action = Qt.emitSignal logWindow "logMessage()" ()
--     components <- Log.getComponents
--     -- FIXME: The log levels have to be initialized first down in main, why?
--     mapM_ (\(logger, prio) -> do
--         Log.updateGlobalLogger
--             logger
--             (Log.setHandlers [chanLogger prio fmt chan action]))
--             components
--     -- Disable stderr logger
--     Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers ([] :: [Log.GenericHandler ()]))
--     where
--         logMessage :: Chan String -> Qt.QTextEdit () -> MainWindow -> IO ()
--         logMessage chan edit _ = do
--             msg <- readChan chan
--             c <- Qt.textCursor edit ()
--             Qt.insertText c msg
--             _ <- Qt.movePosition c (Qt.eEnd :: Qt.MoveOperation)
--             Qt.setTextCursor edit c
-- 
-- clearLog :: MainWindow -> IO ()
-- clearLog logWindow = do
--     edit <- Qt.findChild logWindow ("<QTextEdit*>", "textEdit") :: IO (Qt.QTextEdit ())
--     Qt.setPlainText edit ""

-- ====================================================================
-- Actions

logStrLn :: MonadIO m => String -> m ()
logStrLn = liftIO . hPutStrLn stderr

logAppDirs :: MonadIO m => AppT m ()
logAppDirs = do
    d1 <- App.getProgramDirectory
    d2 <- App.getDataDirectory
    d3 <- App.getResourceDirectory
    logStrLn $ show [d1, d2, d3]

engine :: FilePath -> String -> AppT IO (SynthP.Handle, FeatureSpaceP.Handle, IO ())
engine dbFile pattern = do
    -- logAppDirs
    -- 
    -- docDir <- liftM (flip combine "Documents" . takeDirectory) App.getProgramDirectory
    -- logStrLn $ "Documents: " ++ docDir

    -- components <- Log.getComponents
    -- liftIO $ mapM_ (\(l,p) -> Log.updateGlobalLogger l (Log.setLevel p)) components

    -- createLoggers logWindow

    -- Synth process
    (synthP, synthQuit) <- SynthP.new

    logStrLn "Synth started"
    
    -- Feature space process
    fspaceP <- liftIO FeatureSpaceP.new

    logStrLn "FeatureSpace started"

    -- -- Sequencer process
    patternP <- PatternP.new Patch.defaultPatchEmbedded fspaceP
    
    logStrLn "Sequencer started"
    
    -- -- Database process
    dbP <- liftIO DatabaseP.new
    liftIO $ connect (\(DatabaseP.Changed path pattern) -> FeatureSpaceP.LoadDatabase path pattern) dbP fspaceP
    -- let dbFile = docDir </> "mescaline.db"
    sendTo dbP $ DatabaseP.Load dbFile pattern
    
    logStrLn "Database started"
    
    -- -- Pattern process
    -- patternToFSpaceP <- spawn $ fix $ \loop -> do
    --     x <- recv
    --     case x of
    --         PatternP.Event time event -> do
    --             -- Event.withSynth (return ()) (sendTo synthP . SynthP.PlayUnit time) event
    --             return ()
    --         _ -> return ()
    --     loop
    -- patternToFSpaceP `listenTo` patternP
    -- fspaceToPatternP <- spawn $ fix $ \loop -> do
    --     x <- recv
    --     case x of
    --         FeatureSpaceP.RegionChanged _ -> do
    --             fspace <- query fspaceP FeatureSpaceP.GetModel
    --             sendTo patternP $ PatternP.SetFeatureSpace fspace
    --         _ -> return ()
    --     loop
    -- fspaceToPatternP `listenTo` fspaceP
    
    -- OSC server process
    -- oscServer <- OSCServer.new 2010 synthP fspaceP
    -- logStrLn "OSCServer started"

    -- logStrLn "Starting event loop"

    -- Signal synth thread and wait for it to exit.
    -- Otherwise stale scsynth processes will be lingering around.
    return (synthP, fspaceP, synthQuit >> logStrLn "Bye sucker.")
