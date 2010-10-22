module Mescaline.Synth.Sampler.Process (
    Handle
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Synth.Pattern.Event (SynthParams)
import qualified Mescaline.Synth.Sampler.Model as Model
import qualified Sound.SC3.Server.Process as Server
import qualified Sound.SC3.Server.Process.Monad as Server
import           Sound.SC3.Server.Monad as S
import qualified Sound.SC3.Server.Process.CommandLine as Server

data Input =
    Reset
  | Quit
  | PlayUnit Time Unit.Unit SynthParams
  deriving (Show)

data Output =
    UnitStarted Time Unit.Unit
  | UnitStopped Time Unit.Unit
  -- | EngineStopped ExitCode

type Handle = Process.Handle Input Output

getEnginePaths :: IO (FilePath, Maybe [FilePath])
getEnginePaths = do
    exe <- App.getResourceExecutable "supercollider/scsynth"
    case exe of
        Nothing -> do
            exe' <- App.findExecutable "scsynth"
            case exe' of
                -- TODO: Display this in the UI
                Nothing -> do
                    d <- App.getResourceDirectory
                    fail $ unwords [
                            "I couldn't find the SuperCollider audio engine `scsynth'."
                          , "You need to put it either in `" ++ d ++ "/supercollider" ++ "' or into your PATH."
                          , "WARNING: Sound output will not work!" ]
                Just exe'' -> return (exe'', Nothing)
        Just exe' -> do
            plg <- App.getResourcePath "supercollider/plugins"
            return (exe', Just [plg])

new :: IO (Handle, IO ())
new = do
    (scsynth, plugins) <- getEnginePaths
    let
        serverOptions = Server.defaultServerOptions {
            Server.loadSynthDefs  = False
          , Server.serverProgram  = scsynth
          , Server.ugenPluginPath = plugins
          }
        rtOptions = Server.defaultRTOptions { Server.udpPortNumber = 2278 }
    putStrLn $ unwords $ Server.rtCommandLine serverOptions rtOptions
    handle <- newEmptyMVar
    quit <- newEmptyMVar
    _ <- forkIO $ runSynth serverOptions rtOptions handle quit
    h <- takeMVar handle
    return (h, sendTo h Quit >> takeMVar quit)
    where
        runSynth serverOptions rtOptions handle quit = do
            Server.withSynthUDP
                serverOptions
                rtOptions
                Server.defaultOutputHandler
                -- (Server.withTransport
                --     serverOptions
                --     rtOptions
                $ do
                    sampler <- Model.new
                    io $ putStrLn "starting sampler loop"
                    runHere $ do
                        self >>= io . putMVar handle
                        loop sampler
                    return ()
            putMVar quit ()
        loop :: Model.Sampler -> ReceiverT Input Output Server ()
        loop sampler = do
            x <- recv
            io $ print x
            case x of
                Quit ->
                    return ()
                Reset -> do
                    lift $ Model.free sampler
                    loop sampler
                PlayUnit t u p -> do
                    h <- self
                    _ <- lift $ fork $ do
                        io $ notifyListeners h $ UnitStarted t u
                        -- io $ putStrLn $ "playUnit: " ++ show (t, u, p)
                        Model.playUnit sampler t u p
                        -- io $ putStrLn $ "stoppedUnit: " ++ show (t, u, p)
                        io $ notifyListeners h $ UnitStopped t u
                    loop sampler

--                     fromSynthP <- spawn $ fix $ \loop -> do
--                         x <- recv
--                         case x of
--                             Synth.UnitStopped time unit -> sendTo fspaceP $ FeatureSpaceP.DeactivateUnit time unit
--                             _                           -> return ()
--                         loop
--                     fromSynthP `listenTo` synth
--                     fix $ \loop -> do
--                         e <- readChan ichan
--                         case e of
--                             Left (t, u) -> do
--                                 -- b <- readMVar mute
--                                 -- unless b $ do
--                                 -- print u
--                                 sendTo synth $ Synth.PlayUnit t u (setEnv Event.defaultSynth)
--                                 -- return ()
--                                 loop
--                             Right _ -> return ()) `finally` writeChan ochan ()            -- TODO: Create a ServerT monad transformer!
