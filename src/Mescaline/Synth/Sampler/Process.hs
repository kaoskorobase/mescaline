{-# LANGUAGE FlexibleContexts #-}
module Mescaline.Synth.Sampler.Process (
    Handle
    -- * Input
  , Input(..)
    -- * Output
  , Output(..)
  , new
) where

import           Control.Concurrent
import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Exception
import           Control.Monad.Error
import           Data.Accessor
import           Mescaline (Time)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import           Mescaline.Synth.Pattern.Event (Synth)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Sampler.Model as Model
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 (dumpOSC, PrintLevel(..))
import qualified Sound.SC3.Server.Process as Server
import qualified Sound.SC3.Server.Process.Monad as Server
import           Sound.SC3.Server.Monad as S
import qualified Sound.SC3.Server.Process.CommandLine as Server

data Input =
    Reset
  | Quit
  | PlayUnit Time Synth
  | EngineException_ SomeException
  deriving (Show)

data Output =
    UnitStarted Time Unit.Unit
  | UnitStopped Time Unit.Unit
  | EngineException SomeException

type Handle = Process.Handle Input Output

getEnginePaths :: IO (FilePath, Maybe [FilePath])
getEnginePaths = do
    exe <- App.getResourceExecutable "usr/local/bin/scsynth"
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
            plg <- App.getResourcePath "usr/local/lib/supercollider/plugins"
            return (exe', Just [plg])

getPrintLevel :: MonadError Config.CPError m => Config.ConfigParser -> Config.SectionSpec -> Config.OptionSpec -> m PrintLevel
getPrintLevel conf section option = do
    s <- Config.get conf section option
    case s of
        "none" -> return NoPrinter
        "hex"  -> return HexPrinter
        "text" -> return TextPrinter
        "all"  -> return AllPrinter
        _      -> throwError (Config.ParseError $ "Invalid OSC print level " ++ s, "")

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

    Log.infoM "Synth" (unwords (Server.rtCommandLine serverOptions rtOptions))
    
    conf <- Config.getConfig
    let printLevel = either (const NoPrinter) id (getPrintLevel conf "Synth" "dumpOSC")
        initBundle = OSC.Bundle OSC.immediately [dumpOSC printLevel]

    chan <- newChan
    quit <- newEmptyMVar
    h <- spawn $ process chan
    _ <- forkIO $ runSynth serverOptions rtOptions initBundle h chan quit

    return (h, sendTo h Quit >> readMVar quit)
    where
        runSynth serverOptions rtOptions initBundle h chan quit = do
            e <- try $
                Server.withSynthUDP
                    serverOptions
                    rtOptions
                    (Server.OutputHandler (Log.noticeM "Synth") (Log.errorM "Synth"))
                    -- (Server.withTransport
                    --     serverOptions
                    --     rtOptions
                    (S.async (S.send initBundle) >> Model.new >>= loop h chan)
            case e of
                Left exc -> writeChan chan $ EngineException_ exc
                _ -> return ()
            putMVar quit ()
        process :: Chan Input -> ReceiverT Input Output IO ()
        process chan = do
            x <- recv
            case x of
                EngineException_ exc -> notify $ EngineException exc
                msg -> io $ writeChan chan msg
            process chan
        loop :: Handle -> Chan Input -> Model.Sampler -> Server ()
        loop h chan sampler = do
            x <- io $ readChan chan
            case x of
                Quit ->
                    return ()
                Reset -> do
                    Model.free sampler
                    loop h chan sampler
                PlayUnit t s -> do
                    _ <- fork $ do
                        let u = s ^. Event.unit
                        io $ notifyListeners h $ UnitStarted t u
                        io $ Log.debugM "Synth" $ "PlayUnit: " ++ show (t, s)
                        Model.playUnit sampler t s
                        io $ Log.debugM "Synth" $ "StopUnit: " ++ show (t, s)
                        io $ notifyListeners h $ UnitStopped t u
                    loop h chan sampler
                _ -> loop h chan sampler
