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
import qualified Data.List as L
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
import qualified Sound.SC3.Server.Process.Monad as ServerM
import           Sound.SC3.Server.Monad as S

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

getEngine internal =
    if internal
        then do
            let plgBase = "usr/local/lib/supercollider/plugins"
                splg = "/" ++ plgBase
            rplg <- App.getResourcePath plgBase
            let plg = [rplg, splg]
            Log.infoM "Synth" $ "Using internal server with plugin path `" ++ L.intercalate ":" plg ++ "'"
            return $ ServerM.withInternal
                        serverOptions { Server.ugenPluginPath = Just plg }
                        rtOptions
                        outputHandler
        else do
            (scsynth, plg) <- getEnginePaths
            Log.infoM "Synth" $ "Using local server `" ++ scsynth ++ "'"
                              ++ maybe "" (\p -> " with plugins path `" ++ L.intercalate ":" p ++ "'") plg
            return $ ServerM.withSynth
                        Server.openUDP
                        serverOptions { Server.serverProgram = scsynth
                                      , Server.ugenPluginPath = plg }
                        rtOptions { Server.udpPortNumber = 2278 }
                        outputHandler
    where
        serverOptions = Server.defaultServerOptions { Server.loadSynthDefs = False }
        rtOptions = Server.defaultRTOptions
        outputHandler = Server.OutputHandler (Log.noticeM "Synth") (Log.errorM "Synth")

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
    conf <- Config.getConfig

    -- Get the engine function depending on config file variable
    engine <- getEngine (either (const False) id (Config.get conf "Synth" "useInternalServer"))

    let printLevel = either (const NoPrinter) id (getPrintLevel conf "Synth" "dumpOSC")
        initBundle = OSC.Bundle OSC.immediately [dumpOSC printLevel]

    chan <- newChan
    quit <- newEmptyMVar
    h <- spawn $ process chan
    _ <- forkIO $ runSynth engine initBundle h chan quit

    return (h, sendTo h Quit >> readMVar quit)
    where
        runSynth engine initBundle h chan quit = do
            e <- try $ engine (S.async initBundle >> Model.new >>= loop h chan)
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
