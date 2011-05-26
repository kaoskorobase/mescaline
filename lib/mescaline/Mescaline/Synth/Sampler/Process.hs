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
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Application.Config as Config
import qualified Mescaline.FeatureSpace.Unit as Unit
import           Mescaline.Pattern.Event (Synth)
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Synth.Sampler.Model as Model
import qualified Mescaline.Synth.Sampler.Params as P
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3 (dumpOSC, PrintLevel(..))
import qualified Sound.SC3.Server.Process as Server
import qualified Sound.SC3.Server.Process.Monad as ServerM
import qualified Sound.SC3.Server.Monad as S
import           System.IO (hPutStrLn, stderr, stdout)
import qualified System.Log.Logger as Log

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

getEnginePaths :: MonadIO m => AppT m (FilePath, Maybe [FilePath])
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

logger = "Mescaline.Synth"

getEngine internal =
    if internal
        then do
            let plgBase = "usr/local/lib/supercollider/plugins"
                splg = "/" ++ plgBase
            rplg <- App.getResourcePath plgBase
            let plg = [rplg, splg]
            liftIO $ Log.infoM logger $ "Using internal server with plugin path `" ++ L.intercalate ":" plg ++ "'"
            return $ ServerM.withInternal
                        serverOptions { Server.ugenPluginPath = Just plg }
                        rtOptions
                        outputHandler
        else do
            (scsynth, plg) <- getEnginePaths
            liftIO $ Log.infoM logger $ "Using local server `" ++ scsynth ++ "'"
                              ++ maybe "" (\p -> " with plugins path `" ++ L.intercalate ":" p ++ "'") plg
            return $ ServerM.withSynth
                        serverOptions { Server.serverProgram = scsynth
                                      , Server.ugenPluginPath = plg }
                        rtOptions { Server.networkPort = Server.UDPPort 2278 }
                        outputHandler
    where
        serverOptions = Server.defaultServerOptions { Server.loadSynthDefs = False }
        rtOptions = Server.defaultRTOptions
        -- outputHandler = Server.OutputHandler (Log.noticeM logger) (Log.errorM logger)
        outputHandler = Server.OutputHandler (hPutStrLn stdout) (hPutStrLn stderr)

getPrintLevel :: MonadIO m => AppT m PrintLevel
getPrintLevel = do
    s <- App.config "Synth" "dumpOSC" "none"
    case s of
        "none" -> return NoPrinter
        "hex"  -> return HexPrinter
        "text" -> return TextPrinter
        "all"  -> return AllPrinter
        _      -> return NoPrinter

new :: AppT IO (Handle, IO ())
new = do
    defDir <- App.getUserDataPath "synthdefs"

    printLevel <- getPrintLevel
    fx1 <- App.config "Synth" "sendEffect1" Nothing
    fx2 <- App.config "Synth" "sendEffect2" Nothing
    schedCompBdls <- App.config "Synth" "scheduleCompletionBundles" True
    useIntServ <- App.config "Synth" "useInternalServer" True

    let initBundle = OSC.Bundle OSC.immediately [dumpOSC printLevel]
        modelOpts = Model.Options defDir fx1 fx2 schedCompBdls

    liftIO $ Log.noticeM logger $ (if schedCompBdls then "U" else "Not u") ++ "sing completion bundle scheduling."

    engine <- getEngine useIntServ
    chan <- liftIO $ newChan
    quit <- liftIO $ newEmptyMVar
    h <- spawn $ process chan
    _ <- liftIO $ forkIO $ runSynth engine initBundle modelOpts h chan quit

    return (h, sendTo h Quit >> readMVar quit)
    where
        runSynth engine initBundle modelOpts h chan quit = do
            e <- try $ engine (S.send initBundle >> Model.new modelOpts >>= loop h chan)
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
        loop :: Handle -> Chan Input -> Model.Sampler -> S.Server ()
        loop h chan sampler = do
            x <- io $ readChan chan
            case x of
                Quit ->
                    return ()
                Reset -> do
                    Model.free sampler
                    loop h chan sampler
                PlayUnit t s -> do
                    _ <- S.fork $ do
                        let u = s ^. Event.unit
                        io $ notifyListeners h $ UnitStarted t u
                        io $ Log.debugM logger $ "PlayUnit: " ++ show (t, s)
                        Model.playUnit sampler t (fromSynth s)
                        io $ Log.debugM logger $ "StopUnit: " ++ show (t, s)
                        io $ notifyListeners h $ UnitStopped t u
                    loop h chan sampler
                _ -> loop h chan sampler

fromSynth :: Synth -> P.Params
fromSynth s =
    P.Params {
        P.file         = Unit.sourceFile (s ^. Event.unit)
      , P.unit         = Unit.unit (s ^. Event.unit)
      , P.offset       = s ^. Event.offset
      , P.duration     = Unit.duration (s ^. Event.unit)
      , P.rate         = s ^. Event.rate
      , P.pan          = s ^. Event.pan
      , P.attackTime   = s ^. Event.attackTime
      , P.releaseTime  = s ^. Event.releaseTime
      , P.sustainLevel = s ^. Event.sustainLevel
      , P.gateLevel    = s ^. Event.gateLevel
      , P.sendLevel1   = s ^. Event.sendLevel1
      , P.sendLevel2   = s ^. Event.sendLevel2
      , P.fxParam1     = s ^. Event.fxParam1
      , P.fxParam2     = s ^. Event.fxParam2
      }
