{-# LANGUAGE ScopedTypeVariables #-}

import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..))

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad (unless)
import           Control.Monad.Trans (liftIO)
import           Data.Accessor
import           Data.IORef
import qualified Mescaline.Database.FlatFile as DB
import           Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Unit as Unit
import           Mescaline.Synth.Concat
import qualified Mescaline.Synth.Pattern as P
import           Mescaline.Synth.Pattern.Load as P

import qualified Mescaline.Sampler.GUI as GUI
import qualified Mescaline.Sampler.Keyboard as GUI
import qualified Mescaline.Sampler.MIDI as MIDI
import qualified Mescaline.Sampler.Looper as L

import           System.Environment (getArgs)
import           Control.Exception
import           Sound.SC3
import           Sound.OpenSoundControl (utcr)
import           Sound.OpenSoundControl.Transport
import           Sound.OpenSoundControl.Transport.UDP (UDP)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath
import           Prelude hiding (catch)

import           Graphics.UI.GLUT

playSampler :: Chan (Double, P.Event) -> IO ()
playSampler = bracket newSampler freeSampler . flip runSampler

initialSize :: Int
initialSize = 256

keyChars = [
    'z' , 'a' , 'q'
  , 'x' , 's' , 'w'
  , 'c' , 'd' , 'e'
  , 'v' , 'f' , 'r'
  , 'b' , 'g' , 't'
  , 'n' , 'h' , 'y'
  , 'm' , 'j' , 'u'
  ]

tr505_notes = concat [
    [ 43 .. 50 ]
  , [ 35 .. 42 ]
  ]

latency :: Double
latency = 0.015

mkEvent :: Unit.Unit -> P.Event
mkEvent = setVal (P.synth.>P.latency) latency . P.event

midiCallback :: Chan (Double, P.Event) -> [(Int, Unit.Unit)] -> MIDI.MidiEvent -> IO ()
midiCallback c us (MIDI.MidiEvent _ (MIDI.MidiMessage _ (MIDI.NoteOn n _))) =
    case lookup n us of
        Nothing -> return ()
        Just u  -> do
            t <- utcr
            let e = (t, mkEvent u)
            -- print e
            writeChan c e
midiCallback _ _ _ = return ()

main_midi db us' = do
    let us = cycle tr505_notes `zip` cycle us'
    Just midiSource <- MIDI.findSource (\"RME" _ "Port 1" -> True)
    midi <- newChan
    midiConn <- MIDI.openSource midiSource (Just $ midiCallback midi us)
    MIDI.start midiConn
    playSampler midi

--writeLooperEvent :: Chan (Double, L.Event P.Event) -> L.Event P.Event -> EM.EventM EM.EKey ()
--writeLooperEvent c e = liftIO $ do
--    t <- utcr
--    writeChan c (t, e)
--
--main_gui_looper db us' = do
--    let us = cycle keyChars `zip` cycle us'
--
--    unsafeInitGUIForThreadedRTS
--    
--    window <- windowNew
--    -- windowSetDecorated window False
--    windowSetResizable window True
--    windowSetPosition window WinPosCenterAlways
--    
--    widgetSetAppPaintable window True
--    windowSetTitle window "Fucking hell"
--    windowSetDefaultSize window initialSize initialSize
--    windowSetGeometryHints window (Just window)
--        (Just (32, 32)) (Just (512, 512))
--        Nothing Nothing (Just (1,1))
--
--    looperInput <- newChan
--    looperOutput <- newChan
--    
--    window `on` keyPressEvent $ do
--        keyName <- EM.eventKeyName
--        case keyName of
--            "Return" -> do
--                writeLooperEvent looperInput L.ERecord
--                return True
--            "space" -> do
--                writeLooperEvent looperInput L.EStart
--                return True
--            "BackSpace" -> do
--                writeLooperEvent looperInput L.EStop
--                return True
--            "Escape" -> do
--                liftIO mainQuit
--                return True
--            c -> if elem c keyChars
--                 then let Just u = lookup c us
--                      in do
--                          writeLooperEvent looperInput (L.EInsert (mkEvent u))
--                          return True
--                 else return False
--    forkIO $ L.run (looperInput, looperOutput)
--    forkIO $ playSampler looperOutput
--
--    widgetShowAll window
--    mainGUI
--
--writePlayerEvent :: Chan (Double, P.Event) -> P.Event -> EM.EventM EM.EKey ()
--writePlayerEvent c e = liftIO $ do
--    t <- utcr
--    writeChan c (t, e)
--
--getEvents :: WidgetClass w => w -> IO (Chan Event)
--getEvents w = do
--    prevEventRef <- newIORef Nothing
--    output <- newChan
--    onKeyPress w $ \e -> do
--        prevEvent <- readIORef prevEventRef
--        case prevEvent of
--            Nothing -> do
--                write prevEventRef (eventKeyVal e, eventModifier e)
--                writeChan output e
--                return True
--            Just (val, mods) -> do
--                if val == eventKeyVal e && mods == eventModifier e
--                    then return False
--                    else do
--                        write prevEventRef (eventKeyVal e, eventModifier e)
--                        writeChan output e
--                        return True
--    onKeyRelease w $ \e -> do
--        writeIORef prevEventRef Nothing
--        writeChan output e
--        return True
--    return output
--    where
--        write r x = writeIORef r $ Just x
--

mapEvents :: [(Char, Unit.Unit)] -> Chan Key -> Chan (Double, P.Event) -> IO ()
mapEvents us iChan oChan = do
    i <- readChan iChan
    case i of
        Char c -> do
            case lookup c us of
                Nothing -> return ()
                Just u  -> do
                            t <- utcr
                            writeChan oChan (t, mkEvent u)
        _ -> return ()
    mapEvents us iChan oChan

main_gui db us' = do
    let us = keyChars `zip` cycle us'

    events <- newChan
    playerInput <- newChan
    keyboardMouseCallback $= Just (keyboardMouseHandler events)
    
    forkIO $ mapEvents us events playerInput
    forkIO $ playSampler playerInput
 
    mainLoop

main_ :: [String] -> IO ()
main_ [dbDir, pattern] = do
    db <- DB.open dbDir
    let us = DB.query (DB.pathMatch pattern) db
    main_gui db us

keyboardMouseHandler :: Chan Key -> KeyboardMouseCallback
keyboardMouseHandler _ (Char '\27') Down _ _ = safeExitWith ExitSuccess
keyboardMouseHandler c e Down _ _            = writeChan c e
keyboardMouseHandler _ _             _   _ _ = return ()

safeExitWith :: ExitCode -> IO a
safeExitWith code = do
    -- gma <- get gameModeActive
    -- when gma leaveGameMode
    exitWith code

render :: DisplayCallback
render = do
   -- clear screen and depth buffer
   clear [ ColorBuffer, DepthBuffer ]
   loadIdentity

setupProjection :: ReshapeCallback
setupProjection (Size width height) = do
   -- don't want a divide by zero
   let h = max 1 height
   -- reset the viewport to new dimensions
   viewport $= (Position 0 0, Size width h)
   -- set projection matrix as the current matrix
   matrixMode $= Projection
   -- reset projection matrix
   loadIdentity

   -- calculate aspect ratio of window
   perspective 52 (fromIntegral width / fromIntegral h) 1 1000

   -- set modelview matrix
   matrixMode $= Modelview 0
   -- reset modelview matrix
   loadIdentity

--fullscreenMode :: Options -> IO ()
--fullscreenMode opts = do
--   let addCapability c = maybe id (\x -> (Where' c IsEqualTo x :))
--   gameModeCapabilities $=
--      (addCapability GameModeWidth (Just (windowWidth  opts)) .
--       addCapability GameModeHeight (Just (windowHeight opts)) .
--       addCapability GameModeBitsPerPlane (bpp opts) .
--       addCapability GameModeRefreshRate (refreshRate opts)) []
--   enterGameMode
--   maybeWin <- get currentWindow
--   if isJust maybeWin
--      then cursor $= None
--      else do
--         hPutStr stderr "Could not enter fullscreen mode, using windowed mode\n"
--         windowedMode (opts { useFullscreen = False } )

windowedMode :: IO ()
windowedMode = do
    initialWindowSize $= Size (fromIntegral 200) (fromIntegral 200)
    createWindow "Fucking Hell"
    return ()

main :: IO ()
main = do
    -- Setup the basic GLUT stuff
    (_, args) <- getArgsAndInitialize

    initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
    windowedMode

    -- Register the event callback functions
    displayCallback $= do { render ; swapBuffers }
    reshapeCallback $= Just setupProjection
    -- idleCallback $= Just (do prepare state; postRedisplay Nothing)

    perWindowKeyRepeat $= PerWindowKeyRepeatOff

    main_ args

-- main :: IO ()
-- main = do
--     [dbDir] <- getArgs
--     db <- DB.open dbDir
--     c <- newChan
--     forkIO $ playSampler c
--     GUI.main (\p -> patternFromFile db p >>= handlePattern c)
