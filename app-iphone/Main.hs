{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

import Control.Applicative
import Foreign.C.Types
import Foreign.Ptr
-- import Network
import System.IO
import Data.IORef
import Engine

foreign import ccall safe "openWindow" openWindow
    :: IO CInt

data ViewController_struct
type ViewController = Ptr ViewController_struct

data UITouch_struct
type UITouch = Ptr UITouch_struct

type ViewDidLoad = ViewController -> IO ()
foreign import ccall safe "wrapper" mkViewDidLoad :: ViewDidLoad -> IO (FunPtr ViewDidLoad)
foreign import ccall safe "setViewDidLoad" setViewDidLoad :: FunPtr ViewDidLoad -> IO ()

type Touches = ViewController -> UITouch -> IO ()
foreign import ccall safe "wrapper" mkTouches :: Touches -> IO (FunPtr Touches)
foreign import ccall safe "setTouchesBegan" setTouchesBegan :: ViewController -> FunPtr Touches -> IO ()
foreign import ccall safe "setTouchesMoved" setTouchesMoved :: ViewController -> FunPtr Touches -> IO ()
foreign import ccall safe "setTouchesEnded" setTouchesEnded :: ViewController -> FunPtr Touches -> IO ()

foreign import ccall safe "getTouchX" getTouchX :: ViewController -> UITouch -> IO CDouble
foreign import ccall safe "getTouchY" getTouchY :: ViewController -> UITouch -> IO CDouble
foreign import ccall safe "getTouchTapCount" getTouchTapCount :: UITouch -> IO CInt

foreign import ccall safe "erase" erase :: ViewController -> IO ()
foreign import ccall safe "beginImageContext" beginImageContext :: ViewController -> IO ()
foreign import ccall safe "endImageContext" endImageContext :: ViewController -> IO ()
foreign import ccall safe "drawLine" drawLine :: CDouble -> CDouble -> CDouble -> CDouble -> IO ()

connect :: ViewController -> UITouch -> IORef (Maybe (CDouble, CDouble)) -> IO ()
connect vc touch lastPoint = do
    lp <- readIORef lastPoint
    x1 <- getTouchX vc touch
    y1 <- (\y -> y - 20) <$> getTouchY vc touch
    case lp of
        Nothing -> return ()
        Just (x0, y0) -> do
            beginImageContext vc
            drawLine x0 y0 x1 y1
            endImageContext vc
    writeIORef lastPoint $ Just (x1, y1)

openLogger :: IO Handle
openLogger =
    {-
      do
        log <- connectTo "127.0.0.1" (PortNumber 8049)
        hSetBuffering log LineBuffering
        return log
    `catch` \exc -> do -}
        return stderr

main = do
    log <- openLogger
    hPutStrLn log "Haskell start"

    lastPoint <- newIORef Nothing

    tb <- mkTouches $ \vc touch -> do
        tapC <- getTouchTapCount touch
        hPutStrLn log $ "tapCount="++show tapC
        if tapC == 2
            then erase vc
            else connect vc touch lastPoint

    tm <- mkTouches $ \vc touch -> do
        connect vc touch lastPoint

    te <- mkTouches $ \vc touch -> do
        connect vc touch lastPoint
        writeIORef lastPoint Nothing

    vdl <- mkViewDidLoad $ \vc -> do
        hPutStrLn log $ show vc++" viewDidLoad called back"
        setTouchesBegan vc tb
        setTouchesMoved vc tm
        setTouchesEnded vc te
    setViewDidLoad vdl
    engine "/Users/sk/projects/mescaline/repos/test.db" "*" openWindow
    hPutStrLn log "Haskell exit"
    freeHaskellFunPtr vdl
    freeHaskellFunPtr tb
    freeHaskellFunPtr tm
    freeHaskellFunPtr te
    return ()
