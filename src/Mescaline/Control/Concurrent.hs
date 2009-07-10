module Mescaline.Control.Concurrent (
    Child, forkChild, waitForChild, waitForChildren
) where

import Control.Concurrent
import Control.Exception (finally)

newtype Child = Child (MVar ())

forkChild :: IO () -> IO Child
forkChild io = do
    mvar <- newEmptyMVar
    forkIO (io `finally` putMVar mvar ())
    return (Child mvar)

waitForChild :: Child -> IO ()
waitForChild (Child mvar) = takeMVar mvar

waitForChildren :: [Child] -> IO ()
waitForChildren = mapM_ waitForChild
