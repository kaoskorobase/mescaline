{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Control.Concurrent.MVar.StrictModify (
    modifyMVar
  , modifyMVar_
) where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, block, catch, throw, unblock)
import Prelude hiding (catch)

modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar m io = block $ do
    a <- takeMVar m
    (!a', b) <- unblock (io a) `catch` \(e :: SomeException) ->
                    putMVar m a >> throw e
    putMVar m a'
    return b

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m f =
    modifyMVar m $ \a -> do
        a' <- f a
        return (a', ())
