{-# LANGUAGE FlexibleContexts #-}
module Sound.SC3.Server.State.IO (
    IOState
  , fromState
  , new
  , alloc
  , allocMany
  , free
  , allocRange
  , freeRange
) where

import           Control.Arrow (second)
import           Control.Failure (Failure)
-- import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Concurrent.MVar.Strict (MVar, modifyMVar, modifyMVar_, newMVar)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (AllocFailure, IdAllocator, RangeAllocator, Range)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Options (ServerOptions)

type IOState = MVar State

fromState :: State -> IO IOState
fromState = newMVar

new :: ServerOptions -> IO IOState
new = fromState . State.new

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

alloc :: (IdAllocator i a, Failure AllocFailure IO) => (Accessor State a) -> IOState -> IO i
alloc f ios = modifyMVar ios (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.alloc . getVal f $ s)

allocMany :: (IdAllocator i a, Failure AllocFailure IO)  => (Accessor State a) -> IOState -> Int -> IO [i]
allocMany f m n = modifyMVar m (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.allocMany n . getVal f $ s)

free :: IdAllocator i a => (Accessor State a) -> IOState -> i -> IO ()
free f ios i = modifyMVar_ ios $ \s -> do
    let a  = s ^. f
    a' <- Alloc.free i a
    return $ f ^= a' $ s

allocRange :: (RangeAllocator i a, Failure AllocFailure IO) => (Accessor State a) -> IOState -> Int -> IO (Range i)
allocRange f ios n = modifyMVar ios (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.allocRange n . getVal f $ s)

freeRange :: (RangeAllocator i a, Failure AllocFailure IO) => (Accessor State a) -> IOState -> Range i -> IO ()
freeRange f ios r = modifyMVar_ ios $ \s -> do
    let a  = s ^. f
    a' <- Alloc.freeRange r a
    return $ f ^= a' $ s
