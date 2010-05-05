module Sound.SC3.Server.State.IO (
    IOState
  , fromState
  , new
  , alloc
  , allocMany
  , allocRange
) where

import           Control.Arrow (second)
import           Control.Concurrent.MVar
import           Control.Monad (liftM)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (IdAllocator, RangeAllocator, Range)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.SimpleAllocator (SimpleAllocator)
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SAlloc
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)

type IOState = MVar State

fromState :: State -> IO IOState
fromState = newMVar

new :: ServerOptions -> IO IOState
new = fromState . State.new

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

errorToIO :: Either String a -> IO a
errorToIO (Left e)  = fail e -- TODO: error mechanism?
errorToIO (Right a) = return a

alloc :: IdAllocator i a => (Accessor State a) -> IOState -> IO i
alloc f s = modifyMVar s (\s -> fmap (swap . second (flip (setVal f) s)) . errorToIO . Alloc.alloc . getVal f $ s)

allocMany :: IdAllocator i a => (Accessor State a) -> IOState -> Int -> IO [i]
allocMany f s n = modifyMVar s (\s -> fmap (swap . second (flip (setVal f) s)) . errorToIO . Alloc.allocMany n . getVal f $ s)

allocRange :: RangeAllocator i a => (Accessor State a) -> IOState -> Int -> IO (Range i)
allocRange f s n = modifyMVar s (\s -> fmap (swap . second (flip (setVal f) s)) . errorToIO . Alloc.allocRange n . getVal f $ s)
