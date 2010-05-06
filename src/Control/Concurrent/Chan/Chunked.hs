module Control.Concurrent.Chan.Chunked (
    newChan
  , writeChan
  , readChan
  , readChanAvailable
  , unGetChan
  , isEmptyChan
  , getChanAvailable
  , writeList2Chan
) where

import           Control.Monad
import qualified Control.Concurrent.Chan as C
import           Control.Concurrent.MVar

-- |'Chan' is an abstract type representing an unbounded FIFO channel with a notion of the available number of items.
data Chan a = Chan (MVar Int) (C.Chan a)

-- |Build and returns a new instance of 'Chan'.
newChan :: IO (Chan a)
newChan = do
    m <- newEmptyMVar
    c <- C.newChan
    return $ Chan m c

-- |Write a value to a 'Chan'.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan m c) a = do
    n <- takeMVar m
    C.writeChan c a
    putMVar m (n+1)

-- |Read the next value from the 'Chan'.
readChan :: Chan a -> IO a
readChan (Chan m c) = do
    n <- takeMVar m
    a <- C.readChan c
    let n' = n - 1
    when (n' > 0) $ putMVar m n'
    return a

-- |Read all items that are available without blocking from the 'Chan'.
--
-- Blocks if the 'Chan' is empty.
readChanAvailable :: Chan a -> IO [a]
readChanAvailable (Chan m c) = do
    n <- takeMVar m
    a <- forM [1..n] $ const $ C.readChan c
    return a

-- |Put a data item back onto a channel, where it will be the next item read.
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan m c) a = do
    n <- takeMVar m
    C.unGetChan c a
    putMVar m (n+1)

-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan _ c) = C.isEmptyChan c

-- |Returns the number of items available in 'Chan'.
getChanAvailable :: Chan a -> IO Int
getChanAvailable (Chan m _) = readMVar m
 
-- |Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan (Chan m c) a = do
    n <- takeMVar m
    C.writeList2Chan c a
    putMVar m (n + length a)
