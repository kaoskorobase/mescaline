-- | 'Chunked' channels, maybe 'non-blocking' is the better word.
module Control.Concurrent.Chan.Chunked (
    Chan
  , newChan
  , writeChan
  -- , readChan
  , readChanAvailable
  , isEmptyChan
  -- , unGetChan
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
    m <- newMVar 0
    c <- C.newChan
    return $ Chan m c

-- |Write a value to a 'Chan'.
writeChan :: Chan a -> a -> IO ()
writeChan (Chan m c) a =
    modifyMVar_ m $ \n -> do
        C.writeChan c a
        return (n+1)

-- |Read the next value from the 'Chan'.
-- NOTE: This is impossible to provide without causing a deadlock on the `count' MVar. So we only provide the non-blocking `readChanAvailable'.
-- readChan :: Chan a -> IO a
-- readChan (Chan m c) = do
--     n <- takeMVar m
--     a <- C.readChan c
--     let n' = n - 1
--     when (n' > 0) $ putMVar m n'
--     return a

-- |Read all items that are available without blocking from the 'Chan'.
--
-- Blocks if the 'Chan' is empty.
readChanAvailable :: Chan a -> IO [a]
readChanAvailable (Chan m c) =
    modifyMVar m $ \n -> do
        a <- replicateM n (C.readChan c)
        return (0, a)

-- |Put a data item back onto a channel, where it will be the next item read.
-- unGetChan :: Chan a -> a -> IO ()
-- unGetChan (Chan m c) a = do
--     n <- tryTakeMVar m
--     C.unGetChan c a
--     putMVar m (maybe 1 (+1) n)

-- |Returns the number of items available in 'Chan'.
getChanAvailable :: Chan a -> IO Int
getChanAvailable (Chan m _) = readMVar m
 
-- |Returns 'True' if the supplied 'Chan' is empty.
isEmptyChan :: Chan a -> IO Bool
isEmptyChan = liftM (/=0) . getChanAvailable

-- |Write an entire list of items to a 'Chan'.
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan (Chan m c) as =
    modifyMVar_ m $ \n -> do
        k <- foldM (\s a -> C.writeChan c a >> return (s+1)) 0 as
        return (n+k)
