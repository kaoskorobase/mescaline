module Mescaline.Control.Concurrent.Pool (
    Pool, Job,
    new,
    close,
    schedule
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad                 (join, when)
import Mescaline.Control.Concurrent

-- Thread pool.
data Pool = Pool {
    channel :: Chan Job,    -- ^ Job queue
    workers :: [Worker]     -- ^ Worker threads
}

-- | Action to be executed by the thread pool.
type Job = IO ()

-- | Worker thread with communication variable.
data Worker = Worker {
    mvar  :: MVar (),
    child :: Child
}

-- runAction :: Job -> IO ()
-- runAction a = forkChild a >>= waitForChild

-- | Fork a worker thread with index i, taking jobs off the queue c.
forkWorker :: Int -> Chan Job -> IO Worker
forkWorker i c = do
    mvar  <- newEmptyMVar
    child <- forkChild (go mvar)
    return (Worker mvar child)
    where
        go m = do
            quitReq <- isEmptyMVar m
            chanEmpty <- isEmptyChan c
            when (quitReq || not chanEmpty) $ do
                print ("running job in worker " ++ show i)
                -- readChan c >>= runAction
                join (readChan c)
                go m

-- | Create a thread pool with n threads.
new :: Int -> IO Pool
new n = do
    c <- newChan
    w <- mapM (flip forkWorker c) [0..n-1]
    return (Pool c w)

-- | Schedule a job to be executed by the pool.
schedule :: Pool -> Job -> IO ()
schedule p = writeChan (channel p)

-- Wait for all jobs being processed and close the pool.
close :: Pool -> IO ()
close p = do
    mapM_ (flip putMVar () . mvar) (workers p)
    mapM_ (waitForChild . child) (workers p)
