{-# LANGUAGE GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             ExistentialQuantification,
             FlexibleInstances,
             FunctionalDependencies,
             UndecidableInstances #-} 

-- | This module provides a *very* basic support for processes with message queues.  It was built using channels and MVars.
module Control.Concurrent.Process (
-- * Types
    ReceiverT, Handle, Process, 
-- * Functions
-- ** Process creation / destruction
    makeProcess, runHere, spawn, kill,
-- ** Message passing
    self, sendTo, recv, poll, recvIn, sendRecv
-- ** Notifications
  , Listener
  , addListener, listenTo, notifyListeners, notify
  , io
) where

import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.CatchIO
import Data.Monoid
import Control.Concurrent

-- | A Process handle.  It's returned on process creation and should be used
-- | afterwards to send messages to it
data Handle i o = forall o' . PH { chan      :: Chan i
                                 , thread    :: ThreadId
                                 , listeners :: MVar [Listener o] }

data Listener i = forall o . Listener (Handle i o)

-- | The /ReceiverT/ generic type.
-- 
-- [@r@] the type of things the process will receive
-- 
-- [@m@] the monad in which it will run
-- 
-- [@a@] the classic monad parameter
newtype ReceiverT i o m a = RT { internalReader :: ReaderT (Handle i o) m a }
    deriving (Monad, MonadIO, MonadTrans, MonadCatchIO)

-- | /Process/ are receivers that run in the IO Monad
type Process i o = ReceiverT i o IO

-- | /sendTo/ lets you send a message to a running process. Usage:
-- @
--      sendTo processHandle message
-- @
sendTo :: MonadIO m => Handle i o -- ^ The receiver process handle 
        -> i                      -- ^ The message to send
        -> m ()
sendTo ph = liftIO . writeChan (chan ph)

-- | /kill/ lets you *brutally* terminate a running process. Usage:
-- @
--      kill processHandle
-- @
kill :: MonadIO m => Handle i o -- ^ The handle of process to kill
        -> m ()
kill = liftIO . killThread . thread

-- | /recv/ lets you receive a message in a running process (it's a blocking receive). Usage:
-- @
--      message <- recv
-- @
recv :: MonadIO m => ReceiverT i o m i
recv = RT $ ask >>= liftIO . readChan . chan

-- | Just like /recv/ but only returns a message when one has arrived. Usage:
-- @
--      maybeMessage <- poll
-- @
poll :: MonadIO m => ReceiverT i o m (Maybe i)
poll = RT $ do
    c <- chan `fmap` ask
    b <- liftIO $ isEmptyChan c
    if b
        then return Nothing
        else liftIO $ readChan c >>= return . Just

-- | Just like /recv/ but with a timeout parameter. Usage:
-- @
--      maybeMessage <- recvIn 10
-- @
recvIn :: MonadIO m => Int    -- ^ milliseconds to wait until timeout
        -> ReceiverT i o m (Maybe i)
recvIn ms = RT $
    do
            ch <- ask >>= return . chan
            liftIO $ do
                        tmp <- newEmptyMVar
                        timer <- if ms > 0
                                    then forkIO $ do
                                                    let its = [1..10] :: [Int]
                                                    forM_ its $ \_ -> threadDelay $ ms * 100
                                                    putMVar tmp Nothing
                                    else forkIO $ putMVar tmp Nothing
                        runner <- forkIO $ readChan ch >>= putMVar tmp . Just
                        res <- takeMVar tmp
                        killThread timer
                        killThread runner
                        return res

-- | /sendRecv/ is just a syntactic sugar for:
-- @
--      sendTo h a >> recv
-- @ 
sendRecv :: MonadIO m => Handle i o -- ^ The receiver process handle
          -> i                      -- ^ The message to send
          -> ReceiverT i o m i      -- ^ The process where this action is run will wait until it receives something
sendRecv h a = sendTo h a >> recv 

-- Basic listener support

addListener :: Handle i o -> Handle o o' -> IO ()
addListener (PH _ _ mv) b = modifyMVar_ mv $ \ls -> return $ (Listener b) : ls

listenTo :: Handle o o' -> Handle i o -> IO ()
listenTo = flip addListener

notifyListeners :: MonadIO m => Handle i o -> o -> m ()
notifyListeners (PH _ _ ls) o = liftIO (readMVar ls >>= mapM_ (\(Listener h) -> sendTo h o))

notify :: MonadIO m => o -> ReceiverT i o m ()
notify o = RT $ ask >>= flip notifyListeners o

-- | /spawn/ starts a process and returns its handle. Usage:
-- @
--      handle <- spawn process
-- @
spawn :: MonadIO m => Process i o k       -- ^ The process to be run
        -> m (Handle i o)                 -- ^ The handle for that process
spawn p = liftIO $ do
    pChan <- newChan
    mVar <- newMVar []
    pThread <- forkIO $ do
        t <- myThreadId
        _ <- runReaderT (internalReader p) $ PH pChan t mVar
        return ()
    return $ PH pChan pThread mVar

-- | /runHere/ executes process code in the current environment. Usage:
-- @
--      result <- runHere process
-- @
runHere :: MonadIO m => Process i o t   -- ^ The process to be run
         -> m t                         -- ^ It's returned as an action
runHere p = liftIO $ do
    c <- newChan
    t <- myThreadId
    m <- newMVar []
    runReaderT (internalReader p) $ PH c t m

-- | /self/ returns the handle of the current process. Usage:
-- @
--      handle <- self
-- @
self :: Monad m => ReceiverT i o m (Handle i o)
self = RT ask

-- | /makeProcess/ builds a process from a code that generates an IO action. Usage:
-- @
--      process <- makeProcess evalFunction receiver
-- @ 
makeProcess :: (m t -> IO s) -> ReceiverT i o m t -> Process i o s
makeProcess f (RT a) = RT (mapReaderT f a)

instance MonadState s m => MonadState s (ReceiverT i o m) where
    get = lift get
    put = lift . put

instance MonadReader i m => MonadReader i (ReceiverT i o m) where
    ask = lift ask
    local = onInner . local 

instance (Monoid i, MonadWriter i m) => MonadWriter i (ReceiverT i o m) where
    tell = lift . tell
    listen = onInner listen
    pass = onInner pass

instance MonadError e m => MonadError e (ReceiverT i o m) where
    throwError = lift . throwError
    catchError (RT a) h = RT $ a `catchError` (\e -> internalReader $ h e)

onInner :: (m a -> m b) -> ReceiverT i o m a -> ReceiverT i o m b
onInner f (RT m) = RT $ mapReaderT f m

io :: (MonadIO m) => IO a -> m a
io = liftIO