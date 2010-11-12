{-# LANGUAGE ExistentialQuantification
           , GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Connection (
    Connection
  , state
  , new
  , close
  , fork
  , Send
  , send
  , async
  , syncWith
  , syncAddress
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar
import           Control.Monad.Writer.Strict as Writer
import qualified Data.HashTable as Hash
import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Monoid ()

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Notification (done, synced)
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.State.IO (IOState)
import qualified Sound.SC3.Server.State.IO as IOState

type ListenerId  = Int
type Listener    = OSC -> IO ()
data ListenerMap = ListenerMap !(Hash.HashTable ListenerId Listener) !ListenerId
data Connection  = forall t . Transport t => Connection t IOState (MVar ListenerMap)

state :: Connection -> IOState
state (Connection _ s _) = s

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ _ l) = l

initServer :: Connection -> IO ()
initServer c = do
    _ <- send (Bundle immediately [notify True]) `syncWith` done "notify" $ c
    return ()

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t _ _) = do
    osc <- OSC.recv t
    withMVar (listeners c) (\(ListenerMap h _) -> mapM_ (\(_, l) -> l osc) =<< Hash.toList h)
    recvLoop c

new :: Transport t => State -> t -> IO Connection
new s t = do
    ios <- IOState.fromState s
    h  <- Hash.new (==) Hash.hashInt
    lm <- newMVar (ListenerMap h 0)
    let c = Connection t ios lm
    _ <- forkIO $ recvLoop c
    initServer c
    return c

close :: Connection -> IO ()
close (Connection t _ _) = OSC.close t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = forkIO (f c)

newtype Send a = Send { unSend :: Writer.Writer OSC a }
                    deriving (Functor, Monad, MonadWriter OSC)

send :: OSC -> Send ()
send = Send . Writer.tell

addListener :: Listener -> Connection -> IO ListenerId
addListener l c = modifyMVar
                    (listeners c) $
                    \(ListenerMap h lid) -> do
                        Hash.insert h lid l
                        return (ListenerMap h (lid+1), lid)

removeListener :: ListenerId -> Connection -> IO ()
removeListener uid c = modifyMVar_ (listeners c) (\lm@(ListenerMap h _) -> Hash.delete h uid >> return lm)

async :: Send () -> Connection -> IO ()
async s (Connection t _ _) = do
    let osc = Writer.execWriter (unSend s)
    OSC.send t osc

syncWith :: Send () -> (OSC -> Maybe a) -> Connection -> IO a
syncWith s f c = do
    res <- newEmptyMVar
    uid <- addListener (action res) c
    s `async` c
    a <- takeMVar res
    removeListener uid c
    return a
    where
        action res osc = do
            case f osc of
                Nothing -> return ()
                Just a  -> putMVar res a

-- | Wait for an OSC message matching a specific address.
syncAddress :: Send () -> String -> Connection -> IO OSC
syncAddress s a = s `syncWith` hasAddress
    where
        hasAddress m@(Message a' _) = if a == a' then Just m else Nothing
        hasAddress _                = Nothing

sync :: Send () -> Connection -> IO ()
sync s c = do
    i <- IOState.alloc State.syncId (state c)
    _ <- (s >> send (Message "/sync" [Int i])) `syncWith` synced i $ c
    IOState.free State.syncId (state c) i
    return ()

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Connection -> IO ()
unsafeSync = sync (return ())
