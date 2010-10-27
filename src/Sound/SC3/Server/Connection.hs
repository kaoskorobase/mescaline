{-# LANGUAGE ExistentialQuantification #-}
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

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Writer as Writer
-- import           Data.Iteratee.OSC as It
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import           Data.Unique
import           Foreign (void)

import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Monoid ()
import           Sound.OpenSoundControl.Time (Time(..))

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Notification (done, synced)
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.State.IO (IOState)
import qualified Sound.SC3.Server.State.IO as IOState

type ListenerId = Unique
type Listener = ListenerId -> OSC -> IO ()
type ListenerMap = Map.Map ListenerId (ListenerId, Listener)
data Connection = forall t . Transport t => Connection t IOState (MVar ListenerMap)

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
    withMVar (listeners c) $ Fold.mapM_ (\(uid, l) -> l uid osc)
    recvLoop c

new :: Transport t => State -> t -> IO Connection
new s t = do
    ios <- IOState.fromState s
    ls <- newMVar Map.empty
    let c = Connection t ios ls
    _ <- forkIO $ recvLoop c
    initServer c
    return c

close :: Connection -> IO ()
close (Connection t _ _) = OSC.close t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = forkIO (f c)

type Send a = Writer.Writer OSC a

send :: OSC -> Send ()
send = Writer.tell

addListener :: Listener -> Connection -> IO ListenerId
addListener l c = do
    uid <- newUnique
    modifyMVar_ (listeners c) (return . Map.insert uid (uid, l))
    return uid

removeListener :: ListenerId -> Connection -> IO ()
removeListener uid c = modifyMVar_ (listeners c) (return . Map.delete uid)

async :: Send () -> Connection -> IO ()
async s (Connection t _ _) = do
    let osc = Writer.execWriter s
    when (osc /= mempty) $ OSC.send t osc

syncWith :: Send () -> (OSC -> Maybe a) -> Connection -> IO a
syncWith s f c = do
    res <- newEmptyMVar
    uid <- addListener (action c res) c
    s `async` c
    a <- takeMVar res
    removeListener uid c
    return a
    where
        action conn res uid osc = do
            case f osc of
                Nothing -> return ()
                Just a  -> putMVar res a

-- | Wait for an OSC message matching a specific address.
syncAddress :: Send () -> String -> Connection -> IO OSC
syncAddress s a = s `syncWith` hasAddress a
    where
        hasAddress s m@(Message a _) = if s == a then Just m else Nothing
        hasAddress _ _               = Nothing

sync :: Send () -> Connection -> IO ()
sync s c = do
    i <- (IOState.alloc State.syncId . state) c
    _ <- (s >> send (Message "/sync" [Int i])) `syncWith` synced i $ c
    return ()

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Connection -> IO ()
unsafeSync = sync (return ())
