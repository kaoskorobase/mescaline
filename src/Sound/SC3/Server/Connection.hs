{-# LANGUAGE ExistentialQuantification #-}
module Sound.SC3.Server.Connection (
    Connection
  , state
  , new
  , close
  , fork
  , send
  , Consumer
  , communicate
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId, forkIO, killThread)

import           Foreign (void)

import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Time (Time(..))

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Broadcast (Broadcast)
import qualified Sound.SC3.Server.Broadcast as B
import           Sound.SC3.Server.Iteratee as It
import           Sound.SC3.Server.Notification (done, synced)
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State

data Connection = forall t . Transport t => Connection State t (Broadcast OSC) ThreadId

type Consumer a = B.Consumer OSC a

state :: Connection -> State
state (Connection s _ _ _) = s

broadcast :: Connection -> Broadcast OSC
broadcast (Connection _ _ d _) = d

new :: Transport t => State -> t -> IO Connection
new s t = do
    d <- B.new
    r <- forkIO $ recvLoop d
    let c = Connection s t d r
    initServer c
    return c
    where
        recvLoop d = OSC.recv t >>= return . flatten >>= B.broadcastList d >> recvLoop d
        flatten m@(Message _ _) = [m]
        flatten b@(Bundle _ xs) = concatMap flatten xs

initServer :: Connection -> IO ()
initServer c = do
    communicate c $ do
        -- Turn on notification
        -- TODO: Make this configurable?
        send c (notify True)
        return (waitFor (done "notify"))
    return ()

close :: Connection -> IO ()
close (Connection _ t d r) = do
    killThread r
    B.close d
    OSC.close t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = forkIO (f c)

send :: Connection -> OSC -> IO ()
send (Connection _ t _ _) = OSC.send t

communicate :: Connection -> IO (Consumer a) -> IO a
communicate conn = B.consume (broadcast conn)

syncWith :: Connection -> (OSC -> OSC) -> IO ()
syncWith conn f = void $ communicate conn $ do
    i <- (State.alloc . State.syncId . state) conn
    send conn (f (Message "/sync" [Int i]))
    return $ waitFor (synced i)

appendOSC :: OSC -> OSC -> OSC
appendOSC m1@(Message _ _) m2@(Message _ _)  = Bundle (NTPi 1) [m1, m2]
appendOSC m@(Message _ _)     (Bundle t xs)  = Bundle t        (m:xs)
appendOSC   (Bundle t xs)   m@(Message _ _)  = Bundle t        (xs++[m])
appendOSC   (Bundle t xs1)    (Bundle _ xs2) = Bundle t        (xs1++xs2)

sync :: Connection -> OSC -> IO ()
sync conn = syncWith conn . appendOSC

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order.
unsafeSync :: Connection -> IO ()
unsafeSync conn = syncWith conn id
