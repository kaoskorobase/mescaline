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

import           Control.Concurrent (ThreadId)

import           Foreign (void)

import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Connection (Consumer)
import qualified Sound.OpenSoundControl.Connection as C
import           Sound.OpenSoundControl.Time (Time(..))

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Iteratee as It
import           Sound.SC3.Server.Notification (done, synced)
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State

data Connection = Connection State C.Connection

state :: Connection -> State
state (Connection s _) = s

conn :: Connection -> C.Connection
conn (Connection _ c) = c

new :: Transport t => State -> t -> IO Connection
new s t = do
    c <- Connection s `fmap` C.new t
    initServer c
    return c

initServer :: Connection -> IO ()
initServer c = do
    communicate c $ do
        -- Turn on notification
        -- TODO: Make this configurable?
        send c (notify True)
        return (waitFor (done "notify"))
    return ()

close :: Connection -> IO ()
close = C.close . conn

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = C.fork (conn c) (f . Connection (state c))

send :: Connection -> OSC -> IO ()
send = C.send . conn

communicate :: Connection -> IO (Consumer a) -> IO a
communicate = C.communicate . conn

syncWith :: Connection -> (OSC -> OSC) -> IO ()
syncWith c f = void $ communicate c $ do
    i <- (State.alloc . State.syncId . state) c
    send c (f (Message "/sync" [Int i]))
    return $ waitFor (synced i)

appendOSC :: OSC -> OSC -> OSC
appendOSC m1@(Message _ _) m2@(Message _ _)  = Bundle (NTPi 1) [m1, m2]
appendOSC m@(Message _ _)     (Bundle t xs)  = Bundle t        (m:xs)
appendOSC   (Bundle t xs)   m@(Message _ _)  = Bundle t        (xs++[m])
appendOSC   (Bundle t xs1)    (Bundle _ xs2) = Bundle t        (xs1++xs2)

sync :: Connection -> OSC -> IO ()
sync c = syncWith c . appendOSC

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order.
unsafeSync :: Connection -> IO ()
unsafeSync c = syncWith c id
