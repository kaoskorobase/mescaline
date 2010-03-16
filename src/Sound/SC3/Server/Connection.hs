{-# LANGUAGE ExistentialQuantification #-}
module Sound.SC3.Server.Connection (
    Connection
  , state
  , new
  , dup
  , close
  , fork
  , send
  -- , Consumer
  -- , communicate
  , waitFor
  , wait
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId)
-- import           Data.Iteratee.OSC as It

import           Foreign (void)

import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC
-- import           Sound.OpenSoundControl.Connection (Consumer)
-- import qualified Sound.OpenSoundControl.Connection as C
import           Sound.OpenSoundControl.Time (Time(..))

import           Sound.SC3 (notify, dumpOSC, PrintLevel(..))
import qualified Sound.SC3.Server.BufferedTransport as C
import           Sound.SC3.Server.Notification (done, synced)
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State

data Connection = Connection State C.BufferedTransport

state :: Connection -> State
state (Connection s _) = s

conn :: Connection -> C.BufferedTransport
conn (Connection _ c) = c

initServer :: Connection -> IO ()
initServer c = do
    -- Turn on notification
    -- TODO: Make this configurable?
    send c $ Bundle immediately $ [notify True] -- ++ [dumpOSC TextPrinter]
    void $ c `waitFor` done "notify"

new :: Transport t => State -> t -> IO Connection
new s t = do
    c <- Connection s `fmap` C.new t
    initServer c
    return c

dup :: Connection -> IO Connection
dup (Connection s c) = fmap (Connection s) (C.dup c)

close :: Connection -> IO ()
close = OSC.close . conn

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = C.fork (conn c) (f . Connection (state c))

send :: Connection -> OSC -> IO ()
send = OSC.send . conn

waitFor :: Connection -> (OSC -> Maybe a) -> IO a
waitFor c = C.waitFor (conn c)

-- | Wait for an OSC message matching a specific address.
wait :: Connection -> String -> IO OSC
wait c = C.wait (conn c)

-- communicate :: Connection -> IO (Consumer a) -> IO a
-- communicate = C.communicate . conn

syncWith :: Connection -> (OSC -> OSC) -> IO ()
syncWith c f = do
    i <- (State.alloc . State.syncId . state) c
    send c (f (Message "/sync" [Int i]))
    void $ c `waitFor` synced i

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
