{-# LANGUAGE ExistentialQuantification #-}
module Sound.SC3.Server.Connection (
    Connection
  , state
  , transport
  , new
  , dup
  , fork
  , send
  , waitFor, wait, sync, unsafeSync
) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC
import           Sound.OpenSoundControl.Time (Time(..))

import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.BufferedTransport (BufferedTransport)
import qualified Sound.SC3.Server.BufferedTransport as T

data Connection = Connection {
    state     :: State,
    transport :: BufferedTransport
}

new :: Transport t => State -> t -> IO Connection
new s t = Connection s `fmap` T.new t
    
dup :: Connection -> IO Connection
dup (Connection s t) = Connection s `fmap` T.dup t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = dup c >>= forkIO . f

send :: Connection -> OSC -> IO ()
send conn = OSC.send (transport conn)

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: Connection -> (OSC -> Bool) -> IO OSC
waitFor = T.waitFor . transport

-- | Wait for an OSC message matching a specific address.
wait :: Connection -> String -> IO OSC
wait = T.wait . transport

syncWith :: Connection -> (OSC -> OSC) -> IO ()
syncWith conn f = do
    i  <- (atomically . State.alloc . State.syncId . state) conn
    send conn $ f $ Message "/sync" [Int i]
    waitFor conn (synced i)
    return ()
    where
        synced i (Message "/synced" [Int j]) = j == i
        synced _ _                           = False

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
