module Sound.SC3.Server.Transport where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (join)
import Data.Unique
import Sound.OpenSoundControl (Datum(..), OSC(..), Transport(..))
import Sound.OpenSoundControl.Time (Time(..))
import Sound.SC3.Server.State as State

data STMTransport t = STMTransport t (TChan OSC) (TChan OSC)

newSTMTransport :: Transport t => t -> IO (STMTransport t)
newSTMTransport t = do
    recvChan <- newTChanIO
    sendChan <- newTChanIO
    forkIO $ recvLoop recvChan
    forkIO $ sendLoop sendChan
    return (STMTransport t recvChan sendChan)
    where
        -- TODO: exception handling: terminate loop when handle is closed
        recvLoop c = recv t >>= atomically . writeTChan c >> recvLoop c
        sendLoop c = atomically (readTChan c) >>= send t >> sendLoop c

instance Transport t => Transport (STMTransport t) where
   send  (STMTransport t _ _) = send t
   recv  (STMTransport _ c _) = atomically (readTChan c)
   close (STMTransport t _ _) = close t

sendSTM :: STMTransport t -> OSC -> STM ()
sendSTM (STMTransport _ _ wc) = writeTChan wc

dupSTMTransport :: STMTransport t -> STM (STMTransport t)
dupSTMTransport (STMTransport t rc wc) = do
    rc' <- dupTChan rc
    return $ STMTransport t rc' wc

-- Repeat action until function does not give Nothing when applied to result.
untilM :: Monad m => (a -> Maybe b) -> m a -> m b
untilM f act = recurse
    where g p = let q = f p in case q of { Nothing -> recurse
                                         ; Just r -> return r }
          recurse = act >>= g

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: Transport t => STMTransport t -> (OSC -> Maybe a) -> STM a
waitFor t@(STMTransport _ c _) = flip untilM (readTChan c)

-- Does the OSC message have the specified address.
has_address :: String -> OSC -> Bool
has_address x (Message y _) = x == y
has_address _ _ = False

wait :: Transport t => STMTransport t -> String -> STM OSC
wait t s = waitFor t (\o -> if has_address s o then Just o else Nothing)

type Connection t = (State, STMTransport t)

appendOSC :: OSC -> OSC -> OSC
appendOSC m1@(Message _ _) m2@(Message _ _)  = Bundle (NTPi 1) [m1, m2]
appendOSC m@(Message _ _)     (Bundle t xs)  = Bundle t        (m:xs)
appendOSC   (Bundle t xs)   m@(Message _ _)  = Bundle t        (xs++[m])
appendOSC   (Bundle t xs1)    (Bundle _ xs2) = Bundle t        (xs1++xs2)

sync :: Transport t => Connection t -> OSC -> STM ()
sync (state, t@(STMTransport _ rc wc)) osc = do
    i <- State.alloc (State.syncId state)
    t' <- dupSTMTransport t
    sendSTM t' (appendOSC osc (Message "/sync" [Int i]))
    waitFor t' (synced i)
    where
        synced i (Message "/synced" [Int j]) | j == i = Just ()
        synced _ _                                    = Nothing
