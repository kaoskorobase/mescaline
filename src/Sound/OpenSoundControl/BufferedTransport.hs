{-# LANGUAGE ExistentialQuantification #-}
module Sound.OpenSoundControl.BufferedTransport (
    BufferedTransport
  , new
  , dup
  , fork
  , waitFor
  , wait
) where

import Control.Concurrent
import Sound.OpenSoundControl (OSC(..), Transport(..), waitFor, wait)

data BufferedTransport = forall t . Transport t => BufferedTransport t (Chan OSC)

instance Transport BufferedTransport where
   send  (BufferedTransport t _) = send t
   recv  (BufferedTransport _ c) = readChan c
   close (BufferedTransport t _) = close t

new :: Transport t => t -> IO BufferedTransport
new t = do
    c <- newChan
    forkIO $ recvLoop c
    return $ BufferedTransport t c
    where
        -- TODO: exception handling: terminate loop when handle is closed
        recvLoop c = recv t >>= writeChan c >> recvLoop c

-- | Duplicate the transport so that subsequent reads don't affect the original transport.
dup :: BufferedTransport -> IO BufferedTransport
dup (BufferedTransport t c) = BufferedTransport t `fmap` dupChan c

-- | Fork a thread with a duplicate transport.
fork :: BufferedTransport -> (BufferedTransport -> IO ()) -> IO ThreadId
fork t f = dup t >>= forkIO . f
