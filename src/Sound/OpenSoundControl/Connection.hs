{-# LANGUAGE ExistentialQuantification #-}
module Sound.OpenSoundControl.Connection (
    Connection
  , new
  , close
  , fork
  , send
  , Consumer
  , communicate
) where

import           Control.Concurrent (ThreadId, forkIO, killThread)

import           Sound.OpenSoundControl (OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC

import           Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as B

data Connection = forall t . Transport t => Connection t (Broadcast OSC) ThreadId

type Consumer a = B.Consumer OSC a

broadcast :: Connection -> Broadcast OSC
broadcast (Connection _ d _) = d

new :: Transport t => t -> IO Connection
new t = do
    d <- B.new
    r <- forkIO $ recvLoop d
    return $ Connection t d r
    where
        recvLoop d = OSC.recv t >>= return . flatten >>= B.broadcastList d >> recvLoop d
        flatten m@(Message _ _) = [m]
        flatten   (Bundle _ xs) = concatMap flatten xs

close :: Connection -> IO ()
close (Connection t d r) = do
    killThread r
    B.close d
    OSC.close t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = forkIO (f c)

send :: Connection -> OSC -> IO ()
send (Connection t _ _) = OSC.send t

communicate :: Connection -> IO (Consumer a) -> IO a
communicate conn = B.consume (broadcast conn)
