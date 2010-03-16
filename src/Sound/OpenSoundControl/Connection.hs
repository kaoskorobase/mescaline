{-# LANGUAGE ExistentialQuantification #-}
module Sound.OpenSoundControl.Connection (
    Connection
  , new
  , dup
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

data Connection = forall t . Transport t => Connection t (Maybe ThreadId) (Broadcast OSC)

type Consumer a = B.Consumer OSC a

broadcast :: Connection -> Broadcast OSC
broadcast (Connection _ _ d) = d

new :: Transport t => t -> IO Connection
new t = do
    d <- B.new
    r <- forkIO $ recvLoop d
    return $ Connection t (Just r) d
    where
        recvLoop d = OSC.recv t >>= return . flatten >>= (\l -> mapM_ print l >> return l) >>= B.broadcastList d >> recvLoop d
        flatten m@(Message _ _) = [m]
        flatten   (Bundle _ xs) = concatMap flatten xs

dup :: Connection -> IO Connection
dup (Connection t _ d) = fmap (Connection t Nothing) (B.dup d)

close :: Connection -> IO ()
close (Connection t r d) = do
    case r of
        Nothing -> return ()
        Just r -> killThread r
    B.close d
    OSC.close t

fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = dup c >>= forkIO . f

send :: Connection -> OSC -> IO ()
send (Connection t _ _) = OSC.send t

communicate :: Connection -> IO (Consumer a) -> IO a
communicate conn = B.consume (broadcast conn)
