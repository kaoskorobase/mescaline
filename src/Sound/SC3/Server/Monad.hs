{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad (
  -- *Server Monad
    Server
  , runServer
  , lift
  , liftIO
  , liftSTM
  , rootNode
  -- *Allocators
  , BufferId
  , BusId
  , NodeId
  , nodeId
  , bufferId
  , busId
  , alloc
  , allocMany
  , allocConsecutive
  -- *Synchronization
  , fork
  , send
  , waitFor
  , wait
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad (join)
import           Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks, lift)
import           Control.Monad.Trans (MonadIO, liftIO)

import           Sound.SC3 (Rate(..))
import           Sound.SC3.Server.Allocator (IdAllocator)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as Conn
-- import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)
import           Sound.SC3.Server.State (BufferId, BufferIdAllocator, BusId, BusIdAllocator, NodeId, NodeIdAllocator, State)
import qualified Sound.SC3.Server.State as State

import           Sound.OpenSoundControl (OSC)

newtype Server a = Server (ReaderT Connection IO a)
    deriving (Functor, Monad, MonadReader Connection, MonadIO)

type Allocator a = State -> State.Allocator a


liftSTM :: STM a -> Server a
liftSTM = liftIO . atomically

liftConnIO :: (Connection -> IO a) -> Server a
liftConnIO f = ask >>= liftIO . f

liftState :: (State -> a) -> Server a
liftState f = asks Conn.state >>= return . f

liftStateSTM :: (State -> STM a) -> Server a
liftStateSTM = join . fmap liftSTM . liftState


runServer :: Server a -> Connection -> IO a
runServer (Server r) = runReaderT r

rootNode :: Server NodeId
rootNode = liftState State.rootNode


nodeId :: Allocator NodeIdAllocator
nodeId = State.nodeId

bufferId :: Allocator BufferIdAllocator
bufferId = State.bufferId

busId :: Rate -> Allocator BusIdAllocator
busId AR = State.audioBusId
busId KR = State.controlBusId
busId r  = error ("No bus allocator for rate " ++ show r)


alloc :: IdAllocator i a => Allocator a -> Server i
alloc a = liftStateSTM (State.alloc . a)

allocMany :: IdAllocator i a => Allocator a -> Int -> Server [i]
allocMany a n = liftStateSTM (flip State.allocMany n . a)

allocConsecutive :: IdAllocator i a => Allocator a -> Int -> Server [i]
allocConsecutive a n = liftStateSTM (flip State.allocConsecutive n . a)

fork :: Server () -> Server ThreadId
fork = liftConnIO . flip Conn.fork . runServer

send :: OSC -> Server ()
send = liftConnIO . flip Conn.send

waitFor :: (OSC -> Bool) -> Server OSC
waitFor = liftConnIO . flip Conn.waitFor

-- | Wait for an OSC message matching a specific address.
wait :: String -> Server OSC
wait = liftConnIO . flip Conn.wait

sync :: OSC -> Server ()
sync = liftConnIO . flip Conn.sync

unsafeSync :: Server ()
unsafeSync = liftConnIO Conn.unsafeSync
