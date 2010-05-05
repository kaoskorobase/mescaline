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
  , allocRange
  -- *Synchronization
  , fork
  , send
  -- , communicate
  , waitFor
  , wait
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.MVar
import           Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks, lift)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Accessor
import           Sound.SC3 (Rate(..))
import           Sound.SC3.Server.Allocator (IdAllocator, RangeAllocator, Range)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
-- import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)
import           Sound.SC3.Server.State (BufferId, BufferIdAllocator, BusId, BusIdAllocator, NodeId, NodeIdAllocator, State)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.State.IO as IOState

import           Sound.OpenSoundControl (OSC)

newtype Server a = Server (ReaderT Connection IO a)
    deriving (Functor, Monad, MonadReader Connection, MonadIO)

type Allocator a = Accessor State a

liftSTM :: STM a -> Server a
liftSTM = liftIO . atomically

liftConnIO :: (Connection -> IO a) -> Server a
liftConnIO f = ask >>= liftIO . f

liftState :: (State -> a) -> Server a
liftState f = asks C.state >>= liftIO . readMVar >>= return . f

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
alloc a = asks C.state >>= liftIO . IOState.alloc a

allocMany :: IdAllocator i a => Allocator a -> Int -> Server [i]
allocMany a n = asks C.state >>= liftIO . flip (IOState.allocMany a) n

allocRange :: RangeAllocator i a => Allocator a -> Int -> Server (Range i)
allocRange a n = asks C.state >>= liftIO . flip (IOState.allocRange a) n

fork :: Server () -> Server ThreadId
fork = liftConnIO . flip C.fork . runServer

send :: OSC -> Server ()
send = liftConnIO . flip C.send

-- communicate :: Server (Consumer a) -> Server a
-- communicate a = do
--     c <- ask
--     liftIO $ Conn.communicate c (runServer a c)

waitFor :: (OSC -> Maybe a) -> Server a
waitFor = liftConnIO . flip C.waitFor

-- | Wait for an OSC message matching a specific address.
wait :: String -> Server OSC
wait = liftConnIO . flip C.wait

sync :: OSC -> Server ()
sync = liftConnIO . flip C.sync

unsafeSync :: Server ()
unsafeSync = liftConnIO C.unsafeSync
