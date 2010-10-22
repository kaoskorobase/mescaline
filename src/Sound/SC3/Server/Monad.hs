{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad (
  -- *Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , lift
  , liftIO
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
import           Control.Concurrent.MVar
import           Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks, lift)
import           Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
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

newtype ServerT m a = ServerT (ReaderT Connection m a)
    deriving (Functor, Monad, MonadReader Connection, MonadIO, MonadTrans)

type Server = ServerT IO

type Allocator a = Accessor State a

liftConn :: MonadIO m => (Connection -> IO a) -> ServerT m a
liftConn f = ask >>= \c -> liftIO (f c)

liftState :: MonadIO m => (State -> a) -> ServerT m a
liftState f = asks C.state >>= liftIO . readMVar >>= return . f

runServerT :: ServerT m a -> Connection -> m a
runServerT (ServerT r) = runReaderT r

runServer :: Server a -> Connection -> IO a
runServer = runServerT

rootNode :: MonadIO m => ServerT m NodeId
rootNode = liftState State.rootNode


nodeId :: Allocator NodeIdAllocator
nodeId = State.nodeId

bufferId :: Allocator BufferIdAllocator
bufferId = State.bufferId

busId :: Rate -> Allocator BusIdAllocator
busId AR = State.audioBusId
busId KR = State.controlBusId
busId r  = error ("No bus allocator for rate " ++ show r)


alloc :: (IdAllocator i a, MonadIO m) => Allocator a -> ServerT m i
alloc a = asks C.state >>= liftIO . IOState.alloc a

allocMany :: (IdAllocator i a, MonadIO m) => Allocator a -> Int -> ServerT m [i]
allocMany a n = asks C.state >>= liftIO . flip (IOState.allocMany a) n

allocRange :: (RangeAllocator i a, MonadIO m) => Allocator a -> Int -> ServerT m (Range i)
allocRange a n = asks C.state >>= liftIO . flip (IOState.allocRange a) n

fork :: (MonadIO m) => ServerT IO () -> ServerT m ThreadId
fork = liftConn . flip C.fork . runServerT

send :: (MonadIO m) => OSC -> ServerT m ()
send = liftConn . flip C.send

-- communicate :: ServerT (Consumer a) -> ServerT a
-- communicate a = do
--     c <- ask
--     liftIO $ Conn.communicate c (runServerT a c)

waitFor :: (MonadIO m) => (OSC -> Maybe a) -> ServerT m a
waitFor = liftConn . flip C.waitFor

-- | Wait for an OSC message matching a specific address.
wait :: (MonadIO m) => String -> ServerT m OSC
wait = liftConn . flip C.wait

sync :: (MonadIO m) => OSC -> ServerT m ()
sync = liftConn . flip C.sync

unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = liftConn C.unsafeSync
