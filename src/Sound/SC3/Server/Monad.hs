{-# LANGUAGE
    ExistentialQuantification
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}

module Sound.SC3.Server.Monad where

import           Control.Concurrent.STM             (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad.Reader               (MonadReader, MonadTrans, ReaderT(..), asks, lift)

import           Sound.SC3                          (Rate(..))
import           Sound.SC3.Server.Allocator         (IdAllocator, SimpleAllocator)
import qualified Sound.SC3.Server.Allocator         as Alloc
import           Sound.SC3.Server.Process.Options   (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)
import           Sound.SC3.Server.State             (BufferIdAllocator, BusIdAllocator, NodeId, NodeIdAllocator, State)
import qualified Sound.SC3.Server.State             as State

newtype Server a = Server (ReaderT State STM a)
    deriving (Functor, Monad, MonadReader State)

type Allocator a = State -> TVar a

runServer :: Server a -> State -> STM a
runServer (Server r) = runReaderT r

runServerIO :: Server a -> State -> IO a
runServerIO m = atomically . runServer m

rootNode :: Server NodeId
rootNode = asks State.rootNode

liftSTM :: STM a -> Server a
liftSTM = Server . lift

nodeId :: Allocator NodeIdAllocator
nodeId = State.nodeId

bufferId :: Allocator BufferIdAllocator
bufferId = State.bufferId

busId :: Rate -> Allocator BusIdAllocator
busId AR = State.audioBusId
busId KR = State.controlBusId
busId r  = error ("No bus allocator for rate " ++ show r)

modifyTVar :: TVar a -> (a -> STM (a, b)) -> STM b
modifyTVar var f = do
    (a', b) <- readTVar var >>= f
    writeTVar var a'
    return b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

alloc :: IdAllocator i a => Allocator a -> Server i
alloc allocator = do
    mvar <- asks allocator
    liftSTM $ modifyTVar mvar (fmap swap . Alloc.alloc)

allocMany :: IdAllocator i a => Allocator a -> Int -> Server [i]
allocMany allocator n = do
    mvar <- asks allocator
    liftSTM $ modifyTVar mvar (fmap swap . Alloc.allocMany n)

allocConsecutive :: IdAllocator i a => Allocator a -> Int -> Server [i]
allocConsecutive allocator n = do
    mvar <- asks allocator
    liftSTM $ modifyTVar mvar (fmap swap . Alloc.allocConsecutive n)
