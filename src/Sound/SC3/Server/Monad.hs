{-# LANGUAGE
    ExistentialQuantification
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}

module Sound.SC3.Server.Monad where

import           Control.Concurrent.STM             (STM, TMVar, atomically, newTMVarIO, putTMVar, takeTMVar)
import           Control.Monad.Reader               (MonadReader, MonadTrans, ReaderT(..), asks, lift)

import           Sound.SC3                          (Rate(..))
import           Sound.SC3.Server.Allocator         (IdAllocator, SimpleAllocator)
import qualified Sound.SC3.Server.Allocator         as Alloc
import           Sound.SC3.Server.Process.Options   (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)
import           Sound.SC3.Server.State             (BufferIdAllocator, BusIdAllocator, NodeId, NodeIdAllocator, State)
import qualified Sound.SC3.Server.State             as State

newtype Server t a = Server (ReaderT (State t) STM a)
    deriving (Functor, Monad, MonadReader (State t))

type Allocator a = forall t . State t -> TMVar a

runServer :: Server t a -> State t -> STM a
runServer (Server r) = runReaderT r

withServer :: ServerOptions -> t -> Server t a -> IO a
withServer opts transport m = State.newState opts transport >>= atomically . runServer m

rootNode :: Server t NodeId
rootNode = asks State.rootNode

transport :: Server t t
transport = asks State.transport

nodeId :: Allocator NodeIdAllocator
nodeId = State.nodeIdAlloc

bufferId :: Allocator BufferIdAllocator
bufferId = State.bufferIdAlloc

busId :: Rate -> Allocator BusIdAllocator
busId AR = State.audioBusIdAlloc
busId KR = State.controlBusIdAlloc
busId r  = error ("No bus allocator for rate " ++ show r)

modifyTMVar :: TMVar a -> (a -> STM (a, b)) -> STM b
modifyTMVar mvar f = do
    (a', b) <- takeTMVar mvar >>= f
    putTMVar mvar a'
    return b

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

liftSTM :: STM a -> Server t a
liftSTM = Server . lift

alloc :: IdAllocator i a => Allocator a -> Server t i
alloc allocator = do
    mvar <- asks allocator
    liftSTM $ modifyTMVar mvar (fmap swap . Alloc.alloc)

allocMany :: IdAllocator i a => Allocator a -> Int -> Server t [i]
allocMany allocator n = do
    mvar <- asks allocator
    liftSTM $ modifyTMVar mvar (fmap swap . Alloc.allocMany n)

allocConsecutive :: IdAllocator i a => Allocator a -> Int -> Server t [i]
allocConsecutive allocator n = do
    mvar <- asks allocator
    liftSTM $ modifyTMVar mvar (fmap swap . Alloc.allocConsecutive n)
