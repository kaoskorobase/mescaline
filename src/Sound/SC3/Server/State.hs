{-# LANGUAGE
    ExistentialQuantification
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}

module Sound.SC3.Server.State where

import           Control.Concurrent.STM             (STM, TVar, newTVar, readTVar, writeTVar)

import           Sound.SC3.Server.Allocator         (IdAllocator, SimpleAllocator)
import qualified Sound.SC3.Server.Allocator         as Alloc
import           Sound.SC3.Server.Process.Options   (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)

newtype NodeId   = NodeId   Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype BusId    = BusId    Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype BufferId = BufferId Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

type IntIdAllocator    = SimpleAllocator Int
type NodeIdAllocator   = SimpleAllocator NodeId
type BufferIdAllocator = SimpleAllocator BufferId
type BusIdAllocator    = SimpleAllocator BusId

type Allocator a = TVar a

data State = State {
   options      :: ServerOptions
 , syncId       :: Allocator IntIdAllocator
 , nodeId       :: Allocator NodeIdAllocator
 , bufferId     :: Allocator BufferIdAllocator
 , controlBusId :: Allocator BusIdAllocator
 , audioBusId   :: Allocator BusIdAllocator
}

-- $( nameDeriveAccessors ''State (Just . (++"_")) )

rootNode :: State -> NodeId
rootNode = const (NodeId 0)

newState :: ServerOptions -> STM State
newState os = do
        sid <- newTVar (Alloc.newSimpleAllocator 0)
        nid <- newTVar (Alloc.newSimpleAllocator 1000) -- FIXME
        bid <- newTVar (Alloc.newSimpleAllocator 0)
        cid <- newTVar (Alloc.newSimpleAllocator 0)
        aid <- newTVar (Alloc.newSimpleAllocator (BusId numHardwareChannels))
        return $ State {
            options      = os
          , syncId       = sid
          , nodeId       = nid
          , bufferId     = bid
          , controlBusId = cid
          , audioBusId   = aid
        }
    where numHardwareChannels = numberOfInputBusChannels os
                              + numberOfOutputBusChannels os

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

modifyTVar :: TVar a -> (a -> STM (a, b)) -> STM b
modifyTVar var f = do
    (a', b) <- readTVar var >>= f
    writeTVar var a'
    return b

alloc :: IdAllocator i a => Allocator a -> STM i
alloc allocator = modifyTVar allocator (fmap swap . Alloc.alloc)

allocMany :: IdAllocator i a => Allocator a -> Int -> STM [i]
allocMany allocator n = modifyTVar allocator (fmap swap . Alloc.allocMany n)

allocConsecutive :: IdAllocator i a => Allocator a -> Int -> STM [i]
allocConsecutive allocator n = modifyTVar allocator (fmap swap . Alloc.allocConsecutive n)
