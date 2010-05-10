{-# LANGUAGE
    CPP
  , ExistentialQuantification
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses #-}

module Sound.SC3.Server.State (
    State
  , options
  , IntAllocator
  , NodeIdAllocator
  , BusIdAllocator
  , BufferIdAllocator
  , NodeId
  , BusId
  , BufferId
  , syncId
  , nodeId
  , bufferId
  , controlBusId
  , audioBusId
  , rootNode
  , new
  , Alloc.alloc
  , Alloc.allocMany
  , Alloc.allocRange
  , Alloc.free
) where

import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (IdAllocator, RangeAllocator)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.SimpleAllocator (SimpleAllocator)
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SAlloc
import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)

-- Hide the actual allocator
data IntAllocator = forall a . IdAllocator Int a => IntAllocator a

instance IdAllocator Int IntAllocator where
    alloc  (IntAllocator a) = liftM (second IntAllocator) $ Alloc.alloc a
    free i (IntAllocator a) = liftM         IntAllocator  $ Alloc.free i a

newtype NodeId = NodeId   Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
data NodeIdAllocator = forall a . IdAllocator NodeId a => NodeIdAllocator a

instance IdAllocator NodeId NodeIdAllocator where
    alloc  (NodeIdAllocator a) = liftM (second NodeIdAllocator) $ Alloc.alloc a
    free i (NodeIdAllocator a) = liftM         NodeIdAllocator  $ Alloc.free i a

newtype BufferId = BufferId Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
data BufferIdAllocator = forall a . IdAllocator BufferId a => BufferIdAllocator a

instance IdAllocator BufferId BufferIdAllocator where
    alloc  (BufferIdAllocator a) = liftM (second BufferIdAllocator) $ Alloc.alloc a
    free i (BufferIdAllocator a) = liftM         BufferIdAllocator  $ Alloc.free i a

newtype BusId = BusId    Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
data BusIdAllocator = forall a . IdAllocator BusId a => BusIdAllocator a

instance IdAllocator BusId BusIdAllocator where
    alloc  (BusIdAllocator a) = liftM (second BusIdAllocator) $ Alloc.alloc a
    free i (BusIdAllocator a) = liftM         BusIdAllocator  $ Alloc.free i a

#define ACCESSOR(N,F,T,V) \
    N = accessor F (\x a -> a { F = x }) :: Accessor T V

data State = State {
   options_      :: ServerOptions
 , syncId_       :: IntAllocator
 , nodeId_       :: NodeIdAllocator
 , bufferId_     :: BufferIdAllocator
 , controlBusId_ :: BusIdAllocator
 , audioBusId_   :: BusIdAllocator
 }

ACCESSOR(options, options_, State, ServerOptions)
ACCESSOR(syncId,  syncId_, State, IntAllocator)
ACCESSOR(nodeId,  nodeId_, State, NodeIdAllocator)
ACCESSOR(bufferId, bufferId_, State, BufferIdAllocator)
ACCESSOR(controlBusId, controlBusId_, State, BusIdAllocator)
ACCESSOR(audioBusId, audioBusId_, State, BusIdAllocator)

rootNode :: State -> NodeId
rootNode = const (NodeId 0)

new :: ServerOptions -> State
new os =
    State {
        options_      = os
      , syncId_       = sid
      , nodeId_       = nid
      , bufferId_     = bid
      , controlBusId_ = cid
      , audioBusId_   = aid
    }
    where
        sid = IntAllocator      (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator Int     )
        nid = NodeIdAllocator   (SAlloc.cons $ Alloc.range 1000                        maxBound :: SimpleAllocator NodeId  )
        bid = BufferIdAllocator (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BufferId)
        cid = BusIdAllocator    (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BusId   )
        aid = BusIdAllocator    (SAlloc.cons $ Alloc.range (BusId numHardwareChannels) maxBound :: SimpleAllocator BusId   )
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os
