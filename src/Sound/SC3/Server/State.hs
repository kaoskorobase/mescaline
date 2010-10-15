{-# LANGUAGE
    CPP
  , ExistentialQuantification
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses #-}

#include "Accessor.h"

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

data State = State {
   _options      :: ServerOptions
 , _syncId       :: IntAllocator
 , _nodeId       :: NodeIdAllocator
 , _bufferId     :: BufferIdAllocator
 , _controlBusId :: BusIdAllocator
 , _audioBusId   :: BusIdAllocator
 }

ACCESSOR(options,      _options,      State, ServerOptions)
ACCESSOR(syncId,       _syncId,       State, IntAllocator)
ACCESSOR(nodeId,       _nodeId,       State, NodeIdAllocator)
ACCESSOR(bufferId,     _bufferId,     State, BufferIdAllocator)
ACCESSOR(controlBusId, _controlBusId, State, BusIdAllocator)
ACCESSOR(audioBusId,   _audioBusId,   State, BusIdAllocator)

rootNode :: State -> NodeId
rootNode = const (NodeId 0)

new :: ServerOptions -> State
new os =
    State {
        _options      = os
      , _syncId       = sid
      , _nodeId       = nid
      , _bufferId     = bid
      , _controlBusId = cid
      , _audioBusId   = aid
    }
    where
        sid = IntAllocator      (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator Int     )
        nid = NodeIdAllocator   (SAlloc.cons $ Alloc.range 1000                        maxBound :: SimpleAllocator NodeId  )
        bid = BufferIdAllocator (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BufferId)
        cid = BusIdAllocator    (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BusId   )
        aid = BusIdAllocator    (SAlloc.cons $ Alloc.range (BusId numHardwareChannels) maxBound :: SimpleAllocator BusId   )
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os
