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
import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (IdAllocator)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.SetAllocator (SetAllocator)
import qualified Sound.SC3.Server.Allocator.SetAllocator as SetAlloc
import           Sound.SC3.Server.Allocator.SimpleAllocator (SimpleAllocator)
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SAlloc
import           Sound.SC3.Server.Options (ServerOptions(..))

-- Hide the actual allocator
data IntAllocator = forall a . (IdAllocator Int a, NFData a) => IntAllocator !a

instance IdAllocator Int IntAllocator where
    alloc  (IntAllocator a) = liftM (second IntAllocator) $ Alloc.alloc a
    free i (IntAllocator a) = liftM         IntAllocator  $ Alloc.free i a

instance NFData IntAllocator where
    rnf (IntAllocator a) = rnf a `seq` ()

newtype NodeId       = NodeId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data NodeIdAllocator = forall a . (IdAllocator NodeId a, NFData a) => NodeIdAllocator !a

instance IdAllocator NodeId NodeIdAllocator where
    alloc  (NodeIdAllocator a) = liftM (second NodeIdAllocator) $ Alloc.alloc a
    free i (NodeIdAllocator a) = liftM         NodeIdAllocator  $ Alloc.free i a

instance NFData NodeIdAllocator where
    rnf (NodeIdAllocator a) = rnf a `seq` ()

newtype BufferId       = BufferId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data BufferIdAllocator = forall a . (IdAllocator BufferId a, NFData a) => BufferIdAllocator !a

instance IdAllocator BufferId BufferIdAllocator where
    alloc  (BufferIdAllocator a) = liftM (second BufferIdAllocator) $ Alloc.alloc a
    free i (BufferIdAllocator a) = liftM         BufferIdAllocator  $ Alloc.free i a

instance NFData BufferIdAllocator where
    rnf (BufferIdAllocator a) = rnf a `seq` ()

newtype BusId       = BusId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data BusIdAllocator = forall a . (IdAllocator BusId a, NFData a) => BusIdAllocator !a

instance IdAllocator BusId BusIdAllocator where
    alloc  (BusIdAllocator a) = liftM (second BusIdAllocator) $ Alloc.alloc a
    free i (BusIdAllocator a) = liftM         BusIdAllocator  $ Alloc.free i a

instance NFData BusIdAllocator where
    rnf (BusIdAllocator a) = rnf a `seq` ()

data State = State {
   _options      :: !ServerOptions
 , _syncId       :: !IntAllocator
 , _nodeId       :: !NodeIdAllocator
 , _bufferId     :: !BufferIdAllocator
 , _controlBusId :: !BusIdAllocator
 , _audioBusId   :: !BusIdAllocator
 }

instance NFData State where
    rnf (State x1 x2 x3 x4 x5 x6) =
            x1 `seq`
        rnf x2 `seq`
        rnf x3 `seq`
        rnf x4 `seq`
        rnf x5 `seq`
        rnf x6 `seq` ()

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
        sid = IntAllocator
                (SAlloc.cons :: SimpleAllocator Int)
        nid = NodeIdAllocator
                (SetAlloc.cons $ Alloc.range 1000 (1000 + fromIntegral (maxNumberOfNodes os)) :: SetAllocator NodeId)
        bid = BufferIdAllocator
                (SetAlloc.cons $ Alloc.range 0 (fromIntegral (numberOfSampleBuffers os - 1)) :: SetAllocator BufferId)
        cid = BusIdAllocator
                (SetAlloc.cons $ Alloc.range 0 (fromIntegral (numberOfControlBusChannels os - 1)) :: SetAllocator BusId)
        aid = BusIdAllocator
                (SetAlloc.cons $ Alloc.range
                    (fromIntegral numHardwareChannels)
                    (fromIntegral (numHardwareChannels + numberOfAudioBusChannels os)) :: SetAllocator BusId)
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os
