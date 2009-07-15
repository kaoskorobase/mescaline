{-# LANGUAGE
    ExistentialQuantification
  , RankNTypes
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}

module Sound.SC3.Server.State where

import           Control.Concurrent.STM             (STM, TMVar, newTMVarIO)

import           Sound.SC3                          (Rate(..))
import           Sound.SC3.Server.Allocator         (IdAllocator, SimpleAllocator)
import qualified Sound.SC3.Server.Allocator         as Alloc
import           Sound.SC3.Server.Process.Options   (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)

newtype NodeId   = NodeId   Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype BusId    = BusId    Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)
newtype BufferId = BufferId Int deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

type NodeIdAllocator   = SimpleAllocator NodeId
type BufferIdAllocator = SimpleAllocator BufferId
type BusIdAllocator    = SimpleAllocator BusId

data State t = State {
    rootNode          :: NodeId,
    transport         :: t,
    nodeIdAlloc       :: TMVar NodeIdAllocator,
    bufferIdAlloc     :: TMVar BufferIdAllocator,
    controlBusIdAlloc :: TMVar BusIdAllocator,
    audioBusIdAlloc   :: TMVar BusIdAllocator
}

-- $( nameDeriveAccessors ''State (Just . (++"_")) )

newState :: ServerOptions -> t -> IO (State t)
newState opts transport = do
        nodeIdAlloc         <- newTMVarIO (Alloc.newSimpleAllocator 1) -- FIXME
        bufferIdAlloc       <- newTMVarIO (Alloc.newSimpleAllocator 0)
        controlBusIdAlloc   <- newTMVarIO (Alloc.newSimpleAllocator 0)
        audioBusIdAlloc     <- newTMVarIO (Alloc.newSimpleAllocator (BusId numHardwareChannels))
        return $ State {
            rootNode            = NodeId 0
          , transport           = transport
          , nodeIdAlloc         = nodeIdAlloc
          , bufferIdAlloc       = bufferIdAlloc
          , controlBusIdAlloc   = controlBusIdAlloc
          , audioBusIdAlloc     = audioBusIdAlloc
        }
    where numHardwareChannels = numberOfInputBusChannels opts
                              + numberOfOutputBusChannels opts
