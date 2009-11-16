{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , TemplateHaskell #-}

module Sound.SC3.Server.State (
    State
  , options
  , Allocator
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
  , alloc
  , allocMany
  , allocRange
) where

import           Control.Arrow (second)
import           Control.Concurrent.MVar
import           Control.Monad (liftM)
import           Sound.SC3.Server.Allocator (IdAllocator, RangeAllocator, Range)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.SimpleAllocator (SimpleAllocator)
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SAlloc
import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)

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

newtype Allocator a = Allocator (MVar a)

data State = State {
   options      :: ServerOptions
 , syncId       :: Allocator IntAllocator
 , nodeId       :: Allocator NodeIdAllocator
 , bufferId     :: Allocator BufferIdAllocator
 , controlBusId :: Allocator BusIdAllocator
 , audioBusId   :: Allocator BusIdAllocator
 }

-- $( nameDeriveAccessors ''State (Just . (++"_")) )

rootNode :: State -> NodeId
rootNode = const (NodeId 0)

new :: ServerOptions -> IO State
new os = do
        sid <- newAllocator $ IntAllocator      (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator Int     )
        nid <- newAllocator $ NodeIdAllocator   (SAlloc.cons $ Alloc.range 1000                        maxBound :: SimpleAllocator NodeId  )
        bid <- newAllocator $ BufferIdAllocator (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BufferId)
        cid <- newAllocator $ BusIdAllocator    (SAlloc.cons $ Alloc.range 0                           maxBound :: SimpleAllocator BusId   )
        aid <- newAllocator $ BusIdAllocator    (SAlloc.cons $ Alloc.range (BusId numHardwareChannels) maxBound :: SimpleAllocator BusId   )
        return $ State {
            options      = os
          , syncId       = sid
          , nodeId       = nid
          , bufferId     = bid
          , controlBusId = cid
          , audioBusId   = aid
        }
    where
        newAllocator = fmap Allocator . newMVar
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

errorToIO :: Either String a -> IO a
errorToIO (Left e)  = fail e -- TODO: error mechanism?
errorToIO (Right a) = return a

alloc :: IdAllocator i a => Allocator a -> IO i
alloc (Allocator a) = modifyMVar a (fmap swap . errorToIO . Alloc.alloc)

allocMany :: IdAllocator i a => Allocator a -> Int -> IO [i]
allocMany (Allocator a) n = modifyMVar a (fmap swap . errorToIO . Alloc.allocMany n)

allocRange :: RangeAllocator i a => Allocator a -> Int -> IO (Range i)
allocRange (Allocator a) n = modifyMVar a (fmap swap . errorToIO . Alloc.allocRange n)
