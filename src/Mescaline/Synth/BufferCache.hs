module Mescaline.Synth.BufferCache (
    Buffer(..)
  , BufferCache
  , bufferSize
  , newEmpty
  , newWith
  , free
  , allocBuffer
  , freeBuffer
) where

import           Control.Concurrent.STM
import qualified Data.Set as Set

import           Sound.OpenSoundControl (OSC, Transport)

import           Sound.SC3 hiding (free, uid)
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection  as C
import qualified Sound.SC3.Server.State as State

data Buffer = Buffer {
    uid         :: State.BufferId,
    numChannels :: Int
} deriving (Eq, Ord, Show)

type BufferSet = Set.Set Buffer

data BufferCache = BufferCache {
    bufferSize  :: Int,
    freeBuffers :: TVar BufferSet,
    usedBuffers :: TVar BufferSet
}

insertTVar :: TVar BufferSet -> Buffer -> STM ()
insertTVar tv b = readTVar tv >>= writeTVar tv . Set.insert b

deleteTVar :: TVar BufferSet -> Buffer -> STM ()
deleteTVar tv b = readTVar tv >>= writeTVar tv . Set.delete b

newEmpty :: Int -> IO BufferCache
newEmpty bs = atomically $ do
    fb <- newTVar Set.empty
    ub <- newTVar Set.empty
    return (BufferCache bs fb ub)

bytesToFrames :: Int -> Int -> Int
bytesToFrames b nc = b `div` 4 `div` nc

bufferFrameSize :: BufferCache -> Int -> Int
bufferFrameSize b = bytesToFrames (bufferSize b)

newWith :: Transport t => Connection t -> Int -> [Int] -> IO BufferCache
newWith conn nb ncs = do
    cache <- newEmpty nb
    mapM (\nc -> do
            bid <- atomically $ (State.alloc (State.bufferId (C.state conn)) :: STM State.BufferId)
            let numFrames = bytesToFrames nb nc
                buf       = Buffer bid nc
            atomically $ insertTVar (freeBuffers cache) buf
            C.sync conn (b_alloc (fromIntegral bid) numFrames nc))
        ncs
    return cache

free :: Transport t => Connection t -> BufferCache -> IO ()
free conn cache = do
    b1 <- atomically $ readTVar $ usedBuffers cache
    b2 <- atomically $ readTVar $ freeBuffers cache
    mapM_
        (\b -> C.sync conn (b_free (fromIntegral $ uid b)))
        (Set.elems b1 ++ Set.elems b2)

allocBuffer :: Transport t => Connection t -> BufferCache -> Int -> (Buffer -> Maybe OSC) -> IO Buffer
allocBuffer conn cache nc completion = do
    fb <- Set.filter ((==) nc . numChannels) `fmap` (atomically $ readTVar $ freeBuffers cache)
    if Set.null fb
        then do
            -- Allocate buffer id
            bid <- atomically $ (State.alloc (State.bufferId $ C.state conn) :: STM State.BufferId)
            let buf   = Buffer bid nc 
                alloc = maybe b_alloc (($) b_alloc') (completion buf)
            -- Allocate buffer
            C.sync conn (alloc (fromIntegral bid) (bufferFrameSize cache nc) nc)
            -- Insert into used
            atomically $ insertTVar (usedBuffers cache) buf
            return buf
        else do
            let buf = Set.findMin fb
            atomically $ do
                deleteTVar (freeBuffers cache) buf
                insertTVar (usedBuffers cache) buf
            maybe (return ()) (C.send conn) (completion buf)
            return buf
        
freeBuffer :: Transport t => Connection t -> BufferCache -> Buffer -> IO ()
freeBuffer _ cache buf = atomically $ do
    deleteTVar (usedBuffers cache) buf
    insertTVar (freeBuffers cache) buf
