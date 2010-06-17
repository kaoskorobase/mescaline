module Mescaline.Synth.BufferCache (
    Buffer(..)
  , BufferCache
  , allocFrames
  , allocBytes
  , newEmpty
  , newWith
  , free
  , allocBuffer
  , freeBuffer
) where

import           Control.Concurrent.STM
import qualified Data.Set as Set

import           Sound.OpenSoundControl (OSC, Transport)

import           Sound.SC3 hiding (free)
import           Sound.SC3.Server.Command.Completion
-- import           Sound.SC3.Server.Connection (Connection)
-- import qualified Sound.SC3.Server.Connection  as C
import           Sound.SC3.Server.Monad as S
import qualified Sound.SC3.Server.State as State

data Buffer = Buffer {
    uid         :: State.BufferId
  , numChannels :: Int
  , numFrames   :: Int
} deriving (Eq, Ord, Show)

type BufferSet = Set.Set Buffer

data BufferCache = BufferCache {
    freeBuffers :: TVar BufferSet
  , usedBuffers :: TVar BufferSet
}

data Alloc = Alloc {
    alloc_numChannels :: Int
  , alloc_numFrames   :: Int
} deriving (Eq, Show)

insertTVar :: TVar BufferSet -> Buffer -> STM ()
insertTVar tv b = readTVar tv >>= writeTVar tv . Set.insert b

deleteTVar :: TVar BufferSet -> Buffer -> STM ()
deleteTVar tv b = readTVar tv >>= writeTVar tv . Set.delete b

newEmpty :: Server BufferCache
newEmpty = liftSTM $ do
    fb <- newTVar Set.empty
    ub <- newTVar Set.empty
    return (BufferCache fb ub)

bytesToFrames :: Int -> Int -> Int
bytesToFrames nc b = b `div` 4 `div` nc

allocFrames :: Int -> Int -> Alloc
allocFrames = Alloc

allocBytes :: Int -> Int -> Alloc
allocBytes nc = Alloc nc . bytesToFrames nc

newWith :: [Alloc] -> Server BufferCache
newWith as = do
    cache <- newEmpty
    mapM (\a -> do
            bid <- S.alloc bufferId
            let buf = Buffer bid (alloc_numChannels a) (alloc_numFrames a)
            liftSTM $ insertTVar (freeBuffers cache) buf
            S.sync (b_alloc (fromIntegral bid) (numFrames buf) (numChannels buf)))
        as
    return cache

free :: BufferCache -> Server ()
free cache = do
    b1 <- liftSTM $ readTVar $ usedBuffers cache
    b2 <- liftSTM $ readTVar $ freeBuffers cache
    mapM_
        (\b -> S.sync (b_free (fromIntegral $ uid b)))
        (Set.elems b1 ++ Set.elems b2)

matchBuffer :: Alloc -> Buffer -> Bool
matchBuffer (Alloc nc nf) b = nc == numChannels b && nf == numFrames b

allocBuffer :: BufferCache -> Alloc -> (Buffer -> Maybe OSC) -> Server Buffer
allocBuffer cache alloc completion = do
    fb <- Set.filter (matchBuffer alloc) `fmap` (liftSTM $ readTVar $ freeBuffers cache)
    if Set.null fb
        then do
            -- Allocate buffer id
            bid <- S.alloc bufferId
            let buf   = Buffer bid (alloc_numChannels alloc) (alloc_numFrames alloc)
                msg   = maybe b_alloc (($) b_alloc') (completion buf)
            -- Allocate buffer
            S.sync (msg (fromIntegral bid) (numFrames buf) (numChannels buf))
            -- Insert into used
            liftSTM $ insertTVar (usedBuffers cache) buf
            return buf
        else do
            let buf = Set.findMin fb
            liftSTM $ do
                deleteTVar (freeBuffers cache) buf
                insertTVar (usedBuffers cache) buf
            maybe (return ()) S.send (completion buf)
            return buf
        
freeBuffer :: BufferCache -> Buffer -> Server ()
freeBuffer cache buf = liftSTM $ do
    deleteTVar (usedBuffers cache) buf
    insertTVar (freeBuffers cache) buf
