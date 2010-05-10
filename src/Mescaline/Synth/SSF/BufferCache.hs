{-# LANGUAGE Arrows #-}
module Mescaline.Synth.SSF.BufferCache (
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

import qualified Data.Set as Set

import           Control.Arrow
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Command (b_alloc)
import           Sound.SC3.Server.Command.Completion
import qualified Sound.SC3.Server.State as State
import           Mescaline.Synth.SSF as SF

data Buffer = Buffer {
    uid         :: State.BufferId
  , numChannels :: Int
  , numFrames   :: Int
} deriving (Eq, Ord, Show)

type BufferSet = Set.Set Buffer

data BufferCache = BufferCache {
    freeBuffers :: BufferSet
  , usedBuffers :: BufferSet
} deriving (Eq, Show)

data Alloc = Alloc {
    alloc_numChannels :: Int
  , alloc_numFrames   :: Int
} deriving (Eq, Show)

newEmpty :: BufferCache
newEmpty = BufferCache Set.empty Set.empty

bytesToFrames :: Int -> Int -> Int
bytesToFrames nc b = b `div` 4 `div` nc

allocFrames :: Int -> Int -> Alloc
allocFrames = Alloc

allocBytes :: Int -> Int -> Alloc
allocBytes nc = Alloc nc . bytesToFrames nc

matchBuffer :: Alloc -> Buffer -> Bool
matchBuffer (Alloc nc nf) b = nc == numChannels b && nf == numFrames b

insertBuffer :: BufferCache -> Buffer -> BufferCache
insertBuffer cache buf =
    BufferCache {
        usedBuffers = Set.insert buf (usedBuffers cache)
      , freeBuffers = Set.delete buf (freeBuffers cache)
    }

deleteBuffer :: BufferCache -> Buffer -> BufferCache
deleteBuffer cache buf =
    BufferCache {
        usedBuffers = Set.delete buf (usedBuffers cache)
      , freeBuffers = Set.insert buf (freeBuffers cache)
    }

newWith :: SF (Event [Alloc]) BufferCache
newWith = undefined
-- newWith as = do
--     cache <- newEmpty
--     mapM (\a -> do
--             bid <- S.alloc bufferId
--             let buf = Buffer bid (alloc_numChannels a) (alloc_numFrames a)
--             Set.insert buf (freeBuffers cache)
--             S.sync (b_alloc (fromIntegral bid) (numFrames buf) (numChannels buf)))
--         as
--     return cache

free :: SF (BufferCache, Event a) ()
free = undefined
-- free cache = do
--     b1 <- liftSTM $ readTVar $ usedBuffers cache
--     b2 <- liftSTM $ readTVar $ freeBuffers cache
--     mapM_
--         (\b -> S.sync (b_free (fromIntegral $ uid b)))
--         (Set.elems b1 ++ Set.elems b2)

-- allocBuffer :: (Buffer -> Maybe OSC) -> BufferCache -> BufferId -> Alloc -> (BufferCache, Buffer)
-- allocBuffer completion cache bid alloc =
--     Set.filter (matchBuffer alloc) (freeBuffers cache)
--         if Set.null fb
--             then do
--                 -- Allocate buffer id
--                 bid <- SF.alloc bufferId
--                 let buf = Buffer bid (alloc_numChannels alloc) (alloc_numFrames alloc)
--                     msg = maybe b_alloc (($) b_alloc') (completion buf)
--                 -- Allocate buffer
--                 S.sync (msg (fromIntegral bid) (numFrames buf) (numChannels buf))
--                 -- Insert into used
--                 liftSTM $ insertTVar (usedBuffers cache) buf
--                 return buf
-- 
--     let buf = Set.findMin fb
--     Set.delete buf (freeBuffers cache)
--     Set.insert buf (usedBuffers cache)
--     maybe (return ()) S.send (completion buf)
--     return buf

allocBuffer :: (Buffer -> Maybe OSC) -> SF (BufferCache, Event Alloc) (BufferCache, Event Buffer)
allocBuffer = undefined
-- allocBuffer completion = (arr fst &&& sample) `switch` uncurry doAlloc
--     where
--         doAlloc :: BufferCache -> Alloc -> SF BufferCache (BufferCache, Event Buffer)
--         doAlloc cache alloc =
--             let fb = Set.filter (matchBuffer alloc) (freeBuffers cache)
--             in
--             -- if Set.null fb
--             --     then do
--                     -- Allocate buffer id
--                         now alloc
--                     >>> SF.alloc State.bufferId
--                     >>> arr (fmap (\(alloc, bid) ->
--                             let buf = Buffer bid (alloc_numChannels alloc) (alloc_numFrames alloc)
--                                 msg = maybe b_alloc (($) b_alloc') (completion buf)
--                                         (fromIntegral bid) (numFrames buf) (numChannels buf)
--                                 cache' = insertBuffer cache buf
--                             in (msg, (cache', buf))))
--                     >>> arr split
--                     >>> (first send &&& second sync)
--                     >>> arr (snd.split)
--                     >>> first (hold cache)
--                     -- Allocate buffer
--                     -- S.sync (msg (fromIntegral bid) (numFrames buf) (numChannels buf))
--                     -- Insert into used
--                     -- liftSTM $ insertTVar (usedBuffers cache) buf
--                     -- return buf)
--                 -- else
--                 --     let buf = Set.findMin fb
--                 --         cache' = deleteBuffer cache buf
--                 --     maybe (constant ()) (now (completion buf) >>> send)
--                 --     constant (cache', return buf)

freeBuffer :: SF (BufferCache, Event Buffer) BufferCache
-- freeBuffer = (arr fst &&& sample) `switch` (constant . uncurry deleteBuffer)
freeBuffer = (arr fst &&& sample) >>> second (arr (fmap (constant . uncurry deleteBuffer))) >>> rswitch identity
