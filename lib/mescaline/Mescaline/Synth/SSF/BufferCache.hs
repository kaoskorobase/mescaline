{-# LANGUAGE Arrows #-}
module Mescaline.Synth.SSF.BufferCache (
    Buffer(..)
  , BufferCache
  , newEmpty
  , allocFrames
  , allocBytes
  , aquireBuffer
  , releaseBuffer
  , newWith
  , free
  , allocBuffer
  , freeBufferList
  , freeBuffer
) where

import qualified Data.Set as Set

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Command (b_alloc)
import           Sound.SC3.Server.Command.Completion
import qualified Sound.SC3.Server.State as State
import           Mescaline.Synth.SSF as SF
import           Prelude hiding ((.), id)

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

-- | Add a buffer to 'usedBuffers' and remove it from 'freeBuffers'.
insertBuffer :: BufferCache -> Buffer -> BufferCache
insertBuffer cache buf =
    BufferCache {
        usedBuffers = Set.insert buf (usedBuffers cache)
      , freeBuffers = Set.delete buf (freeBuffers cache)
    }

-- | Aquire a buffer from the cache.
aquireBuffer :: Alloc -> BufferCache -> Maybe (BufferCache, Buffer)
aquireBuffer alloc cache
    | Set.null fbs = Nothing
    | otherwise    = Just (cache', buf)
        where
            fbs    = Set.filter (matchBuffer alloc) (freeBuffers cache)
            buf    = Set.findMin fbs
            cache' = insertBuffer cache buf

-- | Release a buffer to the cache.
releaseBuffer :: BufferCache -> Buffer -> BufferCache
releaseBuffer cache buf =
    BufferCache {
        usedBuffers = Set.delete buf (usedBuffers cache)
      , freeBuffers = Set.insert buf (freeBuffers cache)
    }

-- Effectful interface

newWith :: SF (Event [Alloc]) BufferCache
newWith = pure newEmpty
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
free = pure ()
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

-- TODO: split this in pure part and initialization part for allocation of a fixed number of buffers
allocBuffer :: SF (BufferCache, Event (Alloc, Buffer -> Maybe OSC)) (BufferCache, Event Buffer)
allocBuffer = second never &&& (sample >>> arr (fmap (uncurry doAlloc))) >>> rSwitch id
    where
        -- doAlloc :: BufferCache -> Alloc -> SF (BufferCache, Event Buffer) (BufferCache, Event Buffer)
        doAlloc cache (alloc, completion)
            | Set.null fbs =
                    once ()
                >>> SF.alloc State.bufferId
                >>> arr (split . fmap (\((), bid) ->
                        let buf = Buffer bid (alloc_numChannels alloc) (alloc_numFrames alloc)
                            msg = maybe b_alloc (($) b_alloc') (completion buf)
                                    (fromIntegral bid) (numFrames buf) (numChannels buf)
                            cache' = insertBuffer cache buf
                        in (msg, (cache', buf))))
                >>> send *** sync
                >>> arr (split.snd)
                >>> first (hold cache)
            | otherwise    =
                let buf    = Set.findMin fbs
                    cache' = insertBuffer cache buf
                in
                    maybe never send_ (completion buf)
                >>> pure (cache', Event buf)
            where
                fbs = Set.filter (matchBuffer alloc) (freeBuffers cache)

freeBufferList :: SF (BufferCache, Event [Buffer]) BufferCache
freeBufferList = second (arr (event id (flip (foldl (flip ($))) . fmap (flip releaseBuffer)))) >>> arr (uncurry (flip ($)))

freeBuffer :: SF (BufferCache, Event Buffer) BufferCache
-- freeBuffer = (arr fst &&& sample) >>> second (arr (fmap (constant . uncurry deleteBuffer))) >>> rSwitch identity
freeBuffer = second (arr (fmap (:[]))) >>> freeBufferList
