module Mescaline.Synth.BufferCache (
    Alloc
  , allocBytes
  , allocFrames
  , Buffer(..)
  , fromAlloc
  , matchBuffer
  , BufferCache
  , freeBuffers
  , usedBuffers
  , empty
  , fromList
  , insertBuffer
  , allocBuffer
  , freeBuffer
) where

import           Mescaline.Synth.BufferCache.Alloc (Alloc, allocBytes, allocFrames)
import qualified Mescaline.Synth.BufferCache.Alloc as Alloc
import qualified Data.Set as Set
import qualified Sound.SC3.Server.State as State

data Buffer = Buffer {
    uid         :: State.BufferId
  , numChannels :: Int
  , numFrames   :: Int
} deriving (Eq, Ord, Show)

fromAlloc :: State.BufferId -> Alloc -> Buffer
fromAlloc bid alloc = Buffer bid (Alloc.numChannels alloc) (Alloc.numFrames alloc)

matchBuffer :: Alloc -> Buffer -> Bool
matchBuffer a b = Alloc.numChannels a == numChannels b
                    && Alloc.numFrames a == numFrames b

type BufferSet = Set.Set Buffer

data BufferCache = BufferCache {
    freeBuffers :: BufferSet
  , usedBuffers :: BufferSet
} deriving (Eq, Show)

empty :: BufferCache
empty = BufferCache Set.empty Set.empty

fromList :: [Buffer] -> BufferCache
fromList bs = BufferCache (Set.fromList bs) Set.empty

-- | Add a buffer to 'usedBuffers' and remove it from 'freeBuffers'.
insertBuffer :: BufferCache -> Buffer -> BufferCache
insertBuffer cache buf =
    BufferCache {
        usedBuffers = Set.insert buf (usedBuffers cache)
      , freeBuffers = Set.delete buf (freeBuffers cache)
    }

-- | Allocate a buffer from the cache.
allocBuffer :: BufferCache -> Alloc -> Maybe (BufferCache, Buffer)
allocBuffer cache alloc
    | Set.null fbs = Nothing
    | otherwise    = Just (cache', buf)
        where
            fbs    = Set.filter (matchBuffer alloc) (freeBuffers cache)
            buf    = Set.findMin fbs
            cache' = insertBuffer cache buf

-- | Free a buffer to the cache.
freeBuffer :: BufferCache -> Buffer -> BufferCache
freeBuffer cache buf =
    BufferCache {
        usedBuffers = Set.delete buf (usedBuffers cache)
      , freeBuffers = Set.insert buf (freeBuffers cache)
    }
