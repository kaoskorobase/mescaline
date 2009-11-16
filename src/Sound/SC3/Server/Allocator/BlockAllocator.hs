module Sound.SC3.Server.Allocator.BlockAllocator where

import           Data.Map (Map)
import qualified Data.Map as Map
import Sound.SC3.Server.Allocator

data BufferAllocator i = BufferAllocator {
    freeList :: Map i (Interval i)     -- indexed by lowerBound
  , usedList :: Map i (Interval i)     -- ditto
  , bestList :: Map i [(Interval i)]   -- indexed by size
} deriving (Show)

bAllocN :: (Enum i, Num i, Ord i) => Int -> BufferAllocator i -> Either String ([i], BufferAllocator i)
bAllocN n a = 
    let (_, eq, gt) = Map.splitLookup (fromIntegral n) (bestList a)
    in case eq of
        -- Can satisfy request with perfect fit, a single interval left
        Just (i:[]) -> let fl = Map.delete (lowerBound i) (freeList a)
                           ul = Map.insert (lowerBound i) i (usedList a)
                           bl = Map.delete (size i) (bestList a)
                       in Right (toList i, BufferAllocator fl ul bl)
       -- Can satisfy request with perfect fit, some intervals left
        Just (i:is) -> let fl = Map.delete (lowerBound i) (freeList a)
                           ul = Map.insert (lowerBound i) i (usedList a)
                           bl = Map.insert (size i) is (bestList a)
                       in Right (toList i, BufferAllocator fl ul bl)
        -- Cannot find perfect fit, split next larger interval
        Nothing -> case minView (bestList a) of
                    Nothing     -> Left "Ran out of ids, sorry"
                    Just (i:[]) -> let 
