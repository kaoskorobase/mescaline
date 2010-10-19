module Mescaline.Synth.Pattern.Track (
    Cursor
  , Track
  , offset
  , range
  , cursor
  , cells
  , cons
  , insert
  , delete
  , clear
  , lookup
  , lookupCursor
) where

import           Data.Accessor
import qualified Data.IntMap as Map
import           Prelude hiding (length, lookup)

type Cursor = Int

data Track = Track {
    length  :: Int
  , cells   :: Map.IntMap Double
  , _offset :: Int
  , _range  :: Int
  , _cursor :: Int
  } deriving (Eq, Show)

cons :: Int -> Int -> Int -> Cursor -> Track
cons length offset range cursor = Track length Map.empty offset range cursor

offset :: Accessor Track Int
offset = accessor _offset (\a r -> r { _offset = max 0 (min a (length r - 1)) })

range :: Accessor Track Int
range = accessor _range (\a r -> r { _range = max (_offset r) (min a (length r - 1)) })

cursor :: Accessor Track Cursor
cursor = accessor _cursor (\a r -> r { _cursor = max (_offset r) (min a (_offset r + _range r)) })

insert :: Int -> Double -> Track -> Track
insert i v track
    | i < 0 || i >= length track = track
    | otherwise                  = track { cells = Map.insert i v (cells track) }

delete :: Int -> Track -> Track
delete i track = track { cells = Map.delete i (cells track) }

clear :: Track -> Track
clear track = track { cells = Map.empty }

lookup :: Int -> Track -> Maybe Double
lookup i track = Map.lookup i (cells track)

lookupCursor :: Track -> Maybe Double
lookupCursor track = lookup (_cursor track) track
