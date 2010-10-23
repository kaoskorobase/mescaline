module Mescaline.Synth.Pattern.Sequencer (
    Cursor(..)
  , position
  , Sequencer
  , cons
  , cursors
  , mapCursor
  , rows
  , cols
  , cursors
  , lookup
  , lookupCursor
  , alter
  , insert
  , delete
  , clear
  , assocs
  , active
) where

import           Control.Arrow (first)
import           Control.Monad (join)
import           Data.Accessor
import           Data.Maybe (maybeToList)
import qualified Data.IntMap as Map
import           Prelude hiding (lookup)

-- | Two dimensional cursor.
data Cursor = Cursor {
    row    :: Int
  , column :: Int
  } deriving (Eq, Show)

position :: Cursor -> (Int, Int)
position c = (row c, column c)

data TransportState = Stopped | Running deriving (Eq, Show)

-- | Matrix sequencer.
data Sequencer = Sequencer {
    rows    :: Int
  , cols    :: Int
  , matrix  :: Map.IntMap (Map.IntMap Double)
  , _cursors :: Map.IntMap Cursor
} deriving (Eq, Show)

cons :: Int -> Int -> [(Int, Cursor)] -> Sequencer
cons rows cols cursors = Sequencer rows cols Map.empty (Map.fromList cursors)

cursors :: Sequencer -> [(Int, Cursor)]
cursors = Map.assocs . _cursors

-- | Map a function to the cursor with id c.
mapCursor :: (Cursor -> Cursor) -> Int -> Sequencer -> Sequencer
mapCursor f c s = s { _cursors = Map.alter (fmap f) c (_cursors s) }

-- | Lookup the value in the matrix.
lookup :: Int -> Int -> Sequencer -> Maybe Double
lookup row col = join . fmap (Map.lookup row) . Map.lookup col . matrix

-- | Lookup a value at a cursor.
lookupCursor :: Cursor -> Sequencer -> Maybe Double
lookupCursor c = lookup (row c) (column c)

alter :: (Maybe Double -> Maybe Double) -> Int -> Int -> Sequencer -> Sequencer
alter f row col s | row >= rows s || col >= cols s = s
                  | otherwise =
                      s { matrix = Map.alter
                                       (maybe (Map.singleton row `fmap` f Nothing)
                                              (\m -> let m' = Map.alter f row m
                                                     in if Map.null m' then Nothing else Just m'))
                                        col (matrix s) }

insert :: Int -> Int -> Double -> Sequencer -> Sequencer
insert row col a = alter (const (Just a)) row col

delete :: Int -> Int -> Sequencer -> Sequencer
delete = alter (const Nothing)

clear :: Sequencer -> Sequencer
clear s = s { matrix = Map.empty }

-- | Get element values
assocs :: Sequencer -> [((Int,Int), Double)]
assocs s = concatMap (\(c,m) -> map (first (flip (,) c)) (Map.assocs m)) (Map.assocs (matrix s))

-- | Get elements at cursors.
active :: Sequencer -> [(Int, (Cursor, Double))]
active s = Map.assocs $ Map.mapMaybe (\c -> ((,) c) `fmap` lookupCursor c s) $ _cursors s
