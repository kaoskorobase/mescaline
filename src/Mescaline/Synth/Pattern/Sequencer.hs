module Mescaline.Synth.Pattern.Sequencer (
    Cursor(..)
  , position
  , Sequencer
  , cons
  , empty
  , cursors
  , getCursor
  , setCursor
  , modifyCursor
  , resetCursors
  , rows
  , cols
  , lookup
  , lookupAtCursor
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

-- | Cursor id.
type CursorId = Int

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
    rows     :: Int
  , cols     :: Int
  , matrix   :: Map.IntMap (Map.IntMap Double)
  , _cursors :: Map.IntMap Cursor
} deriving (Eq, Show)

cons :: Int -> Int -> [(CursorId, Cursor)] -> Sequencer
cons rows cols cursors = Sequencer rows cols Map.empty (Map.fromList cursors)

empty :: Int -> Int -> Sequencer
empty rows cols = cons rows cols (map (\r -> (r, Cursor r 0)) [0..rows-1])

cursors :: Sequencer -> [(CursorId, Cursor)]
cursors = Map.assocs . _cursors

getCursor :: CursorId -> Sequencer -> Maybe Cursor
getCursor i = Map.lookup i . _cursors

-- | Set cursor with id i.
setCursor :: CursorId -> Cursor -> Sequencer -> Sequencer
setCursor i (Cursor r c) s =
    let r' = max 0 (min r ((rows s) - 1))
        c' = max 0 (min c ((cols s) - 1))
    in s { _cursors = Map.insert i (Cursor r' c') (_cursors s) }

-- | Map a function to the cursor with id c.
modifyCursor :: (Cursor -> Cursor) -> Int -> Sequencer -> Sequencer
modifyCursor f c s = s { _cursors = Map.alter (fmap f) c (_cursors s) }

-- | Reset cursors to default position.
resetCursors :: Sequencer -> Sequencer
resetCursors s = s { _cursors = Map.mapWithKey (\i _ -> Cursor i 0) (_cursors s) }

-- | Lookup the value in the matrix.
lookup :: Int -> Int -> Sequencer -> Maybe Double
lookup row col = join . fmap (Map.lookup row) . Map.lookup col . matrix

-- | Lookup a cursor.
lookupAtCursor :: Cursor -> Sequencer -> Maybe Double
lookupAtCursor c = lookup (row c) (column c)

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
active s = Map.assocs $ Map.mapMaybe (\c -> ((,) c) `fmap` lookupAtCursor c s) $ _cursors s
