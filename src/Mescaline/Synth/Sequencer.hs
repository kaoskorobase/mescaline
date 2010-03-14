module Mescaline.Synth.Sequencer where

import           Data.Maybe (maybeToList)
import qualified Data.IntMap as Map

data Matrix a = Matrix Int Int (Map.IntMap (Map.IntMap a)) deriving (Eq, Show)

data Cursor = Bar Int | Point Int Int deriving (Eq, Show)

data Sequencer a = Sequencer {
    rows :: Int
  , cols :: Int
  , matrix :: Map.IntMap (Map.IntMap a)
  , cursor :: Cursor
} deriving (Eq, Show)

cons :: Int -> Int -> Cursor -> Sequencer a
cons rows cols = Sequencer rows cols Map.empty

insert :: Int -> Int -> a -> Sequencer a -> Sequencer a
insert row col a s | row >= rows s || col >= cols s = s
                   | otherwise =
                       s { matrix = Map.alter
                                    (Just . maybe (Map.singleton row a) (Map.insert row a))
                                    col (matrix s) }

delete :: Int -> Int -> Sequencer a -> Sequencer a
delete row col s | row >= rows s || col >= cols s = s
                 | otherwise =
                     s { matrix = Map.alter
                                    (maybe Nothing (Just . Map.delete row))
                                    col (matrix s) }

cursorPosition :: Cursor -> (Int, Int)
cursorPosition (Bar col)       = (0, col)
cursorPosition (Point row col) = (row, col)

cursorRow :: Cursor -> Int
cursorRow = fst . cursorPosition

cursorColumn :: Cursor -> Int
cursorColumn = snd . cursorPosition

-- | Set the cursor position.
setCursor :: Sequencer a -> Cursor -> Sequencer a
setCursor s c = s { cursor = c }

-- | Update the cursor position. No bounds check
moveCursor :: Sequencer a -> (Int, Int) -> Sequencer a
moveCursor s (drow, dcol) = s { cursor = newCursor }
    where
        newCursor = case cursor s of
                        Bar col       -> Bar (col+dcol)
                        Point row col -> Point (row+drow) (col+dcol)

-- | Return 'True' if the cursor is currently at index @(row, column)@.
isCursorAtIndex :: Sequencer a -> (Int, Int) -> Bool
isCursorAtIndex s (row, col) =
    case cursor s of
        Bar c_col         -> c_col == col
        Point c_row c_col -> c_row == row && c_col == col

-- | Return 'True' if there is an element at index @(row, column)@.
isElemAtIndex :: Sequencer a -> (Int, Int) -> Bool
isElemAtIndex s (row, col) = maybe False (maybe False (const True) . Map.lookup row) (Map.lookup col (matrix s))

-- | Get elements at cursor.
elemsAtCursor :: Sequencer a -> [a]
elemsAtCursor s =
    case cursor s of
        Bar col       -> maybe [] Map.elems (Map.lookup col (matrix s))
        Point row col -> maybe [] (maybeToList . Map.lookup row) (Map.lookup col (matrix s))

class Algorithm a where
    step :: a -> Sequencer e -> Sequencer e

data Score = Score

instance Algorithm Score where
    step _ s = s `setCursor` Bar (succ col `mod` cols s)
        where col = cursorColumn (cursor s)
