module Mescaline.Synth.Sequencer where

import           Data.Accessor
import           Data.Maybe (maybeToList)
import qualified Data.IntMap as Map
import Debug.Trace

data Matrix a = Matrix Int Int (Map.IntMap (Map.IntMap a)) deriving (Eq, Show)

data Cursor = Bar Int | Point Int Int deriving (Eq, Show)

data Sequencer a = Sequencer {
    rows :: Int
  , cols :: Int
  , matrix :: Map.IntMap (Map.IntMap a)
  , tick_ :: Double
  , cursor_ :: Cursor
} deriving (Eq, Show)

cons :: Int -> Int -> Double -> Cursor -> Sequencer a
cons rows cols tick cursor = Sequencer rows cols Map.empty tick cursor

tick :: Accessor (Sequencer a) Double
tick = accessor tick_ (\a r -> r { tick_ = a })

cursor :: Accessor (Sequencer a) Cursor
cursor = accessor cursor_ (\a r -> r { cursor_ = a })

alter :: (Maybe a -> Maybe a) -> Int -> Int -> Sequencer a -> Sequencer a
alter f row col s | row >= rows s || col >= cols s = s
                  | otherwise =
                      s { matrix = Map.alter
                                       (maybe (Map.singleton row `fmap` f Nothing)
                                              (\m -> let m' = Map.alter f row m
                                                     in if Map.null m' then Nothing else Just m'))
                                        col (matrix s) }

insert :: Int -> Int -> a -> Sequencer a -> Sequencer a
insert row col a = alter (const (Just a)) row col
-- insert row col a s | row >= rows s || col >= cols s = s
--                    | otherwise =
--                        s { matrix = Map.alter
--                                     (Just . maybe (Map.singleton row a) (Map.insert row a))
--                                     col (matrix s) }

delete :: Int -> Int -> Sequencer a -> Sequencer a
delete = alter (const Nothing)
-- delete row col s | row >= rows s || col >= cols s = s
--                  | otherwise =
--                      s { matrix = Map.alter
--                                     (maybe Nothing (Just . Map.delete row))
--                                     col (matrix s) }

toggle :: Int -> Int -> a -> Sequencer a -> Sequencer a
toggle row col a = alter f row col
    where
        f Nothing  = traceShow "toggle on"  $ Just a
        f (Just _) = traceShow "toggle off" $ Nothing

cursorPosition :: Cursor -> (Int, Int)
cursorPosition (Bar col)       = (0, col)
cursorPosition (Point row col) = (row, col)

cursorRow :: Cursor -> Int
cursorRow = fst . cursorPosition

cursorColumn :: Cursor -> Int
cursorColumn = snd . cursorPosition

-- | Return 'True' if the cursor is currently at index @(row, column)@.
isCursorAtIndex :: Sequencer a -> (Int, Int) -> Bool
isCursorAtIndex s (row, col) =
    case cursor_ s of
        Bar c_col         -> c_col == col
        Point c_row c_col -> c_row == row && c_col == col

-- | Return 'True' if there is an element at index @(row, column)@.
isElemAtIndex :: Sequencer a -> (Int, Int) -> Bool
isElemAtIndex s (row, col) = maybe False (maybe False (const True) . Map.lookup row) (Map.lookup col (matrix s))

-- | Get elements at cursor.
elemsAtCursor :: Sequencer a -> [a]
elemsAtCursor s =
    case cursor_ s of
        Bar col       -> maybe [] Map.elems (Map.lookup col (matrix s))
        Point row col -> maybe [] (maybeToList . Map.lookup row) (Map.lookup col (matrix s))

indicesAtCursor :: Sequencer a -> [(Int, Int)]
indicesAtCursor s =
    case cursor_ s of
        Bar col       -> map (flip (,) col) $ maybe [] Map.keys (Map.lookup col (matrix s))
        Point row col -> maybe [] (maybe [] (const [(row,col)]) . Map.lookup row) (Map.lookup col (matrix s))

class Algorithm a where
    step :: a -> Sequencer e -> Sequencer e

data Score = Score

instance Algorithm Score where
    step _ s = setVal cursor (Bar (succ col `mod` cols s)) s
        where col = cursorColumn (cursor_ s)
