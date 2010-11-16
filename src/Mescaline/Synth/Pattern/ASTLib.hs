{-# LANGUAGE FlexibleContexts #-}
module Mescaline.Synth.Pattern.ASTLib (
    module Mescaline.Synth.Pattern.AST
  -- *Stream patterns
  , cycle
  , constant, c
  , replicate
  , times
  , take
  , only
  , restrict
  , gimme
  , once, o
  -- *List patterns
  , seq, seq1
  , ser, ser1
  , choose, choose1
  , chooseNew, chooseNew1
  -- *Numeric functions
  , truncateP
  , roundP
  , ceilingP
  , floorP
  , clip
  , wrap
  , fold
  -- *Numeric utilities
  , ampdb
  , dbamp
  -- *Random functions
  , randi
  -- *Event filters
  , sequencer
) where

import Mescaline.Synth.Pattern.AST
import Prelude hiding ( (==), (>), (>=), (<), (<=)
                      , cycle, filter, map, replicate, seq, take, zip )
import qualified Prelude as P

-- | Repeat a pattern indefinitely.
cycle :: Stream Pattern a => Pattern a -> Pattern a
cycle = streamI SF_Cycle

-- | Constant signal.
constant, c :: Double -> Pattern Scalar
constant = cycle . value
c = constant

-- | Repeat a pattern n times.
--
-- @repeat n p@
replicate :: Stream Pattern a => Int -> Pattern a -> Pattern a
replicate n = streamI (SF_Replicate n)

-- | Repeat a pattern n times.
--
-- This is the same as 'replicate' with the arguments flipped in order to allow infix application:
--
-- @p `times` 4@
times :: Stream Pattern a => Pattern a -> Int -> Pattern a
times = flip replicate

-- | Take the first n values of a pattern.
--
-- If the pattern is shorter, a smaller number of values is returned.
take :: Stream Pattern a => Int -> Pattern a -> Pattern a
take n = streamI (SF_Take n)

-- | Take the n initial values of a pattern repeated infinitely.
--
-- This is the same as 'take' with the arguments flipped in order to allow infix application:
--
-- @p `only` 4@
only :: Stream Pattern a => Pattern a -> Int -> Pattern a
only = flip take

-- | Take the n initial values of a pattern repeated infinitely.
--
-- @restrict n p = cycle (take n p)@
restrict :: Stream Pattern a => Int -> Pattern a -> Pattern a
restrict n = take n . cycle

-- | Take the n initial values of a pattern repeated infinitely.
--
-- This is the same as 'restrict' with the arguments flipped in order to allow infix application:
--
-- @p `gimme` 4@
gimme :: Stream Pattern a => Pattern a -> Int -> Pattern a
gimme = flip restrict

-- | Take the first value of a pattern.
once, o :: Stream Pattern a => Pattern a -> Pattern a
once = take 1
o = once

list1 :: (List Pattern a, Stream Pattern a) =>
    (Pattern Scalar -> [Pattern a] -> Pattern a)
 -> Pattern Scalar -> [Pattern a] -> Pattern a
list1 f n = f n . fmap once

seq :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
seq = listI Enum_Seq

seq1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
seq1 = list1 seq

ser :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
ser = listI Enum_Ser

ser1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
ser1 = list1 ser

choose :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
choose = listI Enum_Rand

choose1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
choose1 = list1 choose

chooseNew :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
chooseNew  = listI Enum_RandX

chooseNew1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
chooseNew1 = list1 chooseNew

-- | Truncate scalar towards -Infinity.
truncateP :: Pattern Scalar -> Pattern Scalar
truncateP = liftAST (S_map UF_truncate)

-- | Round scalar to the closest integer.
roundP :: Pattern Scalar -> Pattern Scalar
roundP = liftAST (S_map UF_round)

-- | Return the next integer bigger than a scalar.
ceilingP :: Pattern Scalar -> Pattern Scalar
ceilingP = liftAST (S_map UF_ceiling)

-- | Truncate scalar towards zero.
floorP :: Pattern Scalar -> Pattern Scalar
floorP = liftAST (S_map UF_floor)

-- | Constrain a scalar to the interval [min,max].
--
-- @clip min max x@
clip :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
clip = limit Clip

-- | Wrap a scalar into the interval [min,max].
--
-- @wrap min max x@
wrap :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
wrap = limit Wrap

-- | Fold a scalar into the interval [min,max].
--
-- @fold min max x@
fold :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
fold = limit Fold

-- Convert a linear amplitude in [0,1] to decibels in [-inf,0].
ampdb :: Pattern Scalar -> Pattern Scalar
ampdb a = 20 * logBase 10 a

-- Convert decibels in [-inf,0] to a linear amplitude in [0,1].
dbamp :: Pattern Scalar -> Pattern Scalar
dbamp a = 10 ** (a * 0.05)

-- Generate integer random value in [min,max[.
--
-- @randi min max@
--
-- * @min@ Minimum value (inclusive).
--
-- * @max@ Maximum value (exclusive).
randi :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
randi l h = truncateP (rand l h)

sequencer :: Pattern Scalar -> Pattern Event -> Pattern Event
sequencer tick e =
    bind (step 0 1 (set Delta tick e)) $
        \e' -> filter (get CursorValue e' > 0) e'
