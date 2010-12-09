{-# LANGUAGE FlexibleContexts #-}
module Mescaline.Pattern.AST.Library (
  -- *Structure
    Pattern
  , patch
  -- *Scalar patterns
  , Scalar
  , value
  -- **Unary and binary function application
  , UnaryFunc(..)
  , BinaryFunc(..)
  , map
  , zip
  , min
  , max
  -- **Numeric functions
  , truncateP
  , roundP
  , ceilingP
  , floorP
  , clip
  , wrap
  , fold
  -- **Scalar conversions
  , ampdb
  , dbamp
  -- **Random functions
  , rand
  , randi
  , exprand
  , gaussian
  , brown
  -- *Boolean patterns
  , Boolean
  -- **Comparisons of scalars
  , (|==|)
  , (|>|)
  , (|>=|)
  , (|<|)
  , (|<=|)
  -- *Coordinates
  , Coord
  , coord
  , polar
  , x
  , y
  -- *Regions
  , center
  , radius
  -- *Event patterns
  , Event
  -- **Event generators
  , rest
  , closest
  , region
  -- **Event accessors
  , Field(..)
  , get
  , set
  -- ***Feature field accessors
  , fSpec
  , fPower
  , fFreq
  -- **Event modifiers
  , mapf
  , zipf
  , fzip
  , add
  , multiply
  -- **Event filters
  , filter
  , player
  , sequencer
  , envelope
  -- *Stream functions
  , cycle
  -- , constant, c
  , replicate
  , times
  , take
  , only
  , restrict
  , gimme
  , once, o
  -- **Event streams
  , takeDur
  , par
  -- **List patterns
  , seq, seq1
  , ser, ser1
  , choose, choose1
  , chooseNew, chooseNew1
  -- *Bindings
  , bind
  -- *Constants
  , Limit(..)
  -- *Debugging
  , trace
) where

import Mescaline.Pattern.AST
import Prelude hiding ( cycle, filter, min, map, max, replicate, seq, take, zip )
import qualified Prelude as P

-- | Repeat a pattern indefinitely.
cycle :: Stream Pattern a => Pattern a -> Pattern a
cycle = streamI SF_Cycle

-- | Constant signal.
-- constant, c :: Double -> Pattern Scalar
-- constant = cycle . value
-- c = constant

-- | Repeat a pattern n times.
--
-- @repeat n p@
replicate :: Stream Pattern a => Int -> Pattern a -> Pattern a
replicate n = streamI (SF_Replicate n)

-- | Repeat a pattern n times.
--
-- This is the same as 'replicate' with the arguments flipped in order to allow infix application:
--
-- @p \`times\` 4@
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
-- @p \`only\` 4@
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
-- @p \`gimme\` 4@
gimme :: Stream Pattern a => Pattern a -> Int -> Pattern a
gimme = flip restrict

-- | Take the first value of a pattern.
once :: Stream Pattern a => Pattern a -> Pattern a
once = take 1

-- | Take the first value of a pattern.
--
-- Short for 'once'.
o :: Stream Pattern a => Pattern a -> Pattern a
o = once

list1 :: (List Pattern a, Stream Pattern a) =>
    (Pattern Scalar -> [Pattern a] -> Pattern a)
 -> Pattern Scalar -> [Pattern a] -> Pattern a
list1 f n = f n . fmap once

-- | Enumerate a list of patterns sequentially, n times.
--
-- @seq n ps@
--
-- * @n@ Number of repeats.
--
-- * @ps@ List of patterns.
seq :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
seq = listI Enum_Seq

-- | Enumerate a list of patterns sequentially, n times.
--
-- Similar to 'seq' but applies 'once' to each of the patterns, so that
-- sequences of scalars can be written more conveniently:
--
-- @seq1 (once 4) [0.5, 0.6, 0.8, 1.3]@
seq1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
seq1 = list1 seq

-- | Enumerate a list of patterns sequentially, returning n items.
--
-- @ser n ps@
--
-- * @n@ Number of items to return.
--
-- * @ps@ List of patterns.
ser :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
ser = listI Enum_Ser

-- | Enumerate a list of patterns sequentially, returning n items.
--
-- Similar to 'ser' but applies 'once' to each of the patterns, so that
-- sequences of scalars can be written more conveniently:
--
-- @ser1 (once 7) [0.5, 0.6, 0.8, 1.3]@
ser1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
ser1 = list1 ser

-- | Returns one item from the list at random for each repeat. 
--
-- @choose n ps@
--
-- * @n@ Number of repeats.
--
-- * @ps@ List of patterns.
choose :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
choose = listI Enum_Rand

-- | Returns one item from the list at random for each repeat. 
--
-- Similar to 'choose' but applies 'once' to each of the patterns, so that
-- sequences of scalars can be written more conveniently:
--
-- @choose1 (once 7) [seq1 (once 1) [0 1], 0.6, 0.8, 1.3]@
choose1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
choose1 = list1 choose

-- | Returns one item from the list at random for each repeat. 
--
-- Similar to 'choose' but doesn't return the same item twice in a row.
chooseNew :: List Pattern a => Pattern Scalar -> [Pattern a] -> Pattern a
chooseNew  = listI Enum_RandX

-- | Returns one item from the list at random for each repeat. 
--
-- Similar to 'chooseNew' but but applies 'once' to each of the patterns.
chooseNew1 :: (List Pattern a, Stream Pattern a) =>
    Pattern Scalar -> [Pattern a] -> Pattern a
chooseNew1 = list1 chooseNew

-- | Return the smaller of the arguments.
--
-- @min a b@
min :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
min = liftAST2 (S_zip F_min)

-- | Return the bigger of the arguments.
--
-- @max a b@
max :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
max = liftAST2 (S_zip F_max)

-- | Truncate scalar towards -Infinity.
truncateP :: Pattern Scalar -> Pattern Scalar
truncateP = liftAST (S_map F_truncate)

-- | Round scalar to the closest integer.
roundP :: Pattern Scalar -> Pattern Scalar
roundP = liftAST (S_map F_round)

-- | Return the next integer bigger than a scalar.
ceilingP :: Pattern Scalar -> Pattern Scalar
ceilingP = liftAST (S_map F_ceiling)

-- | Truncate scalar towards zero.
floorP :: Pattern Scalar -> Pattern Scalar
floorP = liftAST (S_map F_floor)

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

-- | Generate integer random value in [min,max[.
--
-- @randi min max@
--
-- * @min@ Minimum value (inclusive).
--
-- * @max@ Maximum value (exclusive).
randi :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
randi l h = truncateP (rand l h)

-- | Map a unary function to a field.
mapf :: UnaryFunc -> Field -> Pattern Event -> Pattern Event
mapf uf f p = bind p $ \e -> set f (map uf (get f e)) e

-- | Combine a scalar with a field value.
zipf :: BinaryFunc -> Pattern Scalar -> Field -> Pattern Event -> Pattern Event
zipf bf s f p = bind p $ \e -> set f (zip bf s (get f e)) e

-- | Combine a field value with a scalar.
fzip :: BinaryFunc -> Field -> Pattern Scalar -> Pattern Event -> Pattern Event
fzip bf f s p = bind p $ \e -> set f (zip bf (get f e) s) e

-- | Add a scalar to a field value.
add :: Field -> Pattern Scalar -> Pattern Event -> Pattern Event
add = fzip F_add

-- | Multiply a field value by a scalar.
multiply :: Field -> Pattern Scalar -> Pattern Event -> Pattern Event
multiply = fzip F_multiply

-- | Spectral feature at index 0 or 1.
fSpec :: Int -> Field
fSpec i = Feature 0 (P.max 0 (P.min i 1))

-- | Power feature in dB.
fPower :: Field
fPower = Feature 1 0

-- | Fundamental frequency feature in Hz.
fFreq :: Field
fFreq = Feature 2 0

-- | Filter out events that don't have the cursor set.
--
-- @stepPlayer rowInc colInc pattern@
--
-- * @rowInc@ Cursor row increment.
--
-- * @colInc@ Cursor column increment.
--
-- * @pattern@ The pattern to filter.
player :: Pattern Scalar -> Pattern Scalar -> Pattern Event -> Pattern Event
player ri ci e = bind (step ri ci e) $ \e' -> filter (get CursorValue e' |>| 0) e'

-- | Given a tick increment in seconds filter the event pattern argument so
-- that only cursor values greater than zero produce events.
--
-- @sequencer tick pattern@
--
-- * @tick@ Tick increment in seconds.
--
-- * @pattern@ The pattern to filter.
sequencer :: Pattern Scalar -> Pattern Event -> Pattern Event
sequencer tick = player 0 1 . set Delta tick
    -- bind (step 0 1 (set Delta tick e)) $
    --     \e' -> filter (get CursorValue e' |>| 0) e'

-- | Set envelope attack and release time.
--
-- @envelope attackTime releaseTime e@
envelope :: Pattern Scalar -> Pattern Scalar -> Pattern Event -> Pattern Event
envelope a r = set AttackTime a . set ReleaseTime r
