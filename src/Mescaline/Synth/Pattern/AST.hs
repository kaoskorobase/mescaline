{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern.AST where

import           Control.Monad
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Data.IntMap as Map
import           Prelude hiding (cycle, filter, map, seq, zip)

data UnaryFunc =
    F_abs
  | F_signum
  | F_negate
  | F_recip
  | F_truncate
  | F_round
  | F_ceiling
  | F_floor
  | F_exp
  | F_sqrt
  | F_log
  | F_sin
  | F_tan
  | F_cos
  | F_asin
  | F_atan
  | F_acos
  | F_sinh
  | F_tanh
  | F_cosh
  | F_asinh
  | F_atanh
  | F_acosh
  deriving (Eq, Read, Show)

data BinaryFunc =
    F_min
  | F_max
  | F_add
  | F_subtract
  | F_multiply
  | F_divide
  | F_power
  | F_logBase
  deriving (Eq, Read, Show)

data Comparison =
    Comp_eq
  | Comp_gt
  | Comp_geq
  | Comp_lt
  | Comp_leq
  deriving (Eq, Read, Show)

data StreamFunc =
    SF_Cycle
  | SF_Replicate Int
  | SF_Take Int
  deriving (Eq, Read, Show)
  
data Enumerator =
    Enum_Seq     -- ^Sequential
  | Enum_Ser     -- ^Serial
  | Enum_Rand    -- ^Random
  | Enum_RandX   -- ^Random without repetitions
  deriving (Eq, Read, Show)

data Field =
    Delta           -- ^Delta time to next event in seconds.
  | Cursor          -- ^Cursor id.
  | CursorValue     -- ^Value at this cursor.
  | Offset          -- ^Playback offset in seconds.
  | Duration        -- ^Duration of the event being played.
  | Level           -- ^Level between 0 and 1.
  | Pan             -- ^Panning coefficient in [-1;1].
  | Rate            -- ^Playback rate (> 0).
  | AttackTime      -- ^Envelope attack time in seconds.
  | ReleaseTime     -- ^Envelope release time in seconds.
  | SendLevel1      -- ^First send effect level.
  | SendLevel2      -- ^Second send effect level.
  | FXParam1        -- ^Parameter for first send effect.
  | FXParam2        -- ^Parameter for second send effect.
  | Feature Int Int -- ^Feature value.
  deriving (Eq, Read, Show)

-- | Interval boundary behavior.
data Limit =
    Clip -- ^Clip to interval.
  | Wrap -- ^Wrap to other end of interval.
  | Fold -- ^Fold back into interval.
  deriving (Eq, Read, Show)

-- | Construct an infinite constant value scalar pattern.
value :: Double -> Pattern Scalar
value = AST . return . S_value

-- | Bind the current value of a pattern to a function argument.
--
-- This function is needed to express value sharing (as opposed to expression
-- sharing) in the pattern language.
--
-- @bind (\<event pattern expression\>) (\\e -> set Delta (get Duration e) e)@
bind :: Bind Pattern a b => Pattern a -> (Pattern a -> Pattern b) -> Pattern b
bind = bindI

-- *Structure

-- | Apply a stream function to a pattern.
stream :: Stream Pattern a => StreamFunc -> Pattern a -> Pattern a   
stream = streamI

-- | Apply an enumerator function to a list of patterns.
list :: List Pattern a => Enumerator -> Pattern Scalar -> [Pattern a] -> Pattern a
list = listI

-- | Combine a list of event patterns by merging their event streams.
par :: [Pattern Event] -> Pattern Event
par a = AST $ liftM E_par (mapM unAST a)

-- *Scalars

-- | Map a unary function to a scalar.
map :: UnaryFunc -> Pattern Scalar -> Pattern Scalar
map = liftAST . S_map

-- | Combine two scalars with a binary function.
zip :: BinaryFunc -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
zip = liftAST2 . S_zip

-- | Apply an interval limit operation to a scalar.
limit :: Limit -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
limit l = liftAST3 (S_limit l)

-- *Comparisons

-- | Return True if a equals b.
--
-- @a |==| b@
(|==|) :: Pattern Scalar -> Pattern Scalar -> Pattern Boolean
(|==|) = liftAST2 (B_compare Comp_eq)

-- | Return True if a is greater than b.
--
-- @a |>| b@
(|>|) :: Pattern Scalar -> Pattern Scalar -> Pattern Boolean
(|>|) = liftAST2 (B_compare Comp_gt)

-- | Return True if a is greater than or equal to b.
--
-- @a |>=| b@
(|>=|) :: Pattern Scalar -> Pattern Scalar -> Pattern Boolean
(|>=|) = liftAST2 (B_compare Comp_geq)

-- | Return True if a is smaller than b.
--
-- @a |<| b@
(|<|) :: Pattern Scalar -> Pattern Scalar -> Pattern Boolean
(|<|) = liftAST2 (B_compare Comp_lt)

-- | Return True if a is smaller than or equal to b.
--
-- @a |<=| b@
(|<=|) :: Pattern Scalar -> Pattern Scalar -> Pattern Boolean
(|<=|) = liftAST2 (B_compare Comp_leq)

-- *Events

-- | Return the value of an event field.
--
-- @get Duration e@
get :: Field -> Pattern Event -> Pattern Scalar
get    = liftAST . S_get

-- | Set the value of an event field
--
-- @set Duration 0.1 e@
set :: Field -> Pattern Scalar -> Pattern Event -> Pattern Event
set    = liftAST2 . E_set

-- | Filter an event stream with the given boolean pattern.
--
-- Events for which the predicate returns True are replaced by rests.
--
-- @bind (\<event expression\>) (\\e -> filter (get Duration e |>| 0.5) e)@
filter :: Pattern Boolean -> Pattern Event -> Pattern Event
filter = liftAST2 E_filter

-- | Limit an event pattern to the first n seconds.
--
-- @takeDur 2.125 e@
takeDur :: Double -> Pattern Event -> Pattern Event
takeDur = liftAST . E_takeDur

-- *Coordinates

-- | Construct a coordinate pattern from a pair of scalar patterns, one for each axis.
--
-- @coord 0.5 (rand 0.4 0.6)@
coord :: Pattern Scalar -> Pattern Scalar -> Pattern Coord
coord     = liftAST2 C_coord

-- | Construct a coordinate pattern from a center coordinate, a radius and an angle in radians.
--
-- @polar (coord 0.5 0.5) 0.5 (pi/2)@
polar :: Pattern Coord -> Pattern Scalar -> Pattern Scalar -> Pattern Coord
polar = liftAST3 C_polar

-- | Retrieve the x value from a coordinate.
x :: Pattern Coord -> Pattern Scalar
x = liftAST S_x

-- | Retrieve the y value from a coordinate.
y :: Pattern Coord -> Pattern Scalar
y = liftAST S_y

-- *Regions

-- | Return the center of the specified feature space region.
center :: Int -> Pattern Coord
center = liftAST C_center . fromIntegral

-- | Return the radius of the specified feature space region.
radius :: Int -> Pattern Scalar
radius = liftAST S_radius . fromIntegral

-- | Select the unit closest to a given coordinate.
--
-- @closest cursor defaultDelta radius coord@
--
-- * @cursor@ Cursor id for the generated event.
--
-- * @defaultDelta@ Default delta time in case no unit is found.
--
-- * @radius@ Radius to restrict the search; if the found unit is outside the given radius around the coordinate, a rest is generated.
--
-- * @coord@ Coordinate for nearest neighbor search.
closest :: Int -> Pattern Scalar -> Pattern Scalar -> Pattern Coord -> Pattern Event
closest i = liftAST3 (E_closest i)

-- | Select a unit from a region according to the iterator.
--
-- @region cursor defaultDelta iterator@
--
-- * @cursor@ Cursor and region id for this event.
--
-- * @defaultDelta@ Default delta time in case the region is empty.
--
-- * @iterator@ Scalar pattern in the range [0;1[ that selects a unit from the region.
region :: Int -> Pattern Scalar -> Pattern Scalar -> Pattern Event
region i = liftAST2 (E_region i)

-- * Cursor

-- | Advance the cursor by a row and column increment as a side effect.
--
-- The event pattern argument is not modified.
step :: Pattern Scalar -> Pattern Scalar -> Pattern Event -> Pattern Event
step = liftAST3 E_step

-- *Randomness

-- | Generate white noise in a given range.
--
-- @rand min max@
--
-- * @min@ Minimum value.
--
-- * @max@ Maximum value.
rand :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
rand = liftAST2 S_rand

-- | Generate exponential noise in a given range.
--
-- @exprand min max@
--
-- * @min@ Minimum value.
--
-- * @max@ Maximum value.
exprand :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
exprand = liftAST2 S_exprand

-- | Generate gaussian noise with the given mean and variance.
--
-- @gaussian mean var@
--
-- * @mean@ Mean value.
--
-- * @var@ Variance, or standard deviation squared.
gaussian :: Pattern Scalar -> Pattern Scalar -> Pattern Scalar
gaussian = liftAST2 S_gaussian

-- | Generate brown noise in a given range with a given step size.
--
-- @brown limmit stepMin stepMax min max@
--
--  * @limit@ Interval boundary behavior.
--
--  * @stepMin@ Minimum step size.
--
--  * @stepMax@ Maximum step size.
--
--  * @min@ Minimum value.
--
--  * @max@ Maximum value.
brown :: Limit -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar -> Pattern Scalar
brown b = liftAST4 (S_brown b)

-- * Debugging

-- | Print the current value of the argument pattern to the console.
trace :: Trace Pattern a => Pattern a -> Pattern a
trace a = traceI a

class Bind pattern a b where
    bindI :: pattern a -> (pattern a -> pattern b) -> pattern b

class Stream pattern a where
    streamI :: StreamFunc -> pattern a -> pattern a

class List pattern a where
    listI :: Enumerator -> pattern Scalar -> [pattern a] -> pattern a

class Trace pattern a where
    traceI :: pattern a -> pattern a

-- | An integer identifier for referring to a binding.
type Binding = Int

-- | A map from identifiers to expressions.
type ExpMap t = Map.IntMap t

-- | State used when constructing abstract pattern trees.
--
-- We basically need a running counter for allocating binding ids and mappings from
-- bindings to expressions, one for each type in the object language.
data ASTState = ASTState {
    hashCount :: Binding    -- ^Running counter for binding identifiers
  -- , bMap :: ExpMap Boolean  -- ^Binding map for boolean expressions.
  -- , cMap :: ExpMap Coord    -- ^Binding map for coordinate expressions.
  , eMap :: ExpMap Event    -- ^Binding map for event expressions.
  , sMap :: ExpMap Scalar   -- ^Binding map for scalar expressions.
  } deriving (Eq, Read, Show)

-- | Abstract pattern tree wrapped in a state monad.
newtype Pattern t = AST { unAST :: State ASTState t }

-- | Return a new unique binding hash.
newHash :: State ASTState Binding
newHash = do
    s <- State.get
    let h = hashCount s
    State.put s { hashCount = succ h }
    return h

-- | A Patch is an event pattern.
type Patch = Pattern Event

liftAST :: (a -> r) -> Pattern a -> Pattern r
liftAST f = AST . liftM f . unAST

liftAST2 :: (a1 -> a2 -> r) -> Pattern a1 -> Pattern a2 -> Pattern r
liftAST2 f a b = AST $ liftM2 f (unAST a) (unAST b)

liftAST3 :: (a1 -> a2 -> a3 -> r) -> Pattern a1 -> Pattern a2 -> Pattern a3 -> Pattern r
liftAST3 f a b c = AST $ liftM3 f (unAST a) (unAST b) (unAST c)

liftAST4 :: (a1 -> a2 -> a3 -> a4 -> r) -> Pattern a1 -> Pattern a2 -> Pattern a3 -> Pattern a4 -> Pattern r
liftAST4 f a b c d = AST $ liftM4 f (unAST a) (unAST b) (unAST c) (unAST d)

instance Show (Pattern Scalar) where
    show = const "Pattern Scalar"

instance Show (Pattern Event) where
    show = const "Pattern Event"

instance Eq (Pattern Scalar) where
    (==) = error "Cannot compare (Pattern Scalar) for equality"

instance Eq (Pattern Event) where
    (==) = error "Cannot compare (Pattern Event) for equality"

dInf :: Double
dInf = 1/0

inf :: Pattern Scalar
inf = value dInf

instance Num (Pattern Scalar) where
    (+) = zip F_add
    (-) = zip F_subtract
    (*) = zip F_multiply
    abs = map F_abs
    signum = map F_signum
    fromInteger = value . fromInteger
    negate = map F_negate

instance Fractional (Pattern Scalar) where
    (/) = zip F_divide
    recip = map F_recip
    fromRational = value . fromRational

instance Floating (Pattern Scalar) where
    pi      = value pi
    exp     = map F_exp
    sqrt    = map F_sqrt
    log     = map F_log
    (**)    = zip F_power
    logBase = zip F_logBase
    sin     = map F_sin
    tan     = map F_tan
    cos     = map F_cos
    asin    = map F_asin
    atan    = map F_atan
    acos    = map F_acos
    sinh    = map F_sinh
    tanh    = map F_tanh
    cosh    = map F_cosh
    asinh   = map F_asinh
    atanh   = map F_atanh
    acosh   = map F_acosh

bindP :: (Binding -> a)
      -> (Binding -> a -> b -> b)
      -> Pattern a -> (Pattern a -> Pattern b) -> Pattern b
bindP fBinding fBind e f = AST $ do
    h <- newHash
    a <- unAST e
    b <- unAST (f (AST (return (fBinding h))))
    return $ fBind h a b

instance Bind Pattern Boolean Boolean where bindI = bindP B_binding B_bind_B
instance Bind Pattern Boolean Coord   where bindI = bindP B_binding C_bind_B
instance Bind Pattern Boolean Event   where bindI = bindP B_binding E_bind_B
instance Bind Pattern Boolean Scalar  where bindI = bindP B_binding S_bind_B

instance Bind Pattern Coord Boolean where bindI = bindP C_binding B_bind_C
instance Bind Pattern Coord Coord   where bindI = bindP C_binding C_bind_C
instance Bind Pattern Coord Event   where bindI = bindP C_binding E_bind_C
instance Bind Pattern Coord Scalar  where bindI = bindP C_binding S_bind_C

instance Bind Pattern Event Boolean where bindI = bindP E_binding B_bind_E
instance Bind Pattern Event Coord   where bindI = bindP E_binding C_bind_E
instance Bind Pattern Event Event   where bindI = bindP E_binding E_bind_E
instance Bind Pattern Event Scalar  where bindI = bindP E_binding S_bind_E

instance Bind Pattern Scalar Boolean where bindI = bindP S_binding B_bind_S
instance Bind Pattern Scalar Coord   where bindI = bindP S_binding C_bind_S
instance Bind Pattern Scalar Event   where bindI = bindP S_binding E_bind_S
instance Bind Pattern Scalar Scalar  where bindI = bindP S_binding S_bind_S

instance Stream Pattern Boolean where
    streamI f a = AST $ liftM (B_stream f) (unAST a)

instance Stream Pattern Coord where
    streamI f a = AST $ liftM (C_stream f) (unAST a)

instance Stream Pattern Event where
    streamI f a = AST $ liftM (E_stream f) (unAST a)

instance Stream Pattern Scalar where
    streamI f a = AST $ liftM (S_stream f) (unAST a)

instance List Pattern Boolean where
    listI e a b = AST $ liftM2 (B_list e) (unAST a) (mapM unAST b)

instance List Pattern Coord where
    listI e a b = AST $ liftM2 (C_list e) (unAST a) (mapM unAST b)

instance List Pattern Event where
    listI e a b = AST $ liftM2 (E_list e) (unAST a) (mapM unAST b)

instance List Pattern Scalar where
    listI e a b = AST $ liftM2 (S_list e) (unAST a) (mapM unAST b)

instance Trace Pattern Scalar where
    traceI = AST . liftM S_trace . unAST

instance Trace Pattern Boolean where
    traceI = AST . liftM B_trace . unAST

instance Trace Pattern Event where
    traceI = AST . liftM E_trace . unAST

instance Trace Pattern Coord where
    traceI = AST . liftM C_trace . unAST

data Tree t = Tree (ExpMap Scalar) (ExpMap Event) t
                deriving (Eq, Read, Show)

runAST :: Pattern t -> Tree t
runAST e = Tree (sMap s) (eMap s) a
    where (a, s) = State.runState (unAST e) (ASTState 0 Map.empty Map.empty)

patch :: Pattern Event -> Tree Event
patch = runAST

-- | AST node representing boolean expressions.
data Boolean =
  -- Binding
    B_binding Binding
  | B_bind_B Binding Boolean Boolean
  | B_bind_C Binding Coord   Boolean
  | B_bind_E Binding Event   Boolean
  | B_bind_S Binding Scalar  Boolean
  -- Structure
  | B_stream StreamFunc Boolean
  | B_list Enumerator Scalar [Boolean]
  -- Scalar comparison
  | B_compare Comparison Scalar Scalar
  -- Regions
  | B_contains Coord Scalar Coord
  -- Debugging
  | B_trace Boolean
  deriving (Eq, Read, Show)

-- | AST node representing scalar expressions.
data Scalar =
  -- Constant
    S_value Double
  -- Binding
  | S_binding Binding
  | S_bind_B Binding Boolean Scalar
  | S_bind_C Binding Coord   Scalar
  | S_bind_E Binding Event   Scalar
  | S_bind_S Binding Scalar  Scalar
  | S_get Field Event
  -- Structure
  | S_stream StreamFunc Scalar
  | S_list Enumerator Scalar [Scalar]
  -- Filters
  | S_map UnaryFunc Scalar
  | S_zip BinaryFunc Scalar Scalar
  | S_limit Limit Scalar Scalar Scalar
  -- Coordinates
  | S_x Coord
  | S_y Coord
  -- Regions
  | S_radius Scalar
  -- Randomness
  | S_rand Scalar Scalar
  | S_exprand Scalar Scalar
  | S_gaussian Scalar Scalar
  | S_brown Limit Scalar Scalar Scalar Scalar
  -- Debugging
  | S_trace Scalar
  deriving (Eq, Read, Show)

-- | AST node representing coordinate expressions.
data Coord =
  -- Constructor
    C_coord Scalar Scalar
  | C_polar Coord Scalar Scalar
  -- Binding
  | C_binding Binding
  | C_bind_B Binding Boolean Coord
  | C_bind_C Binding Coord   Coord
  | C_bind_E Binding Event   Coord
  | C_bind_S Binding Scalar  Coord
  -- Structure
  | C_stream StreamFunc Coord
  | C_list Enumerator Scalar [Coord]
  -- Regions
  | C_center Scalar
  -- Debugging
  | C_trace Coord
  deriving (Eq, Read, Show)

-- | AST node representing event expressions.
data Event =
  -- Binding
    E_binding Binding
  | E_bind_B Binding Boolean Event
  | E_bind_C Binding Coord   Event
  | E_bind_E Binding Event   Event
  | E_bind_S Binding Scalar  Event
  -- Field access
  | E_set Field Scalar Event
  -- Structure
  | E_stream StreamFunc Event
  | E_list Enumerator Scalar [Event]
  | E_par [Event]
  | E_takeDur Double Event
  -- Filters
  | E_filter Boolean Event
  -- Generators
  | E_closest Int Scalar Scalar Coord
  | E_region Int Scalar Scalar
  -- Cursor
  | E_step Scalar Scalar Event
  -- Debugging
  | E_trace Event
  deriving (Eq, Read, Show)
