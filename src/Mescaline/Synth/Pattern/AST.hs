{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern.AST where

import           Control.Monad
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Data.IntMap as Map
import           Prelude hiding (cycle, filter, map, seq, zip)

data UnaryFunc =
    UF_abs
  | UF_signum
  | UF_negate
  | UF_recip
  | UF_truncate
  | UF_round
  | UF_ceiling
  | UF_floor
  | UF_exp
  | UF_sqrt
  | UF_log
  | UF_sin
  | UF_tan
  | UF_cos
  | UF_asin
  | UF_atan
  | UF_acos
  | UF_sinh
  | UF_tanh
  | UF_cosh
  | UF_asinh
  | UF_atanh
  | UF_acosh
  deriving (Eq, Read, Show)

data BinaryFunc =
    BF_add
  | BF_subtract
  | BF_multiply
  | BF_divide
  | BF_power
  | BF_logBase
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
  | Rate            -- ^Playback rate (> 0).
  | AttackTime      -- ^Envelope attack time in seconds.
  | ReleaseTime     -- ^Envelope release time in seconds.
  | Feature Int Int -- ^Feature value at index i.
  deriving (Eq, Read, Show)

-- | Region identifier.
type Region = Int

data RegionIterator =
    Uniform
    deriving (Eq, Read, Show)

-- | Interval boundary behavior.
data Limit =
    Clip -- ^Clip to interval.
  | Wrap -- ^Wrap to other end of interval.
  | Fold -- ^Fold back into interval.
  deriving (Eq, Read, Show)

-- | This class represents the abstract pattern language syntax.
--
-- The pattern argument is a placeholder for the concrete representation; in our case
-- the representation constructs an abstract syntax tree (see 'Pattern'), that is
-- read and compiled to a 'Mescaline.Synth.Pattern.Pattern'.
class Language pattern where
    value :: Double -> pattern Scalar

    bind :: Bind pattern a b => pattern a -> (pattern a -> pattern b) -> pattern b

    stream :: Stream pattern a => StreamFunc -> pattern a -> pattern a    
    list :: List pattern a => Enumerator -> pattern Scalar -> [pattern a] -> pattern a
    par :: [pattern Event] -> pattern Event

    map :: UnaryFunc -> pattern Scalar -> pattern Scalar
    zip :: BinaryFunc -> pattern Scalar -> pattern Scalar -> pattern Scalar
    limit :: Limit -> pattern Scalar -> pattern Scalar -> pattern Scalar -> pattern Scalar

    -- Comparisons
    (==) :: pattern Scalar -> pattern Scalar -> pattern Boolean
    (>)  :: pattern Scalar -> pattern Scalar -> pattern Boolean
    (>=) :: pattern Scalar -> pattern Scalar -> pattern Boolean
    (<)  :: pattern Scalar -> pattern Scalar -> pattern Boolean
    (<=) :: pattern Scalar -> pattern Scalar -> pattern Boolean

    -- Events
    get :: Field -> pattern Event -> pattern Scalar
    set :: Field -> pattern Scalar -> pattern Event -> pattern Event
    filter :: pattern Boolean -> pattern Event -> pattern Event

    -- Coordinates
    coord :: pattern Scalar -> pattern Scalar -> pattern Coord
    polar :: pattern Coord -> pattern Scalar -> pattern Scalar -> pattern Coord
    x :: pattern Coord -> pattern Scalar
    y :: pattern Coord -> pattern Scalar

    -- Regions
    center :: pattern Scalar -> pattern Coord
    radius :: pattern Scalar -> pattern Scalar
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
    closest :: Int -> pattern Scalar -> pattern Scalar -> pattern Coord -> pattern Event
    -- | Select a unit from a region according to the iterator.
    --
    -- @region cursor defaultDelta iterator@
    --
    -- * @cursor@ Cursor and region id for this event.
    --
    -- * @defaultDelta@ Default delta time in case the region is empty.
    --
    -- * @iterator@ Scalar pattern in the range [0;1[ that selects a unit from the region.
    region :: Int -> pattern Scalar -> pattern Scalar -> pattern Event

    -- Cursor
    step :: pattern Scalar -> pattern Scalar -> pattern Event -> pattern Event

    -- Randomness
    
    -- | Generate white noise in a given range.
    --
    -- @rand min max@
    --
    -- * @min@ Minimum value.
    --
    -- * @max@ Maximum value.
    rand :: pattern Scalar -> pattern Scalar -> pattern Scalar
    -- | Generate exponential noise in a given range.
    --
    -- @exprand min max@
    --
    -- * @min@ Minimum value.
    --
    -- * @max@ Maximum value.
    exprand :: pattern Scalar -> pattern Scalar -> pattern Scalar
    -- | Generate gaussian noise with the given mean and variance.
    --
    -- @gaussian mean var@
    --
    -- * @mean@ Mean value.
    --
    -- * @var@ Variance, or standard deviation squared.
    gaussian :: pattern Scalar -> pattern Scalar -> pattern Scalar
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
    brown :: Limit -> pattern Scalar -> pattern Scalar -> pattern Scalar -> pattern Scalar -> pattern Scalar

    -- Debugging
    trace :: Trace pattern a => pattern a -> pattern a

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

instance Language Pattern where
    value = AST . return . S_value
    bind = bindI
    
    stream = streamI
    list = listI
    par a = AST $ liftM E_par (mapM unAST a)

    -- Scalars
    map = liftAST . S_map
    zip = liftAST2 . S_zip
    limit l = liftAST3 (S_limit l)

    -- Comparisons
    (==) = liftAST2 (B_compare Comp_eq)
    (>)  = liftAST2 (B_compare Comp_gt)
    (>=) = liftAST2 (B_compare Comp_geq)
    (<)  = liftAST2 (B_compare Comp_lt)
    (<=) = liftAST2 (B_compare Comp_leq)

    -- Events
    get    = liftAST . S_get
    set    = liftAST2 . E_set
    filter = liftAST2 E_filter

    -- *Coordinates
    
    coord     = liftAST2 C_coord
    polar     = liftAST3 C_polar
    x         = liftAST S_x
    y         = liftAST S_y

    -- Regions
    center  = liftAST C_center
    radius  = liftAST S_radius
    
    -- Generators
    closest i = liftAST3 (E_closest i)
    region i = liftAST2 (E_region i)

    -- Cursor
    step = liftAST3 E_step

    -- Randomness
    rand     = liftAST2 S_rand
    exprand  = liftAST2 S_exprand
    gaussian = liftAST2 S_gaussian
    brown b  = liftAST4 (S_brown b)

    -- Debugging
    trace a = traceI a

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
    (+) = zip BF_add
    (-) = zip BF_subtract
    (*) = zip BF_multiply
    abs = map UF_abs
    signum = map UF_signum
    fromInteger = value . fromInteger
    negate = map UF_negate

instance Fractional (Pattern Scalar) where
    (/) = zip BF_divide
    recip = map UF_recip
    fromRational = value . fromRational

instance Floating (Pattern Scalar) where
    pi      = value pi
    exp     = map UF_exp
    sqrt    = map UF_sqrt
    log     = map UF_log
    (**)    = zip BF_power
    logBase = zip BF_logBase
    sin     = map UF_sin
    tan     = map UF_tan
    cos     = map UF_cos
    asin    = map UF_asin
    atan    = map UF_atan
    acos    = map UF_acos
    sinh    = map UF_sinh
    tanh    = map UF_tanh
    cosh    = map UF_cosh
    asinh   = map UF_asinh
    atanh   = map UF_atanh
    acosh   = map UF_acosh

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
