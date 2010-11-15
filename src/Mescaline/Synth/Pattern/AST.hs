{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern.AST where

import           Control.Monad
import           Control.Monad.State (MonadState, State)
import qualified Control.Monad.State as State
import qualified Data.IntMap as Map
import           Prelude hiding (cycle, filter, map, seq, zip)

data UnaryFunc =
    UF_abs
  | UF_signum
  | UF_negate
  | UF_recip
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

data RegionIterator =
    Uniform
    deriving (Eq, Read, Show)

-- | This class represents the abstract pattern language syntax.
--
-- The pattern argument is a placeholder for the concrete representation; in our case
-- the representation constructs an abstract syntax tree (see 'Pattern'), that is
-- read and compiled to a 'Mescaline.Synth.Pattern.Pattern'.
class Language pattern where
    value :: Double -> pattern Scalar
    
    bind :: Bind pattern a b => pattern a -> (pattern a -> pattern b) -> pattern b
    
    cycle :: Cycle pattern a => pattern a -> pattern a
    
    map   :: UnaryFunc -> pattern Scalar -> pattern Scalar
    zip   :: BinaryFunc -> pattern Scalar -> pattern Scalar -> pattern Scalar
    
    seq   :: Seq pattern a => [pattern a] -> pattern Scalar -> pattern a
    ser   :: Ser pattern a => [pattern a] -> pattern Scalar -> pattern a
    par   :: [pattern Event] -> pattern Event

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
    constrain :: pattern Coord -> pattern Scalar -> pattern Coord -> pattern Coord

    -- Regions
    center :: pattern Scalar -> pattern Coord
    radius :: pattern Scalar -> pattern Scalar
    -- | @closest cursor defaultDelta coord radius@
    closest :: Int -> pattern Scalar -> pattern Coord -> pattern Scalar -> pattern Event
    -- | @region iterator cursor defaultDelta region@
    region :: RegionIterator -> Int -> pattern Scalar -> pattern Scalar -> pattern Event
    
    -- Cursor
    step :: pattern Scalar -> pattern Scalar -> pattern Event -> pattern Event

    -- Randomness
    rand    :: pattern Scalar -> pattern Scalar -> pattern Scalar
    exprand :: pattern Scalar -> pattern Scalar -> pattern Scalar

    -- Debugging
    trace :: Trace pattern a => pattern a -> pattern a

class Bind pattern a b where
    bindI :: pattern a -> (pattern a -> pattern b) -> pattern b

class Cycle pattern a where
    cycleI :: pattern a -> pattern a

class Seq pattern a where
    seqI :: [pattern a] -> pattern Scalar -> pattern a

class Ser pattern a where
    serI :: [pattern a] -> pattern Scalar -> pattern a

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

instance Language Pattern where
    value = AST . return . S_value
    bind = bindI
    
    cycle = cycleI
    map = liftAST . S_map
    zip = liftAST2 . S_zip
    seq = seqI
    ser = serI
    par a = AST $ liftM E_par (mapM unAST a)
    
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
    constrain = liftAST3 C_constrain

    -- Regions
    center  = liftAST C_center
    radius  = liftAST S_radius
    
    -- Generators
    closest = liftAST3 . E_closest
    region it i = liftAST2 (E_region it i)

    -- Cursor
    step = liftAST3 E_step

    -- Randomness
    rand    = liftAST2 S_rand
    exprand = liftAST2 S_exprand

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

-- instance Bind Pattern Scalar where
--     bindI e f = AST $ do
--         a <- unAST e
--         s <- State.get
--         let h  = hashCount s
--             s' = s { hashCount = succ h
--                    , sMap = Map.insert h a (sMap s) }
--         State.put s'
--         unAST $ f (AST (return (S_binding h)))
-- 
-- instance Bind Pattern Event where
--     bindI e f = AST $ do
--         a <- unAST e
--         s <- State.get
--         let h  = hashCount s
--             s' = s { hashCount = succ h
--                    , eMap = Map.insert h a (eMap s) }
--         State.put s'
--         unAST $ f (AST (return (E_binding h)))

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

instance Cycle Pattern Scalar where
    cycleI = AST . liftM S_cycle . unAST

instance Cycle Pattern Event where
    cycleI = AST . liftM E_cycle . unAST

instance Seq Pattern Scalar where
    seqI a b = AST $ liftM2 S_seq (mapM unAST a) (unAST b)

instance Seq Pattern Event where
    seqI a b = AST $ liftM2 E_seq (mapM unAST a) (unAST b)

instance Ser Pattern Scalar where
    serI a b = AST $ liftM2 S_ser (mapM unAST a) (unAST b)

instance Ser Pattern Event where
    serI a b = AST $ liftM2 E_ser (mapM unAST a) (unAST b)

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
  | S_cycle Scalar
  | S_seq [Scalar] Scalar
  | S_ser [Scalar] Scalar
  -- Filters
  | S_map UnaryFunc Scalar
  | S_zip BinaryFunc Scalar Scalar
  -- Coordinates
  | S_x Coord
  | S_y Coord
  -- Regions
  | S_radius Scalar
  -- Randomness
  | S_rand Scalar Scalar
  | S_exprand Scalar Scalar
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
  -- Regions
  | C_center Scalar
  | C_constrain Coord Scalar Coord
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
  | E_cycle Event
  | E_seq [Event] Scalar
  | E_ser [Event] Scalar
  | E_par [Event]
  -- Filters
  | E_filter Boolean Event
  -- Generators
  | E_closest Int Scalar Coord Scalar
  | E_region RegionIterator Int Scalar Scalar
  -- Cursor
  | E_step Scalar Scalar Event
  -- Debugging
  | E_trace Event
  deriving (Eq, Read, Show)
