{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern.AST where

-- import Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.IntMap as Map
import           Prelude hiding (cycle, map, seq, zip)

-- type UnFunc = String
-- type BinFunc = String
data UnaryFunc =
    UF_abs
  | UF_signum
  | UF_negate
  | UF_recip
  deriving (Eq, Read, Show)

data BinaryFunc =
    BF_add
  | BF_subtract
  | BF_multiply
  | BF_divide
  deriving (Eq, Read, Show)

type Name = String
data Predicate = Predicate deriving (Eq, Read, Show)

data RegionIterator =
    Uniform
    deriving (Eq, Read, Show)

-- | This class represents the abstract pattern language syntax.
--
-- The repr argument is a placeholder for the concrete representation; in our case
-- the representation constructs an abstract syntax tree (see 'Pattern'), that is
-- read and compiled to a 'Mescaline.Synth.Pattern.Pattern'.
class Language repr where
    value :: Double -> repr Scalar
    
    bindS :: repr Scalar -> (repr Scalar -> repr b) -> repr b -- like flip ($)
    bindE :: repr Event -> (repr Event -> repr b) -> repr b -- like flip ($)
    
    cycleS :: repr Scalar -> repr Scalar
    cycleE :: repr Event -> repr Event
    
    map   :: UnaryFunc -> repr Scalar -> repr Scalar
    zip   :: BinaryFunc -> repr Scalar -> repr Scalar -> repr Scalar
    
    seqS  :: [repr Scalar] -> repr Scalar -> repr Scalar
    seqE  :: [repr Event] -> repr Scalar -> repr Event
    serS  :: [repr Scalar] -> repr Scalar -> repr Scalar
    serE  :: [repr Event] -> repr Scalar -> repr Event
    par   :: [repr Event] -> repr Event

    metro :: Int -> repr Scalar -> repr Event
    sequencer :: Int -> repr Scalar -> repr Event
    region :: RegionIterator -> repr Scalar -> repr Event -> repr Event

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
  , sMap :: ExpMap Scalar   -- ^Binding map for scalar expressions.
  , eMap :: ExpMap Event    -- ^Binding map for event expressions.
  } deriving (Eq, Read, Show)

-- | Abstract pattern tree wrapped in a state monad.
newtype Pattern t = AST { unAST :: State ASTState t }

-- | A Patch is an event pattern.
type Patch = Pattern Event

instance Language Pattern where
    value = AST . return . S_value
    bindS e f = AST $ do
        a <- unAST e
        s <- get
        let h  = hashCount s
            s' = s { hashCount = succ h
                   , sMap = Map.insert h a (sMap s) }
        put s'
        unAST $ f (AST (return (S_binding h)))
    bindE e f = AST $ do
        a <- unAST e
        s <- get
        let h  = hashCount s
            s' = s { hashCount = succ h
                   , eMap = Map.insert h a (eMap s) }
        put s'
        unAST $ f (AST (return (E_binding h)))
    cycleS e = AST $ unAST e >>= return . S_cycle
    cycleE e = AST $ unAST e >>= return . E_cycle
    map f e = AST $ unAST e >>= return . S_map f
    zip f e1 e2 = AST $ do
        a <- unAST e1
        b <- unAST e2
        return $ S_zip f a b
    seqS es ns = AST $ do
        as <- mapM unAST es
        ns' <- unAST ns
        return $ S_seq as ns'
    seqE es ns = AST $ do
        as <- mapM unAST es
        ns' <- unAST ns
        return $ E_seq as ns'
    serS es ns = AST $ do
        as <- mapM unAST es
        ns' <- unAST ns
        return $ S_ser as ns'
    serE es ns = AST $ do
        as <- mapM unAST es
        ns' <- unAST ns
        return $ E_ser as ns'
    par es = AST $ do
        as <- mapM unAST es
        return $ E_par as
    metro i dt = AST $ do
        dt' <- unAST dt
        return $ E_metro i dt'
    sequencer i dt = AST $ do
        dt' <- unAST dt
        return $ E_sequencer i dt'
    region it i a = AST $ do
        i' <- unAST i
        a' <- unAST a
        return $ E_region it i' a'

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

class Bind repr a where
    bind :: repr a -> (repr a -> repr b) -> repr b

instance Bind Pattern Scalar where
    bind = bindS

instance Bind Pattern Event where
    bind = bindE

class Cycle repr a where
    cycle :: repr a -> repr a

instance Cycle Pattern Scalar where
    cycle = cycleS

instance Cycle Pattern Event where
    cycle = cycleE

class Seq repr a where
    seq :: [repr a] -> repr Scalar -> repr a

instance Seq Pattern Scalar where
    seq = seqS

instance Seq Pattern Event where
    seq = seqE

class Ser repr a where
    ser :: [repr a] -> repr Scalar -> repr a

instance Ser Pattern Scalar where
    ser = serS

instance Ser Pattern Event where
    ser = serE

data Tree t = Tree (ExpMap Scalar) (ExpMap Event) t
                deriving (Eq, Read, Show)

runAST :: Pattern t -> Tree t
runAST e = Tree (sMap s) (eMap s) a
    where (a, s) = runState (unAST e) (ASTState 0 Map.empty Map.empty)

patch :: Pattern Event -> Tree Event
patch = runAST

data Scalar =
    S_value Double
  | S_binding Binding
  -- Structure
  | S_cycle Scalar
  | S_seq [Scalar] Scalar
  | S_ser [Scalar] Scalar
  -- Filters
  | S_map UnaryFunc Scalar
  | S_zip BinaryFunc Scalar Scalar
  -- S_get Name
  deriving (Eq, Read, Show)

data Event =
  -- Binding
    E_binding Binding
  -- Structure
  | E_cycle Event
  | E_seq [Event] Scalar
  | E_ser [Event] Scalar
  | E_par [Event]
  -- Generators
  | E_metro Int Scalar
  | E_sequencer Int Scalar
  -- Filters
  | E_region RegionIterator Scalar Event
  -- E_bind Name Scalar
  -- E_filter Predicate Event
  deriving (Eq, Read, Show)
