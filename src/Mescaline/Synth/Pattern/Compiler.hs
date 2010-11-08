{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts #-}
module Mescaline.Synth.Pattern.Compiler (
    CompileError(..)
  , compile
) where

import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Data.IntMap as Map
import qualified Data.Traversable as T
import           Data.Typeable (Typeable)
import qualified Mescaline.Synth.Pattern.AST as AST
import           Mescaline.Synth.Pattern
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern.Library

-- | A type for compilation errors.
data CompileError = CompileError String
                  | BindingNotFound AST.Binding
                  deriving (Eq, Read, Show, Typeable)

instance Error CompileError where
    strMsg = CompileError

instance Exception CompileError

type PMap t = Map.IntMap (Pattern t)

-- | Compiler environment.
data Env = Env {
    sMap :: PMap Double     -- ^Scalar pattern bindings.
  , eMap :: PMap Event      -- ^Event pattern bindings.
  }

-- | Empty environment.
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty

-- | The monad compilations are performed in.
type C a = ErrorT CompileError (Reader Env) a

-- | Run a compilation given an environment.
--
-- Either an error is returned or the compilation result.
runC :: Env -> C a -> Either CompileError a
runC e c = runReader (runErrorT c) e

-- | Convert a unary function tag to the corresponding function.
unFunc :: AST.UnaryFunc -> (Double -> Double)
unFunc AST.UF_abs    = abs
unFunc AST.UF_signum = signum
unFunc AST.UF_negate = negate
unFunc AST.UF_recip  = recip

-- | Convert a binary function tag to the corresponding function.
binFunc :: AST.BinaryFunc -> (Double -> Double -> Double)
binFunc AST.BF_add      = (+)
binFunc AST.BF_subtract = (-)
binFunc AST.BF_multiply = (*)
binFunc AST.BF_divide   = (/)

-- | Return a binding in the map identified by f.
binding :: (Env -> PMap a) -> AST.Binding -> C (Pattern a)
binding f h = do
    m <- asks f
    case Map.lookup h m of
        Nothing -> throwError (BindingNotFound h)
        Just a  -> return a

-- | Compile a scalar expression to a scalar pattern.
compileS :: AST.Scalar -> C (Pattern Double)
compileS (AST.S_value v) =
    return $ return v
compileS (AST.S_binding h) = binding sMap h
compileS (AST.S_cycle a) = do
    a' <- compileS a
    return $ pcycle a'
compileS (AST.S_map f a) = do
    a' <- compileS a
    return $ fmap (unFunc f) a'
compileS (AST.S_zip f a b) = do
    a' <- compileS a
    b' <- compileS b
    return $ pzipWith (binFunc f) a' b'
compileS (AST.S_seq xs ns) = do
    xs' <- mapM compileS xs
    ns' <- compileS ns
    return $ pseq xs' (fmap truncate ns')
compileS (AST.S_ser xs ns) = do
    xs' <- mapM compileS xs
    ns' <- compileS ns
    return $ pser xs' (fmap truncate ns')

-- | Compile an event expression to an event pattern.
compileE :: AST.Event -> C (Pattern Event)
compileE (AST.E_binding h) = binding eMap h
compileE (AST.E_cycle a) = do
    a' <- compileE a
    return $ pcycle a'
compileE (AST.E_seq xs ns) = do
    xs' <- mapM compileE xs
    ns' <- compileS ns
    return $ pseq xs' (fmap truncate ns')
compileE (AST.E_ser xs ns) = do
    xs' <- mapM compileE xs
    ns' <- compileS ns
    return $ pser xs' (fmap truncate ns')
compileE (AST.E_par xs) = do
    xs' <- mapM compileE xs
    return $ ppar xs'
compileE (AST.E_metro i t) = do
    t' <- compileS t
    return $ metro i t'
compileE (AST.E_sequencer i t) = do
    t' <- compileS t
    return $ sequencer i t'
compileE (AST.E_region it i a) = do
    i' <- compileS i
    a' <- compileE a
    return $ region it i' a'

-- | Compile a syntax tree with the corresponding environment to an event pattern.
compile :: AST.Tree AST.Event -> Either CompileError (Pattern Event)
compile (AST.Tree sm em e) = runC emptyEnv $ do
    -- Compile scalar expression bindings
    sm' <- T.mapM compileS sm
    -- Compile event expression bindings
    em' <- T.mapM compileE em
    -- Compile the toplevel expression given the compiled bindings
    local (const $ Env sm' em') $ compileE e
