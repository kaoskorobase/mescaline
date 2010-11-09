{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts #-}
module Mescaline.Synth.Pattern.Compiler (
    CompileError(..)
  , compile
) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Accessor
import qualified Data.IntMap as Map
import           Data.Typeable (Typeable)
import qualified Mescaline.Synth.Pattern.AST as AST
import           Mescaline.Synth.Pattern hiding (step)
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
    sBindings :: PMap Double     -- ^Scalar pattern bindings.
  , eBindings :: PMap Event      -- ^Event pattern bindings.
  , sMap :: AST.ExpMap AST.Scalar
  , eMap :: AST.ExpMap AST.Event
  }

-- | Empty environment.
mkEnv :: AST.ExpMap AST.Scalar -> AST.ExpMap AST.Event -> Env
mkEnv = Env Map.empty Map.empty

-- | The monad compilations are performed in.
type C a = ErrorT CompileError (State Env) a

-- | Run a compilation given an environment.
--
-- Either an error is returned or the compilation result.
runC :: Env -> C a -> Either CompileError a
runC e c = evalState (runErrorT c) e

-- | Convert a unary function tag to the corresponding function.
unFunc :: AST.UnaryFunc -> (Double -> Double)
unFunc AST.UF_abs       = abs
unFunc AST.UF_signum    = signum
unFunc AST.UF_negate    = negate
unFunc AST.UF_recip     = recip
unFunc AST.UF_exp       = exp
unFunc AST.UF_sqrt      = sqrt
unFunc AST.UF_log       = log
unFunc AST.UF_sin       = sin
unFunc AST.UF_tan       = tan
unFunc AST.UF_cos       = cos
unFunc AST.UF_asin      = asin
unFunc AST.UF_atan      = atan
unFunc AST.UF_acos      = acos
unFunc AST.UF_sinh      = sinh
unFunc AST.UF_tanh      = tanh
unFunc AST.UF_cosh      = cosh
unFunc AST.UF_asinh     = asinh
unFunc AST.UF_atanh     = atanh
unFunc AST.UF_acosh     = acosh

-- | Convert a binary function tag to the corresponding function.
binFunc :: AST.BinaryFunc -> (Double -> Double -> Double)
binFunc AST.BF_add      = (+)
binFunc AST.BF_subtract = (-)
binFunc AST.BF_multiply = (*)
binFunc AST.BF_divide   = (/)
binFunc AST.BF_power    = (**)
binFunc AST.BF_logBase  = logBase

comparison :: AST.Comparison -> (Double -> Double -> Bool)
comparison AST.Comp_eq  = (==)
comparison AST.Comp_gt  = (>)
comparison AST.Comp_geq = (>=)
comparison AST.Comp_lt  = (<)
comparison AST.Comp_leq = (<=)

field :: AST.Field -> Accessor Event Double
field AST.Delta = delta
field AST.CursorValue = accessor (cursorValue . getVal cursor) (const id)

-- | Return a binding in the map identified by f.
-- bindingS :: AST.Binding -> C (Pattern Double)
-- bindingS h = do
--     s <- get
--     case Map.lookup h (sBindings s) of
--         Nothing -> case Map.lookup h (sMap s) of
--                     Nothing -> throwError (BindingNotFound h)
--                     Just e -> do
--                         let p = compileS s
--                         put $ s { sBindings = Map.insert h p (sBindings s) }
--                         return p
--         Just p  -> return p

-- | Compile a scalar expression to a scalar pattern.
compileS :: AST.Scalar -> C (Pattern Double)
compileS (AST.S_value v) = return $ pure v
compileS (AST.S_binding h) = do
    s <- get
    case Map.lookup h (sBindings s) of
        Nothing -> case Map.lookup h (sMap s) of
                    Nothing -> throwError (BindingNotFound h)
                    Just e -> do
                        p <- compileS e
                        put $ s { sBindings = Map.insert h p (sBindings s) }
                        return p
        Just p  -> return p
compileS (AST.S_get f a) = liftM (fmap (getVal (field f))) (compileE a)
compileS (AST.S_cycle a) = liftM pcycle (compileS a)
compileS (AST.S_map f a) = liftM (fmap (unFunc f)) (compileS a)
compileS (AST.S_zip f a b) = liftM2 (pzipWith (binFunc f)) (compileS a) (compileS b)
compileS (AST.S_seq xs ns) = do
    xs' <- mapM compileS xs
    ns' <- compileS ns
    return $ pseq xs' (fmap truncate ns')
compileS (AST.S_ser xs ns) = do
    xs' <- mapM compileS xs
    ns' <- compileS ns
    return $ pser xs' (fmap truncate ns')
-- Coordinates
compileS (AST.S_x a) = liftM (fmap fst) (compileC a)
compileS (AST.S_y a) = liftM (fmap snd) (compileC a)
compileS (AST.S_radius a) = liftM radius (compileS a)
-- Randomness
compileS (AST.S_rand a b) = liftM2 prrand_ (compileS a) (compileS b)
compileS (AST.S_exprand a b) = liftM2 pexprand_ (compileS a) (compileS b)
-- Debugging
compileS (AST.S_trace a) = liftM ptrace (compileS a)

-- | Compile a boolean expression to a pattern of Bool.
compileB :: AST.Boolean -> C (Pattern Bool)
compileB (AST.B_compare c a b)  = liftM2 (pzipWith (comparison c)) (compileS a) (compileS b)
-- compileB (AST.B_contains a b c) = liftM3 contains (compileC a) (compileS b) (compileC c)
compileB (AST.B_trace a) = liftM ptrace (compileB a)

-- | Compile a coordinate expression to a pattern of pairs.
compileC :: AST.Coord -> C (Pattern (Double, Double))
compileC (AST.C_coord a b) = liftM2 coord (compileS a) (compileS b)
compileC (AST.C_polar a b c) = liftM3 polar (compileC a) (compileS b) (compileS c)
compileC (AST.C_center a) = liftM center (compileS a)
compileC (AST.C_constrain a b c) = liftM3 constrain (compileC a) (compileS b) (compileC c)
compileC (AST.C_trace a) = liftM ptrace (compileC a)

-- | Compile an event expression to an event pattern.
compileE :: AST.Event -> C (Pattern Event)
compileE (AST.E_binding h) = do
    s <- get
    case Map.lookup h (eBindings s) of
        Nothing -> case Map.lookup h (eMap s) of
                    Nothing -> throwError (BindingNotFound h)
                    Just e -> do
                        p <- compileE e
                        put $ s { eBindings = Map.insert h p (eBindings s) }
                        return p
        Just p  -> return p
compileE (AST.E_cycle a) = liftM pcycle (compileE a)
compileE (AST.E_seq xs ns) = do
    xs' <- mapM compileE xs
    ns' <- compileS ns
    return $ pseq xs' (fmap truncate ns')
compileE (AST.E_ser xs ns) = do
    xs' <- mapM compileE xs
    ns' <- compileS ns
    return $ pser xs' (fmap truncate ns')
compileE (AST.E_par xs) = liftM ppar (mapM compileE xs)
compileE (AST.E_set f a b) = liftM2 (pzipWith (setVal (field f))) (compileS a) (compileE b)
-- Filter
compileE (AST.E_filter a b) = liftM2 filterE (compileB a) (compileE b)
-- Generators
compileE (AST.E_closest i a b c) = liftM3 (closest i) (compileS a) (compileC b) (compileS c)
compileE (AST.E_region it i a b) = liftM2 (region it i) (compileS a) (compileS b)
-- Cursor
compileE (AST.E_step a b c) = liftM3 step (compileS a) (compileS b) (compileE c)
-- Debugging
compileE (AST.E_trace a) = liftM ptrace (compileE a)

-- | Compile a syntax tree with the corresponding environment to an event pattern.
compile :: AST.Tree AST.Event -> Either CompileError (Pattern Event)
compile (AST.Tree sm em e) = runC (mkEnv sm em) (compileE e)
    -- -- Compile scalar expression bindings
    -- sm' <- T.mapM compileS sm
    -- -- Compile event expression bindings
    -- em' <- T.mapM compileE em
    -- -- Compile the toplevel expression given the compiled bindings
    -- local (const $ Env sm' em') $ compileE e
