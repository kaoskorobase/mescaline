{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts #-}
module Mescaline.Synth.Pattern.Compiler (
    CompileError(..)
  , Bindings
  , compile
) where

import           Control.Applicative
import           Control.Category
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import qualified Control.Monad.State as State
import           Data.Accessor hiding ((%:))
import           Data.Accessor.Tuple
import           Data.Accessor.Monad.MTL.State ((%:))
import qualified Data.Vector.Generic as V
import qualified Data.IntMap as Map
import           Data.Typeable (Typeable)
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Synth.FeatureSpace.Unit as Unit
import qualified Mescaline.Synth.Pattern.AST as AST
import           Mescaline.Synth.Pattern hiding (step)
import qualified Mescaline.Synth.Pattern.Binding as B
import           Mescaline.Synth.Pattern.Environment as Env
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern.Library
import qualified Mescaline.Synth.Pattern.Sequencer as Seq
import           Prelude hiding (id, (.))

-- | A type for compilation errors.
data CompileError = CompileError String
                  | BindingNotFound AST.Binding
                  deriving (Eq, Read, Typeable)

instance Show CompileError where
    show (CompileError s) = "Compile error: " ++ s
    show (BindingNotFound i) = "Binding not found: " ++ show i

instance Error CompileError where
    strMsg = CompileError

instance Exception CompileError

type PMap t = Map.IntMap (Pattern t)

-- | Compiler environment.
-- data Env = Env {
--     bBindings :: PMap Bool             -- ^Bool pattern bindings.
--   , cBindings :: PMap (Double, Double) -- ^Coord pattern bindings.
--   , eBindings :: PMap Event            -- ^Event pattern bindings.
--   , sBindings :: PMap Double           -- ^Scalar pattern bindings.
--   -- , sMap :: AST.ExpMap AST.Scalar
--   -- , eMap :: AST.ExpMap AST.Event
--   }
type Bindings = B.Bindings Environment

-- | Empty environment.
mkEnv :: AST.ExpMap AST.Scalar -> AST.ExpMap AST.Event -> Bindings
mkEnv _ _ = B.empty -- Env Map.empty Map.empty Map.empty Map.empty

-- | The monad compilations are performed in.
type C a = ErrorT CompileError (State.State Bindings) a

-- | Run a compilation given an environment.
--
-- Either an error is returned or the compilation result.
runC :: Bindings -> C a -> Either CompileError (a, Bindings)
runC e c = case State.runState (runErrorT c) e of
            (Left err, _)  -> Left err
            (Right a, env) -> Right (a, env)

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

synthAccessor :: a -> Accessor Synth a -> Accessor Event a
synthAccessor a0 acc = accessor (maybe a0 (getVal acc) . getVal synth)
                                (\a e -> case e ^. synth of
                                            Nothing -> e
                                            Just s -> setVal synth (Just (setVal acc a s)) e)

field :: AST.Field -> Accessor (Environment, Event) Double
field AST.Delta = delta . second
field AST.Cursor = accessor (fromIntegral . getVal (cursor . second))
                            (const id)
field AST.CursorValue = accessor (maybe 0 id . f) (const id)
    where
        f (env, evt) = do
            let s = env ^. sequencer
            c <- Seq.getCursor (evt ^. cursor) s
            Seq.lookupAtCursor c s
field AST.Offset = synthAccessor 0 offset . second
field AST.Duration = synthAccessor 0 duration . second
field AST.Rate = synthAccessor 1 rate . second
field AST.Level = synthAccessor 1 sustainLevel . second
field AST.AttackTime = synthAccessor 0 attackTime . second
field AST.ReleaseTime = synthAccessor 0 releaseTime . second
field (AST.Feature i j) = synthAccessor 0 (acc . unit) . second
    where
        at i v | i < 0 || i >= V.length v = Nothing
               | otherwise                = Just (v V.! i)
        acc = accessor (maybe 0 id . join . fmap (at j) . fmap Feature.value . at i . Unit.features)
                       (const id)

-- | Compile a binding.
compileBinding ::
    Accessor (B.Bindings Environment) (B.BindingMap Environment a)
 -> Int
 -> C (Pattern a)
compileBinding bm = return . B.pbinding (Env.bindings .> bm)

-- | Compile a bind.
compileBind ::
    (t1 -> C (Pattern a))
 -> (t2 -> C (Pattern b))
 -> Accessor (B.Bindings Environment) (B.BindingMap Environment a)
 -> AST.Binding
 -> t1
 -> t2
 -> C (Pattern b)
compileBind ca cb bm h a b = do
    -- Compile bound pattern
    a' <- ca a
    -- Compile body pattern
    b' <- cb b
    -- Insert bound pattern into binding map
    bm %: Map.insert h (B.mkBinding a')
    -- Return pbind
    return $ B.pbind (Env.bindings .> bm) h b'

-- | Compile a boolean expression to a pattern of Bool.
compileB :: AST.Boolean -> C (Pattern Bool)
compileB (AST.B_binding h)    = compileBinding B.boolean h
compileB (AST.B_bind_B h a b) = compileBind compileB compileB B.boolean h a b
compileB (AST.B_bind_C h a b) = compileBind compileC compileB B.coord   h a b
compileB (AST.B_bind_E h a b) = compileBind compileE compileB B.event   h a b
compileB (AST.B_bind_S h a b) = compileBind compileS compileB B.scalar   h a b
compileB (AST.B_compare c a b)  = liftM2 (pzipWith (comparison c)) (compileS a) (compileS b)
-- compileB (AST.B_contains a b c) = liftM3 contains (compileC a) (compileS b) (compileC c)
compileB (AST.B_trace a) = liftM ptrace (compileB a)

-- | Compile a coordinate expression to a pattern of pairs.
compileC :: AST.Coord -> C (Pattern (Double, Double))
compileC (AST.C_binding h)    = compileBinding B.coord h
compileC (AST.C_bind_B h a b) = compileBind compileB compileC B.boolean h a b
compileC (AST.C_bind_C h a b) = compileBind compileC compileC B.coord   h a b
compileC (AST.C_bind_E h a b) = compileBind compileE compileC B.event   h a b
compileC (AST.C_bind_S h a b) = compileBind compileS compileC B.scalar  h a b
compileC (AST.C_coord a b) = liftM2 coord (compileS a) (compileS b)
compileC (AST.C_polar a b c) = liftM3 polar (compileC a) (compileS b) (compileS c)
compileC (AST.C_center a) = liftM center (compileS a)
compileC (AST.C_trace a) = liftM ptrace (compileC a)

-- | Compile an event expression to an event pattern.
compileE :: AST.Event -> C (Pattern Event)
-- compileE (AST.E_binding h) = do
--     s <- get
--     case Map.lookup h (eBindings s) of
--         Nothing -> case Map.lookup h (eMap s) of
--                     Nothing -> throwError (BindingNotFound h)
--                     Just e -> do
--                         p <- compileE e
--                         put $ s { eBindings = Map.insert h p (eBindings s) }
--                         return p
--         Just p  -> return p
compileE (AST.E_binding h)    = compileBinding B.event h
compileE (AST.E_bind_B h a b) = compileBind compileB compileE B.boolean h a b
compileE (AST.E_bind_C h a b) = compileBind compileC compileE B.coord   h a b
compileE (AST.E_bind_E h a b) = compileBind compileE compileE B.event   h a b
compileE (AST.E_bind_S h a b) = compileBind compileS compileE B.scalar  h a b
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
compileE (AST.E_set f a b) = liftM (fmap snd) $ liftM2 (pzipWith (setVal (field f))) (compileS a) (fmap (pzip ask) (compileE b))
-- Filter
compileE (AST.E_filter a b) = liftM2 filterE (compileB a) (compileE b)
-- Generators
compileE (AST.E_closest i a b c) = liftM3 (closest i) (compileS a) (compileC b) (compileS c)
compileE (AST.E_region it i a b) = liftM2 (region it i) (compileS a) (compileS b)
-- Cursor
compileE (AST.E_step a b c) = liftM3 step (compileS a) (compileS b) (compileE c)
-- Debugging
compileE (AST.E_trace a) = liftM ptrace (compileE a)

-- | Compile a scalar expression to a scalar pattern.
compileS :: AST.Scalar -> C (Pattern Double)
compileS (AST.S_value v) = return $ pure v
-- compileS (AST.S_binding h) = do
--     s <- get
--     case Map.lookup h (sBindings s) of
--         Nothing -> case Map.lookup h (sMap s) of
--                     Nothing -> throwError (BindingNotFound h)
--                     Just e -> do
--                         p <- compileS e
--                         put $ s { sBindings = Map.insert h p (sBindings s) }
--                         return p
--         Just p  -> return p
compileS (AST.S_binding h)    = compileBinding B.scalar h
compileS (AST.S_bind_B h a b) = compileBind compileB compileS B.boolean h a b
compileS (AST.S_bind_C h a b) = compileBind compileC compileS B.coord   h a b
compileS (AST.S_bind_E h a b) = compileBind compileE compileS B.event   h a b
compileS (AST.S_bind_S h a b) = compileBind compileS compileS B.scalar  h a b
compileS (AST.S_get f a) = liftM (fmap (getVal (field f))) (fmap (pzip ask) (compileE a))
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

-- | Compile a syntax tree with the corresponding environment to an event pattern.
compile :: AST.Tree AST.Event -> Either CompileError (Pattern Event, Bindings)
compile (AST.Tree sm em e) = runC (mkEnv sm em) (compileE e)
