{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern (
    Pattern
  , lift
  , arrP
    -- *Reader
  , ask
  -- , local
  , asks
  , askA
    -- *State
  -- , get
  -- , put
  , modify
  , modify_
  -- , gets
    -- *MonadRandom
  , runRand
  , rrandf
  , rrand
  , rrandexp
    -- *Patterns
  -- , module Mescaline.Synth.Pattern.Ppar
    -- *Base module
  -- , module Mescaline.Synth.Pattern.Base
) where

-- import           Control.Applicative
import           Control.Arrow
-- import qualified Control.Monad as M
import qualified Control.Monad.Random as R
-- import           Control.Monad.Reader (MonadReader(..), asks)
-- import           Control.Monad.State (MonadState(..), modify, gets)
import           Data.Accessor
import           Data.Signal.SF
import           Mescaline.Synth.Pattern.Environment (Environment)
-- import           Mescaline.Synth.Pattern.Base

type Pattern a b = SF (Environment, a) (Environment, b)

-- instance Monad (SF s) where
--     return = pure
--     -- Monad m => m a -> (a -> m b) -> m b
--     sf >>= f = SF g
--         where
--             g s =
--                 let (a, _) = runSF sf s
--                     sfb    = f a
--                 in runSF sfb s

-- instance MonadReader s (SF s) where
--     ask = arr fst
--     -- FIXME: Is this correct?!
--     -- local f p = prp $ \s -> (p, f s)

lift :: SF a b -> Pattern a b
lift = second

arrP :: (a -> b) -> Pattern a b
arrP = lift . arr

asks :: (a -> c) -> SF (a, b) (a, c)
asks f = arr $ \(s, _) -> (s, f s)

ask :: SF (a, b) (a, a)
ask = asks id

askA :: Accessor s a -> SF (s, b) (s, a)
askA acc = asks (getVal acc)

modify :: (s -> (b, s)) -> SF (s, a) (s, b)
modify f = arr $ \(s, _) -> let (b, s') = f s in (s', b)

modify_ :: (s -> s) -> SF (s, a) (s, a)
modify_ f = arr $ \(s, a) -> (f s, a)

-- instance MonadState s (P s) where
--     get   = ask
--     put s = prp $ const (prepeat (), s)

runRand :: R.RandomGen s => SF (s, R.Rand s a) (s, a)
-- -- runRand = M.join . fmap (\r -> prp $ \s -> let (a, s') = R.runRand r s in (return a, s'))
runRand = arr $ \(s, r) -> let (a, s') = R.runRand r s in (s', a)

rrandf :: (R.RandomGen s, R.Random a) => 
    (a -> a -> a -> a) -> SF (s, (a, a)) (s, a)
rrandf f = arr (second (\(lo, hi) -> fmap (f lo hi) (R.getRandomR (lo, hi)))) >>> runRand

rrand :: (R.RandomGen s, R.Random a) => 
    SF (s, (a, a)) (s, a)
rrand = rrandf (\_ _ a -> a)

rrandexp :: (R.RandomGen s, Floating a, R.Random a) => 
    SF (s, (a, a)) (s, a)
rrandexp = rrandf (\l r x -> l * (log (r / l) * x))

-- pchoosea :: (R.RandomGen s) => A.Array Int (P s a) -> P s a
-- pchoosea r = prp (\g -> let (i, g') = R.randomR (A.bounds r) g 
--                         in (r A.! i, g'))
-- 
-- pchoose :: R.RandomGen s => [P s a] -> P s a
-- pchoose l = pchoosea (A.listArray (0, length l - 1) l)

-- prand :: R.RandomGen s => [P s a] -> P s Int -> P s a
-- prand p = pseq [pchoose p]

-- pwhite :: (R.RandomGen s, R.Random a) => 
--           P s a -> P s a -> P s Int -> P s a
-- pwhite l r n = prestrict n (M.join (pzipWith prrand l r))

-- pexprand :: (R.RandomGen s, Floating a, R.Random a) => 
--             P s a -> P s a -> P s Int -> P s a
-- pexprand l r n = prestrict n (M.join (pzipWith prrandexp l r))

-- pxrand :: (R.RandomGen s, Eq a) => [P s a] -> P s Int -> P s a
-- pxrand p n = ptake n (prsd (pseq [pchoose p] pinf))
-- 
-- pwrand :: R.RandomGen s => [P s a] -> [P s a] -> P s Int -> P s a
-- pwrand = undefined