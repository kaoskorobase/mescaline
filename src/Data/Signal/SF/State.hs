{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Data.Signal.SF.State (
    SF
  , Base.runSF
  , runState
  , module Control.Arrow.Transformer.State
  , module Control.Arrow.Operations
) where

import           Control.Applicative
import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.State hiding (runState)
import qualified Control.Arrow.Transformer.State as State
import           Control.CCA.Types
import           Control.CCA.ArrowP
import           Control.Category
import           Data.Zip (Zip(..))
import qualified Euterpea.Signal.SF as Base
import           Prelude hiding ((.), init, zip)

newtype SF s a b = SF { unSF :: StateArrow s Base.SF a b }
                   deriving (Applicative, Arrow, ArrowChoice, ArrowLoop, Category, Functor)

instance ArrowInit (SF s) where
    init = liftState . init
    -- FIXME: Compiles, but is it correct?
    loopD i g = SF $ liftState $ loopD i g
        -- where f ((b, s), e) = let (c, e') = g (b, e) in ((c, s), e')

-- FIXME: Is this correct?
instance ArrowInitP (SF s) p

instance ArrowState s (SF s) where
    fetch = SF fetch
    store = SF store
    
instance ArrowAddState s (SF s) Base.SF where
    liftState = SF . liftState
    elimState = elimState . unSF

instance Zip (SF s a) where
    zip sf = (<*>) (pure (,) <*> sf)

runState :: SF s a b -> Base.SF (a, s) (b, s)
runState = State.runState . unSF
