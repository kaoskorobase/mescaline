{-# LANGUAGE CPP
           , ExistentialQuantification
           , FlexibleInstances
           , MultiParamTypeClasses #-}
module Mescaline.Synth.Pattern.Binding (
    Binding
  , mkBinding
  , value
  , pattern
  , BindingMap
  , Bindings
  , empty
  , boolean
  , coord
  , event
  , scalar
  -- , HasBindings(..)
  -- , HasBindingMap(..)
  -- , ofType
  , pbinding
  , pbind
) where

#include "Accessor.h"

import           Data.Accessor
import qualified Data.IntMap as Map
import           Mescaline.Synth.Pattern.Base
import           Mescaline.Synth.Pattern.Event

-- import           Debug.Trace

data Binding s t = Binding { value :: t, pattern :: P s t }

mkBinding :: P s t -> Binding s t
mkBinding = Binding (error "Uninitialized Binding")

type BindingMap s t = Map.IntMap (Binding s t)

-- | Compiler environment.
data Bindings s = Bindings {
    _boolean :: BindingMap s Bool             -- ^Bool pattern bindings.
  , _coord   :: BindingMap s (Double, Double) -- ^Coord pattern bindings.
  , _event   :: BindingMap s Event            -- ^Event pattern bindings.
  , _scalar  :: BindingMap s Double           -- ^Scalar pattern bindings.
  }

ACCESSOR(boolean, _boolean, Bindings s, BindingMap s Bool)
ACCESSOR(coord,   _coord,   Bindings s, BindingMap s (Double, Double))
ACCESSOR(event,   _event,   Bindings s, BindingMap s Event)
ACCESSOR(scalar,  _scalar,  Bindings s, BindingMap s Double)

empty :: Bindings s
empty = Bindings Map.empty Map.empty Map.empty Map.empty

pbinding :: forall s a . Accessor s (BindingMap s a) -> Int -> P s a
pbinding bMap h = punfoldr f ()
    where
        f s _ =
            let bValue = {- trace ("pbinding: " ++ show h) $ -} fmap value . Map.lookup h $ s ^. bMap
            in maybe (s, Nothing) (\v -> (s, Just (v, ()))) bValue

pbind :: forall s a b . (Accessor s (BindingMap s a)) -> Int -> P s b -> P s b
pbind bMap h = punfoldr f
    where
        f s pb =
            let binding = maybe
                            (error $ "pbind: Binding not fount: " ++ show h)
                            id
                            ({- trace ("pbind: " ++ show h) $ -} Map.lookup h (s ^. bMap))
            in case step s (pattern binding) of
                Done s' ->
                    (bMap ^: Map.delete h $ s', Nothing)
                Result s' a p' ->
                    case step (bMap ^:
                                {- trace ("pbind: update " ++ show h) -}
                                (Map.insert h (Binding a p')) $ s')
                               pb of
                        Done s''         -> (s'', Nothing)
                        Result s'' b pb' -> (s'', Just (b, pb'))
