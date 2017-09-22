{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Mescaline.Pattern (
    -- *Patterns
    Pattern
  , module Mescaline.Pattern.Ppar
  , ptrace
  , unPE
    -- *Base module
  , module Sound.SC3.Lang.Pattern.P
    -- *Events
  , module Mescaline.Pattern.Event
, loop
) where

import           GHC.Exts (IsList(..))
import           Mescaline.Pattern.Event
import           Mescaline.Pattern.Ppar
import           Sound.SC3.Lang.Pattern.P
import           Data.String (IsString(..))
import qualified Debug.Trace as Debug

ptrace :: Show a => String -> P a -> P a
ptrace tag = fmap (\a -> Debug.trace (tag ++ show a) a)

newtype Pattern a = Pattern (P a) deriving (Eq, Functor, Monoid, Show)

instance IsString (Pattern Event) where
  fromString = Pattern . return . sound

instance IsList (Pattern Event) where
  type Item (Pattern Event) = Event
  fromList = Pattern . toP
  toList (Pattern p) = unP p

unPE :: Pattern Event -> [Event]
unPE = toList

loop :: Pattern Event -> Pattern Event
loop (Pattern p) = Pattern (pcycle p)

