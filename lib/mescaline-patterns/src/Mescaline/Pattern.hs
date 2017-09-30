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
    -- *Base module
  , module Sound.SC3.Lang.Pattern.P
    -- Re-exports from base
  , (<>)
    -- *Events
  , module Mescaline.Pattern.Event
) where

import           Data.Monoid ((<>))
import           GHC.Exts (IsList(..))
import           Mescaline.Pattern.Event
import           Mescaline.Pattern.Ppar
import           Sound.SC3.Lang.Pattern.P
import           Data.String (IsString(..))
import qualified Debug.Trace as Debug

ptrace :: Show a => String -> P a -> P a
ptrace tag = fmap (\a -> Debug.trace (tag ++ show a) a)

type Pattern a = P a

instance IsString (Pattern Event) where
  fromString = return . sound

instance IsList (Pattern Event) where
  type Item (Pattern Event) = Pattern Event
  fromList = pconcat
  toList = (:[])
