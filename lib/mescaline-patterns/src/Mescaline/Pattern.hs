module Mescaline.Pattern (
    -- *Patterns
    module Mescaline.Pattern.Ppar
  , ptrace
    -- *Base module
  , module Sound.SC3.Lang.Pattern.P
) where

import           Mescaline.Pattern.Ppar
import           Sound.SC3.Lang.Pattern.P
import qualified Debug.Trace as Debug

ptrace :: Show a => String -> P a -> P a
ptrace tag = fmap (\a -> Debug.trace (tag ++ show a) a)
