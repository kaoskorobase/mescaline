module Mescaline.Synth.Pattern.ASTLib (
    module Mescaline.Synth.Pattern.AST
  , constant
) where

import Mescaline.Synth.Pattern.AST
import Prelude hiding (cycle, map, seq, zip)
import qualified Prelude as P

constant :: Real a => a -> Pattern Scalar
constant = cycle . realToFrac
