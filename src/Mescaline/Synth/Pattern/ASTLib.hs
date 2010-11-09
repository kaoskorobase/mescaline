module Mescaline.Synth.Pattern.ASTLib (
    module Mescaline.Synth.Pattern.AST
  -- , constant
  , sequencer
) where

import Mescaline.Synth.Pattern.AST
import Prelude hiding ( (==), (>), (>=), (<), (<=)
                      , cycle, filter, map, seq, zip )
import qualified Prelude as P

-- constant :: Real a => a -> Pattern Scalar
-- constant = cycle . realToFrac

sequencer :: Pattern Scalar -> Pattern Event -> Pattern Event
sequencer tick e = bind (step 0 1 (set Delta tick e)) $
                    \e' -> filter (get CursorValue e' > 0) e'
