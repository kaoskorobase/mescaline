module Mescaline.Synth.Pattern.Type (
    Pattern
) where

import Data.Signal.SF (SF)
import Mescaline.Synth.Pattern.Environment (Environment)

type Pattern a b = SF Environment a b
