module Mescaline.Quant where

import Data.Default (Default(..))

data Quant = Quant {
    quant :: Double
  , phase :: Double
  } deriving (Eq, Show)

instance Default Quant where
  def = Quant 0 0


