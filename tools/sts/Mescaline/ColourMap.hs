module Mescaline.ColourMap
  ( ColourMap
  , hsv
  ) where

import           Data.Colour (Colour)
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace as RGB
import qualified Data.Colour.RGBSpace.HSV as RGB
import qualified Data.Map as Map

type ColourMap a b = Map.Map a (Colour b)

hsv :: (Ord a, Floating b, RealFrac b) => [a] -> ColourMap a b
hsv as = Map.fromList (zip as rgbs)
    where
        n = length as
        dh = 360 / fromIntegral n
        hues = take n (iterate (+dh) 0)
        rgbs = map (\h -> RGB.uncurryRGB (RGB.rgbUsingSpace SRGB.sRGBSpace) (RGB.hsv h 1 1)) hues
