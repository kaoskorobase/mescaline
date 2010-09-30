module Mescaline.UI (
    qColorFromRgbaF
  , colorsFromFile
  , defaultColorsFromFile
) where

import           Data.Bits
import qualified Mescaline.Application as App
import           Qtc.ClassTypes.Gui
import           Qtc.Gui.QColor

qColorFromRgbaF :: (Double, Double, Double, Double) -> IO (QColor ())
qColorFromRgbaF (r, g, b, a) = qColorFromRgba
                                ( (shiftL (round (a*255)) 24)
                              .|. (shiftL (round (r*255)) 16)
                              .|. (shiftL (round (g*255)) 8)
                              .|. (shiftL (round (b*255)) 0) )


colorsFromFile :: FilePath -> IO [QColor ()]
colorsFromFile path = do
    s <- readFile path
    mapM (f.words) (lines s)
    where
        f [cr, cg, cb, ca] = qColorFromRgbaF (read cr, read cg, read cb, read ca)
        f _                = error "colorsFromFile: parse error"

defaultColorsFromFile :: IO [QColor ()]
defaultColorsFromFile = colorsFromFile =<< App.getResourcePath "colors.txt"
