module Mescaline.Pattern.Interpreter (
    eval
) where

import qualified Mescaline.Application as App
import           Mescaline.Hugs (Module(..), Import(..))
import qualified Mescaline.Hugs as Hugs
import           Mescaline.Pattern.AST
import           System.FilePath

eval :: String -> IO (Either String (Tree Event))
eval src = do
    hugsDir <- App.getResourcePath "hugs/packages"
    srcDir  <- App.getResourcePath "src"
    let opts = [ Hugs.Haskell98 False
               , Hugs.SearchPath [ hugsDir </> "hugsbase"
                                 , hugsDir </> "base"
                                 , hugsDir </> "mtl"
                                 , srcDir ]
               , Hugs.ShowLoadedFiles True ]
        mods = [ Module "Control.Applicative" Unqual
               , Module "Mescaline.Pattern.AST.Library" Unqual
               , Module "Prelude" (Hiding [ "cycle", "filter", "min", "map", "max", "replicate", "seq", "take", "zip" ])
               , Module "Prelude" (Qual "P")
               , Module "Data.List" (Qual "List") ]
    Hugs.eval opts mods src
