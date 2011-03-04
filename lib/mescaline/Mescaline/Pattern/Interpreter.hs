module Mescaline.Pattern.Interpreter (
    eval
) where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.Hugs (Module(..), Import(..))
import qualified Mescaline.Hugs as Hugs
import           Mescaline.Pattern.AST
import           System.FilePath

eval :: MonadIO m => String -> AppT m (Either String (Tree Event))
eval src = do
    exe <- App.findExecutable "usr/local/bin/runhugs"
    case exe of
        Nothing -> return $ Left "Couldn't find the `runhugs' executable; maybe you haven't installed Hugs?"
        Just runhugs -> do
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
            liftIO $ Hugs.eval runhugs opts mods src
