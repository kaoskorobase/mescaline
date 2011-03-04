{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Pattern.Patch (
    Patch
  , sourceCode
  , setSourceCode
  , evalSourceCode
  , sequencer
  , regions
  , setRegions
  , new
  , defaultPatch
  , defaultPatchEmbedded
  , pattern
  , modifySequencer
) where

import           Control.Exception
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Typeable
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import           Mescaline.FeatureSpace.Model (Region, defaultRegions)
import           Mescaline.Pattern
import qualified Mescaline.Pattern.AST as AST
import           Mescaline.Pattern.Compiler (CompileError)
import qualified Mescaline.Pattern.Compiler as Comp
import           Mescaline.Pattern.Event
import qualified Mescaline.Pattern.Interpreter as Interp
import           Mescaline.Pattern.Patch.Default (defaultTree)
import           Mescaline.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Pattern.Sequencer as Sequencer

type SyntaxTree = AST.Tree AST.Event

data Patch = Patch {
    sourceCode :: String
  , syntaxTree :: SyntaxTree
  , sequencer  :: Sequencer
  , regions    :: [Region]
  } deriving (Typeable)

cons :: String -> SyntaxTree -> Sequencer -> [Region] -> Patch
cons = Patch

new :: MonadIO m => String -> Sequencer -> [Region] -> AppT m Patch
new src s rs = do
    res <- Interp.eval src
    case res of
        Left e -> throw (Comp.CompileError e)
        Right ast -> return $ cons src ast s rs

defaultPatch :: MonadIO m => AppT m Patch
defaultPatch = do
    src <- App.getResourcePath "patches/default.hs" >>= liftIO . readFile
    new src (Sequencer.empty n n) rs
    where
        rs = defaultRegions
        n  = length rs

defaultPatchEmbedded :: Patch
defaultPatchEmbedded = cons src tree (Sequencer.empty n n) rs
    where
        (src, tree) = defaultTree
        rs = defaultRegions
        n = length rs

pattern :: Patch -> Either CompileError (Pattern Event, Comp.Bindings)
pattern = Comp.compile . syntaxTree

modifySequencer :: (Sequencer -> Sequencer) -> Patch -> Patch
modifySequencer f p = p { sequencer = f (sequencer p) }

setSourceCode :: String -> Patch -> Patch
setSourceCode src patch = patch { sourceCode = src }

evalSourceCode :: MonadIO m => Patch -> AppT m (Either String Patch)
evalSourceCode patch = do
    res <- Interp.eval (sourceCode patch)
    case res of
        Left e -> return $ Left e
        Right ast -> return $ Right $ patch { syntaxTree = ast }

setRegions :: [Region] -> Patch -> Patch
setRegions rs patch = patch { regions = rs }
