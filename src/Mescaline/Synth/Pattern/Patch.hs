{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Synth.Pattern.Patch (
    Patch
  , code
  , sequencer
  , regions
  , newPatch
  , defaultPatch
  , pattern
) where

import           Control.Applicative
import           Control.Exception
import           Data.Accessor
import           Data.Typeable
import qualified Mescaline.Application as App
import           Mescaline.Synth.FeatureSpace.Model (Region, defaultRegions)
import           Mescaline.Synth.Pattern
import qualified Mescaline.Synth.Pattern.AST as AST
import           Mescaline.Synth.Pattern.Compiler (CompileError)
import qualified Mescaline.Synth.Pattern.Compiler as Comp
import           Mescaline.Synth.Pattern.Event
import qualified Mescaline.Synth.Pattern.Interpreter as Interp
import           Mescaline.Synth.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import qualified Mescaline.Time as Time

type SyntaxTree = AST.Tree AST.Event

data Patch = Patch {
    code       :: String
  , syntaxTree :: SyntaxTree
  , sequencer  :: Sequencer
  , regions    :: [Region]
  } deriving (Typeable)

cons :: String -> SyntaxTree -> Sequencer -> [Region] -> Patch
cons = Patch

newPatch :: String -> Sequencer -> [Region] -> IO Patch
newPatch src sequencer regions = do
    res <- Interp.eval src
    case res of
        Left e -> throw (Comp.CompileError e)
        Right ast -> return $ cons src ast sequencer regions

defaultPatch :: IO Patch
defaultPatch = do
    src <- App.getResourcePath "patches/default.hs" >>= readFile
    newPatch src (Sequencer.empty n n) rs
    where
        rs = defaultRegions
        n  = length rs

pattern :: Patch -> Either CompileError (Pattern Event)
pattern = Comp.compile . syntaxTree
