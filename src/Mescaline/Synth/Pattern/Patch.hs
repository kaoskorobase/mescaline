{-# LANGUAGE DeriveDataTypeable
           , FlexibleInstances #-}
module Mescaline.Synth.Pattern.Patch (
    Patch
  , cons
  , mkDefault
  , sequencer
  , regions
  , getPattern
  , MkTracks
  , tracks
) where

import           Control.Applicative
import           Control.Exception
import           Data.Accessor
import           Data.Typeable
import           Language.Haskell.Interpreter (Interpreter, InterpreterError, OptionVal((:=)), as)
import qualified Language.Haskell.Interpreter as Interp
import qualified Language.Haskell.Interpreter.Unsafe as Interp
import           Mescaline.Synth.FeatureSpace.Model (Region, defaultRegions)
import           Mescaline.Synth.Pattern
import           Mescaline.Synth.Pattern.Event
import           Mescaline.Synth.Pattern.Sequencer (Sequencer)
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import qualified Mescaline.Time as Time

data Patch = Patch {
    sourceCode :: String
  , sequencer  :: Sequencer
  , regions    :: [Region]
  } deriving (Typeable)

data MkTracks = MkTracks (Int -> Pattern () (Maybe Event))
                deriving (Typeable)

tracks :: (Int -> Pattern () (Maybe Event)) -> MkTracks
tracks = MkTracks

cons :: String -> Sequencer -> [Region] -> Patch
cons = Patch

mkDefault :: String -> Patch
mkDefault source = cons source (Sequencer.empty n n) regions
    where
        regions = defaultRegions
        n       = length regions

getPattern :: Patch -> IO (Pattern () (Maybe Event))
getPattern patch = do
    putStrLn (sourceCode patch)
    e <- loadString (sourceCode patch)
    case e of
        Left err ->
            throw err
        Right (MkTracks mkTracks) -> do
            let n = Sequencer.rows (sequencer patch)
            return $ par (map mkTracks [0..n-1])

-- serialize :: Patch -> String
-- serialize p = undefined
-- 
-- unserialize :: String -> Maybe Patch
-- unserialize s = undefined

loadString :: String -> IO (Either InterpreterError MkTracks)
loadString = Interp.runInterpreter . interpret

interpret :: String -> Interpreter MkTracks
interpret expr = do
    -- Interp.set [ Interp.searchPath := ["src", "include"]
    --            , Interp.installedModulesInScope := False ]
    -- Interp.loadModules ["Mescaline.Synth.Pattern"]
    -- Interp.unsafeSetGhcOption "-hide-package monads-fd"
    -- TODO: Make this configurable.
    Interp.setImportsQ [
        ("Prelude", Nothing)
      , ("Control.Applicative", Nothing)
      , ("Control.Arrow", Nothing)
      , ("Data.Accessor", Nothing)
      , ("Data.List", Just "L")
      , ("Data.Maybe", Nothing)
      , ("Mescaline.Synth.Pattern", Nothing)
      , ("Mescaline.Synth.Pattern.DefaultPatch", Nothing)
      , ("Mescaline.Synth.Pattern.Patch", Nothing)
      -- , ("Mescaline.Synth.Pattern.Patch", Just "Patch")
      , ("Mescaline.Synth.Pattern.Event", Nothing)
      -- , ("Sound.SC3.Lang.Collection", Nothing)
        ]
    Interp.interpret expr (as :: MkTracks)
