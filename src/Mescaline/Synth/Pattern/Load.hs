{-# LANGUAGE DeriveDataTypeable #-}
module Mescaline.Synth.Pattern.Load (
    MakePatch
  , makePatch
  , loadFile
  , loadDirectory
) where

import           Control.Monad.Trans (liftIO)
import           Data.Typeable
import           Language.Haskell.Interpreter (Interpreter, InterpreterError, as)
import qualified Language.Haskell.Interpreter as Interp
import           Mescaline.Synth.Pattern.Environment
import           Mescaline.Synth.Pattern.Patch
import           System.FilePath
import qualified System.FilePath.Find as Find

data MakePatch = MakePatch (Environment -> Patch) deriving (Typeable)

makePatch :: (Environment -> Patch) -> MakePatch
makePatch = MakePatch

loadFile :: FilePath -> IO (Either InterpreterError MakePatch)
loadFile = Interp.runInterpreter . interpretFile

loadDirectory :: FilePath -> IO [(FilePath, Either InterpreterError MakePatch)]
loadDirectory path = do
    files <- Find.find
                Find.always
                (fmap (== ".hs") Find.extension)
                path
    zip files `fmap` mapM loadFile files

-- interpretModule :: String -> FilePath -> Interpreter PCons
-- interpretModule expr modPath = do
--     Interp.setImportsQ [
--         ("Prelude", Nothing),
--         ("Sound.SC3.Lang.Pattern", Nothing),
--         ("Mescaline.Synth.Pattern", Nothing),
--         ("Mescaline.Database", Nothing),
--         ("Mescaline.Database.Query", Nothing),
--         ("Mescaline.Database.SourceFile", Just "SourceFile"),
--         ("Mescaline.Database.Unit", Just "Unit")
--         ]
-- 
--     Interp.loadModules [modPath]
--     Interp.setTopLevelModules [(takeFileName.dropExtension) modPath]
-- 
--     Interp.interpret expr (as :: PCons)

interpretFile :: FilePath -> Interpreter MakePatch
interpretFile path = do
    Interp.setImportsQ [
        ("Prelude", Nothing)
      , ("Data.Accessor", Nothing)
      , ("Data.List", Nothing)
      -- , ("Sound.SC3.Lang.Pattern", Nothing)
      , ("Mescaline.Synth.Pattern", Nothing)
      , ("Mescaline.Database.FlatFile", Nothing)
      , ("Mescaline.Database.Query", Nothing)
      , ("Mescaline.Database.SourceFile", Just "SourceFile")
      , ("Mescaline.Database.Unit", Just "Unit")
      , ("Sound.SC3.Lang.Collection", Nothing)
        ]
    expr <- liftIO $ readFile path
    Interp.interpret expr (as :: MakePatch)
