module Mescaline.Synth.Pattern.Load (
    loadFile
  , loadDirectory
) where

import           Control.Monad.Trans (liftIO)
import           Language.Haskell.Interpreter (Interpreter, InterpreterError, as)
import qualified Language.Haskell.Interpreter as Interp
import Sound.SC3.Lang.Pattern
import Mescaline.Synth.Pattern
import           System.FilePath
import qualified System.FilePath.Find as Find

loadFile :: FilePath -> IO (Either InterpreterError PCons)
loadFile = Interp.runInterpreter . interpretFile

loadDirectory :: FilePath -> IO [(FilePath, Either InterpreterError PCons)]
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

interpretFile :: FilePath -> Interpreter PCons
interpretFile path = do
    Interp.setImportsQ [
        ("Prelude", Nothing)
      , ("Data.Accessor", Nothing)
      , ("Data.List", Nothing)
      , ("Sound.SC3.Lang.Pattern", Nothing)
      , ("Mescaline.Synth.Pattern", Nothing)
      , ("Mescaline.Database", Nothing)
      , ("Mescaline.Database.Query", Nothing)
      , ("Mescaline.Database.SourceFile", Just "SourceFile")
      , ("Mescaline.Database.Unit", Just "Unit")
      , ("Sound.SC3.Lang.Collection", Nothing)
        ]
    expr <- liftIO $ readFile path
    Interp.interpret expr (as :: PCons)
