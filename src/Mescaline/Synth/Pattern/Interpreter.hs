module Mescaline.Synth.Pattern.Interpreter (
    eval
) where

import qualified Mescaline.Application as App
import           Mescaline.Hugs (Module(..), Import(..))
import qualified Mescaline.Hugs as Hugs
import           Mescaline.Synth.Pattern.AST
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
               , Module "Mescaline.Synth.Pattern.ASTLib" Unqual
               , Module "Prelude" (Hiding [ "cycle", "filter", "map", "replicate", "seq", "take", "zip" ])
               , Module "Prelude" (Qual "P")
               , Module "Data.List" (Qual "List") ]
    Hugs.eval opts mods src

-- import Mescaline.Synth.Pattern.AST
-- import Mescaline.Util (readMaybe)
-- 
-- import Qtc.Classes.Qccs
-- import Qtc.Classes.Qccs_h
-- import Qtc.Classes.Core
-- import Qth.ClassTypes.Core
-- import Qth.Core
-- import Qtc.Classes.Base
-- import Qtc.Enums.Base
-- import Qtc.ClassTypes.Core
-- import Qtc.ClassTypes.Script
-- import Qtc.Core.Base
-- import Qtc.Enums.Core.Qt
-- import Qtc.Core.QIODevice
-- import Qtc.Enums.Core.QIODevice
-- import Qtc.Core.QFile
-- import Qtc.ClassTypes.Gui
-- import Qtc.Gui.Base
-- import Qtc.Core.QTextStream
-- import Qtc.ClassTypes.Tools
-- import Qtc.Classes.Script
-- 
-- evaluateFile :: QScriptEngine () -> String -> IO ()
-- evaluateFile _engine _script = do
--     scriptFile <- qFile _script
--     open scriptFile fReadOnly
--     textStream <- qTextStream scriptFile
--     evaluate _engine =<< readAll textStream ()
--     close scriptFile ()
--     return ()
-- 
-- evaluateString :: String -> IO String
-- evaluateString src = do
--     engine <- qScriptEngine ()
--     evaluateFile engine "pattern.js"
--     res <- evaluate engine src
--     toScriptString res
-- 
-- evaluateAST :: String -> IO (Either String PFile)
-- evaluateAST src = do
--     expr <- evaluateString $ "try { \"Right (\" + (" ++ src ++ ").toString() + \")\" } catch (e) { \"Left (\" + e.toString() + \")\" }"
--     case readMaybe expr of
--         Nothing -> return $ Left "Parse error"
--         Just e  -> return e
