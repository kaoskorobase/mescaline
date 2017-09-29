{-# LANGUAGE DeriveDataTypeable #-}
module Mescaline.Pattern.Interpreter where

import           Language.Haskell.Interpreter as Interp
import qualified Mescaline.Pattern as P

compilePattern :: String -> IO (Either InterpreterError (P.Pattern P.Event))
compilePattern code =
  runInterpreter $ do
    Interp.set [ languageExtensions := [ OverloadedLists, OverloadedStrings ] ]
    setImportsQ [
        ("Prelude", Nothing)
      , ("Control.Lens", Nothing)
      , ("Mescaline.Pattern", Nothing)
      ]
    interpret code (undefined :: P.Pattern P.Event)
