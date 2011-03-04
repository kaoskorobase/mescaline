{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , ScopedTypeVariables #-}
module Data.Aeson.Config
    (
        Config
      , runConfig
      , option
      , section
      , Configurable(..)
      , applyConfigFile
      , applyConfigFiles
    ) where

import           Control.Applicative
import           Control.Exception (Exception, SomeException, try)
import           Control.Failure (Failure(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Accessor
import           Data.Accessor.Basic (compose, modify)
import           Data.Aeson as A
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Lazy as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.DList as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)

type Seq a = Seq.DList a
newtype Config s a = Config (ReaderT (Object, Seq Text) (WriterT (Seq (s -> s)) Parser) a)
                        deriving (Applicative, Functor, Monad)

pathToString :: Seq Text -> String
pathToString path = List.intercalate "." (map Text.unpack (Seq.toList path))

execConfig :: Config s a -> Value -> Seq Text -> Parser (s -> s)
execConfig (Config c) (Object object) path = do
    updates <- execWriterT (runReaderT c (object, path))
    return $ compose (Seq.toList updates)
execConfig _ value path =
    fail ("Invalid value `" ++ show value ++ "' for section `" ++ pathToString path ++ "'")

option :: FromJSON a => Text.Text -> Accessor s a -> Config s ()
option field acc = Config $ do
    (object, path) <- ask
    value <- lift (lift (object .:? field <|> fail ("Invalid value for `" ++ pathToString (Seq.snoc path field) ++ "'")))
    case value of
        Nothing -> return ()
        Just a -> lift (tell (Seq.singleton (setVal acc a)))

section :: Text.Text -> Accessor s s' -> Config s' () -> Config s ()
section field acc action = Config $ do
    (object, path) <- ask
    value <- lift (lift (object .:? field <|> fail ("Could not convert to expected type")))
    case value of
        Just value' -> do
            update <- lift $ lift $ execConfig action value' (Seq.snoc path field)
            lift $ tell (Seq.singleton (modify acc update))
        Nothing -> return ()

class Configurable a where
    config :: Config a ()

data ConfigFailure =
    ConfigFailure String
  | ParseFailure String
    deriving (Eq, Show, Typeable)

instance Exception ConfigFailure

runConfig :: (Monad m, Failure ConfigFailure m) => Config s a -> Value -> m (s -> s)
runConfig c v =
    case A.parse (const (execConfig c v Seq.empty)) () of
        Success f -> return f
        Error e   -> failure (ConfigFailure e)

readConfigFile :: (Configurable a, Failure ConfigFailure IO) => FilePath -> IO (a -> a)
readConfigFile path = do
    e <- try (B.readFile path)
    case e of
        Left (_::SomeException) -> return id
        Right b ->
            case P.parse A.json (BL.fromChunks [b]) of
                P.Fail _ _ e -> failure (ParseFailure e)
                P.Done _ value -> runConfig config value

applyConfigFile :: (Configurable a, Failure ConfigFailure IO) => FilePath -> a -> IO a
applyConfigFile path a = readConfigFile path >>= \f -> return (f a)

applyConfigFiles :: (Configurable a, Failure ConfigFailure IO) => [FilePath] -> a -> IO a
applyConfigFiles paths a = do
    fs <- mapM readConfigFile paths
    return $ compose fs a
