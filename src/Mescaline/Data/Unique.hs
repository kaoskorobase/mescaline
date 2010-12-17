{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Mescaline.Data.Unique (
    Unique(..)
  , Id
  , Namespace
  , mkNamespace
  , fromString
  , unsafeFromString
  , toString
  , fromBinary
  , Map
  , toMap
) where

import           Data.Binary (Put)
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.UUID.V5 (generateNamed)
import qualified System.Random as Random

newtype Id = Id { unId :: UUID } deriving (Eq, Ord, Random.Random, Show)
type Namespace = Id

class Unique a where
    uuid :: a -> Id

instance Unique Id where
    uuid = id

unsafeFromString :: String -> Id
unsafeFromString s = maybe e id (fromString s)
    where e = error ("Couldn't convert String to UUID: " ++ s)

fromString :: String -> Maybe Id
fromString = fmap Id . UUID.fromString

toString :: Id -> String
toString = UUID.toString . unId

mkNamespace :: String -> Namespace
mkNamespace = unsafeFromString

fromBinary :: Namespace -> Put -> Id
fromBinary (Id ns) = Id . generateNamed ns . ByteString.unpack . Binary.runPut

type Map a = Map.Map Id a

toMap :: Unique a => [a] -> Map a
toMap xs = Map.fromList (zip (map uuid xs) xs)
