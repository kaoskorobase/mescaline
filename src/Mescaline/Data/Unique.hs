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

import           Data.Binary (Binary, Put)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.UUID.V5 (generateNamed)
import           Database.HDBC (SqlType(..))
import qualified System.Random as Random

newtype Id = Id { unId :: UUID } deriving (Eq, Ord, Random.Random, Show)
type Namespace = Id

class Unique a where
    uuid :: a -> Id

instance Unique Id where
    uuid = id

instance SqlType Id where
    fromSql s = case UUID.fromString (fromSql s) of
                    Nothing -> error ("Couldn't convert SqlType to UUID: " ++ show s)
                    Just u  -> Id u
    toSql = toSql . UUID.toString . unId

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
