{-# LANGUAGE TypeSynonymInstances #-}

module Mescaline.Data.Unique (
    Unique(..)
  , Id
  , Namespace
  , mkNamespace
  , fromString
  , unsafeFromString
  , toString
  , fromBinary
  , mapFromList
) where

import           Data.Binary (Binary, Put)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UUID (UUID, fromString, toString)
import qualified Data.UUID as UUID
import           Data.UUID.V5 (generateNamed)
import           Database.HDBC (SqlType(..))

type Id = UUID
type Namespace = UUID

class Unique a where
    uuid :: a -> Id

instance SqlType UUID where
    fromSql s = case UUID.fromString (fromSql s) of
                    Nothing -> error "Couldn't convert SqlType to UUID"
                    Just u  -> u
    toSql = toSql . UUID.toString

-- class Binary a where
--     put :: a -> Binary.Put
-- 
-- instance Binary Int where
--     put = Binary.put
-- 
-- instance Binary Double where
--     put = Binary.put
-- 
-- instance Binary String where
--     put = Binary.put

unsafeFromString :: String -> Id
unsafeFromString s = maybe e id (fromString s)
    where e = error ("Couldn't convert String to UUID: " ++ s)

mkNamespace :: String -> Namespace
mkNamespace = unsafeFromString

toMap :: Unique a => [a] -> Map Id a
toMap xs = Map.fromList (zip (map uuid xs) xs)

mapFromList :: Unique a => [a] -> Map Id a
mapFromList = toMap

fromBinary :: Namespace -> Put -> Id
fromBinary ns = generateNamed ns . ByteString.unpack . Binary.runPut
