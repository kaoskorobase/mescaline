module Mescaline.Data.Unique (
    Unique(..)
  , UUID, Id
  , generateNamed
  , mapFromList
) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Data.UUID.V5 (generateNamed)
import           Database.HDBC (SqlType(..))

-- | Obsolete.
type Id = UUID

class Unique a where
    uuid :: a -> UUID

instance SqlType UUID where
    fromSql s = case UUID.fromString (fromSql s) of
                    Nothing -> error "Couldn't convert SqlType to UUID"
                    Just u  -> u
    toSql = toSql . UUID.toString
    
-- | Obsolete.
uid :: Unique a => a -> UUID
uid = uuid

toMap :: Unique a => [a] -> Map UUID a
toMap xs = Map.fromList (zip (map uuid xs) xs)

mapFromList :: Unique a => [a] -> Map UUID a
mapFromList = toMap
