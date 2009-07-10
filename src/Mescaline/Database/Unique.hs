module Mescaline.Database.Unique (
    Unique(..),
    Id,
    mapFromList
) where

import Data.Map (Map)
import qualified Data.Map as Map

type Id = Int

class Unique a where
    uid :: a -> Id

mapFromList :: Unique a => [a] -> Map Id a
mapFromList xs = Map.fromList (zip (map uid xs) xs)
