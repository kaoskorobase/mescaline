module Mescaline.Database.FeatureDescriptor (
    FeatureDescriptor(..)
) where

-- import Mescaline.Database.SqlRow            (SqlRow(..), fromSql, toSql)
import Mescaline.Database.Unique            (Unique)
import qualified Mescaline.Database.Unique  as Unique
import Prelude                              hiding (id)

data FeatureDescriptor = FeatureDescriptor {
    id          :: Unique.Id,
    name        :: String,
    description :: String
} deriving (Show)

instance Unique (FeatureDescriptor) where
    uid = id

-- instance SqlRow (FeatureDescriptor) where
--     toSqlRow x =  map ($x) [ toSql.id, toSql.name ]
--     fromSqlRow [_id, _name] = FeatureDescriptor
--                                 (fromSql _id)
--                                 (fromSql _name)
--                                 "no description yet"
