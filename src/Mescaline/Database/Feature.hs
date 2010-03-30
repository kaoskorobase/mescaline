{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TemplateHaskell #-}

module Mescaline.Database.Feature where

import           Data.Accessor.Template (nameDeriveAccessors)
import           Data.Accessor (Accessor, (.>))
import           Data.Accessor.Tuple
import qualified Data.Binary as Binary
import qualified Data.Vector.Unboxed as V
import qualified Mescaline.Data.ListReader as ListReader
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.Sql (GetSql)
import qualified Mescaline.Database.Sql as Sql
import           Prelude hiding (id)

newtype Descriptor = Descriptor { unDescriptor :: (Unique.Id, String, Int) } deriving (Eq, Show)
newtype FeatureOf  = FeatureOf Descriptor deriving (Eq, Show)
newtype Feature    = Feature    { unFeature :: (Unique.Id, Descriptor, Value) } deriving (Eq, Show)
type    Value      = V.Vector Double

$(nameDeriveAccessors ''Descriptor (return.(++"_")))
$(nameDeriveAccessors ''Feature (return.(++"_")))

id_     = unDescriptor_ .> first3
name_   = unDescriptor_ .> second3
degree_ = unDescriptor_ .> third3

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "be38ae7f-da19-4df8-99b7-e4ca78b28d92"

instance Unique.Unique Descriptor where
    uuid (Descriptor (u, _, _)) = u

instance Unique.Unique FeatureOf where
    uuid (FeatureOf d) = Unique.uuid d

instance Unique.Unique Feature where
    -- The feature is uniquely identified by the unit
    -- (because there is one table per feature and a unit can contain a given
    -- feature only once).
    uuid (Feature (u, _, _)) = Unique.uuid u

mkDescriptor :: Unique.Id -> String -> Int -> Descriptor
mkDescriptor i n d = Descriptor (i, n, d)

consDescriptor :: String -> Int -> Descriptor
consDescriptor n d = mkDescriptor (Unique.fromBinary namespace p) n d
    where p = Binary.put n >> Binary.put d

id :: Descriptor -> Unique.Id
id (Descriptor (i, _, _)) = i

name :: Descriptor -> String
name (Descriptor (_, n, _)) = n

degree :: Descriptor -> Int
degree (Descriptor (_, _, d)) = d

indices :: Descriptor -> [Int]
indices d = [0..degree d - 1]

cons :: Unique.Id -> Descriptor -> Value -> Feature
cons u d v = Feature (u, d, v)

mkFeature :: Unique.Id -> Descriptor -> Value -> Feature
mkFeature = cons

unit :: Feature -> Unique.Id
unit (Feature (u, _, _)) = u

descriptor :: Feature -> Descriptor
descriptor (Feature (_, d, _)) = d

value :: Feature -> Value
value (Feature (_, _, v)) = v

-- column :: Feature -> Int
-- column (Feature (_, c)) = c
-- 
-- slice :: Feature -> (Int, Int)
-- slice f = (column f, (degree.descriptor)f)

sqlTableName :: Descriptor -> String
sqlTableName f = "feature_" ++ (map tr $ name f)
    where
        tr '.' = '_'
        tr c   = c

sqlColumnNames :: Descriptor -> [String]
sqlColumnNames = map (("value_" ++) . show) . indices

getSql :: Unique.Id -> Descriptor -> GetSql Feature
getSql u d = ListReader.take (degree d) >>= return . mkFeature u d . V.fromList . map Sql.fromSql
