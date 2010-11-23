{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TemplateHaskell #-}

module Mescaline.Database.Feature where

import qualified Data.Binary as Binary
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Mescaline.Data.ListReader as ListReader
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Sql (GetSql)
import qualified Mescaline.Database.Sql as Sql
import           Prelude hiding (id)

data Descriptor = Descriptor {
    id     :: !Unique.Id
  , name   :: !String
  , degree :: !Int
  } deriving (Eq, Show)

newtype FeatureOf = FeatureOf Descriptor
                    deriving (Eq, Show)

data Feature = Feature {
    unit       :: !Unique.Id
  , descriptor :: !Descriptor
  , value      :: !Value
  } deriving (Eq, Show)

type Value = SV.Vector Double

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "be38ae7f-da19-4df8-99b7-e4ca78b28d92"

instance Unique.Unique Descriptor where
    uuid = id

instance Unique.Unique FeatureOf where
    uuid (FeatureOf d) = Unique.uuid d

instance Unique.Unique Feature where
    -- The feature is uniquely identified by the unit
    -- (because there is one table per feature and a unit can contain a given
    -- feature only once).
    uuid = unit

mkDescriptor :: Unique.Id -> String -> Int -> Descriptor
mkDescriptor = Descriptor

consDescriptor :: String -> Int -> Descriptor
consDescriptor n d = mkDescriptor (Unique.fromBinary namespace p) n d
    where p = Binary.put n >> Binary.put d

indices :: Descriptor -> [Int]
indices d = [0..degree d - 1]

cons :: Unique.Id -> Descriptor -> Value -> Feature
cons = Feature

mkFeature :: Unique.Id -> Descriptor -> Value -> Feature
mkFeature = cons

setValue :: Value -> Feature -> Feature
setValue v f = cons (unit f) (descriptor f) v

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
