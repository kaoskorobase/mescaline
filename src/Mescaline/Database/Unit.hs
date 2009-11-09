{-# LANGUAGE TemplateHaskell #-}

module Mescaline.Database.Unit where

import           Data.Accessor.Template (nameDeriveAccessors)
import qualified Data.Binary as Binary
import           Data.List (find)
import           Database.HDBC (SqlValue, fromSql)
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
-- import           Mescaline.Database.Feature (Feature)
-- import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Data.Array.Vector
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Prelude hiding(id)

type Time  = Double
type DTime = Double

data Unit = Unit {
    id         :: Unique.Id,
    sourceFile :: SourceFile,
    onset      :: Time,
    duration   :: DTime
} deriving (Show)

$(nameDeriveAccessors ''Unit (return.(++"_")))

-- data FeatureTable = FeatureTable {
--     features :: [Feature],
--     table    :: [(Unit, Vector Double)]
-- } deriving (Show)

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "6a1f3de2-91fb-43c5-8ee0-8d4fb43d0d20"

instance Unique Unit where
    uuid = id

instance Eq Unit where
    a == b = id a == id b

unsafeCons :: Unique.Id -> SourceFile -> Time -> DTime -> Unit
unsafeCons = Unit

cons :: SourceFile -> Time -> DTime -> Unit
cons sf o d = unsafeCons (Unique.fromBinary namespace p) sf o d
    where p = Binary.put o >> Binary.put d

-- descriptors :: FeatureTable -> [Feature.Descriptor]
-- descriptors = map (Feature.descriptor) . features

--feature :: Feature -> Unit -> Double
--feature f u = indexU (featureValues u) (Feature.column f)
--
--featureVector :: Feature -> Unit -> Vector Double
--featureVector f u = sliceU (featureValues u) (Feature.column f) (Feature.degree f)
