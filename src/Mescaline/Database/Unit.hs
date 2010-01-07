{-# LANGUAGE TemplateHaskell #-}

module Mescaline.Database.Unit (
    Segmentation(..)
  , Unit, id, sourceFile, segmentation, onset, duration
  , namespace
  , unsafeCons
  , cons
) where

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

data Segmentation = Onset | Beat deriving (Enum, Eq, Read, Show)

data Unit = Unit {
    id           :: Unique.Id
  , sourceFile   :: SourceFile
  , segmentation :: Segmentation
  , onset        :: Time
  , duration     :: DTime
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

unsafeCons :: Unique.Id -> SourceFile -> Segmentation -> Time -> DTime -> Unit
unsafeCons = Unit

cons :: SourceFile -> Segmentation -> Time -> DTime -> Unit
cons sf s o d = unsafeCons (Unique.fromBinary namespace p) sf s o d
    where p = Binary.put (fromEnum o) >> Binary.put o >> Binary.put d

-- descriptors :: FeatureTable -> [Feature.Descriptor]
-- descriptors = map (Feature.descriptor) . features

--feature :: Feature -> Unit -> Double
--feature f u = indexU (featureValues u) (Feature.column f)
--
--featureVector :: Feature -> Unit -> Vector Double
--featureVector f u = sliceU (featureValues u) (Feature.column f) (Feature.degree f)
