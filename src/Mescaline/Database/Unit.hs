{-# LANGUAGE TemplateHaskell #-}

module Mescaline.Database.Unit (
    Segmentation(..)
  , Unit, id, sourceFile, segmentation, onset, duration
  , namespace
  , unsafeCons
  , cons
) where

-- import           Data.Accessor.Template (nameDeriveAccessors)
import qualified Data.Binary as Binary
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.SourceFile (SourceFile)
import           Mescaline.Time (Duration, Time)
import           Prelude hiding(id)

data Segmentation = Onset | Beat
                    deriving (Enum, Eq, Read, Show)

data Unit = Unit {
    id           :: !Unique.Id
  , sourceFile   :: !SourceFile
  , segmentation :: !Segmentation
  , onset        :: !Time
  , duration     :: !Duration
  } deriving (Show)

-- $(nameDeriveAccessors ''Unit (return.(++"_")))

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "6a1f3de2-91fb-43c5-8ee0-8d4fb43d0d20"

instance Unique Unit where
    uuid = id

instance Eq Unit where
    a == b = id a == id b

instance Ord Unit where
    compare a b = compare (id a) (id b)

unsafeCons :: Unique.Id -> SourceFile -> Segmentation -> Time -> Duration -> Unit
unsafeCons = Unit

cons :: SourceFile -> Segmentation -> Time -> Duration -> Unit
cons sf s o d = unsafeCons (Unique.fromBinary namespace p) sf s o d
    where p = Binary.put (fromEnum s) >> Binary.put o >> Binary.put d
