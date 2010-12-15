module Mescaline.FeatureSpace.Unit (
    Feature
  , Unit
  , cons
  , unit
  , id
  , sourceFile
  , onset
  , duration
  , features
  , featureVectors
  , feature
  , value
  -- , withValues
) where

import qualified Data.Map as Map
import           Data.Int (Int64)
import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import qualified Database.Persist as DB
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.Entity as DB
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import           Mescaline.Time (Duration, Time)
import           Prelude hiding (id)

data Unit = Unit {
    sourceFile :: !DB.SourceFile
  , id         :: !Int64
  , unit       :: !DB.Unit
  , features   :: !(Vector DB.Feature)
  } deriving (Show)

instance Eq (Unit) where
    -- (==) a b = Unique.uuid (unit a) == Unique.uuid (unit b)
    (==) a b = id a == id b

cons :: DB.SourceFileMap -> DB.UnitId -> DB.Unit -> [DB.Feature] -> Unit
cons sfs i u fs = Unit sf (DB.fromPersistKey i) u (V.fromList fs)
    where Just sf = Map.lookup (DB.unitSourceFile u) sfs

-- {-# INLINE id #-}
-- id :: Unit -> Unique.Id
-- id = Unit.id . unit

-- {-# INLINE sourceFile #-}
-- sourceFile :: Unit -> SourceFile
-- sourceFile = Unit.sourceFile . unit

{-# INLINE onset #-}
onset :: Unit -> Time
onset = DB.unitOnset . unit

{-# INLINE duration #-}
duration :: Unit -> Duration
duration = DB.unitDuration . unit

featureVectors :: Unit -> Vector (SV.Vector Double)
featureVectors = V.map (DB.toVector . DB.featureValue) . features

{-# INLINE feature #-}
feature :: Int -> Unit -> DB.Feature
feature i = flip (V.!) i . features

{-# INLINE value #-}
value :: Int -> Unit -> SV.Vector Double
value i = DB.toVector . DB.featureValue . feature i

-- {-# INLINE withValues #-}
-- withValues :: Unit -> [Feature.Value] -> Unit
-- withValues u vs = Unit (unit u) (V.zipWith Feature.setValue (V.fromList vs) (features u))
