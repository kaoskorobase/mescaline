module Mescaline.Synth.FeatureSpace.Unit (
    Feature
  , Unit
  , cons
  , unit
  , id
  , sourceFile
  , onset
  , duration
  , features
  , feature
  , value
  , withValues
) where

import           Data.Vector (Vector)
import qualified Data.Vector.Generic as V
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import           Mescaline.Time (Duration, Time)
import           Prelude hiding (id)

data Unit = Unit {
    unit     :: !Unit.Unit
  , features :: !(Vector Feature.Feature)
  } deriving (Show)

cons :: Unit.Unit -> [Feature.Feature] -> Unit
cons u = Unit u . V.fromList

{-# INLINE id #-}
id :: Unit -> Unique.Id
id = Unit.id . unit

{-# INLINE sourceFile #-}
sourceFile :: Unit -> SourceFile
sourceFile = Unit.sourceFile . unit

{-# INLINE onset #-}
onset :: Unit -> Time
onset = Unit.onset . unit

{-# INLINE duration #-}
duration :: Unit -> Duration
duration = Unit.duration . unit

{-# INLINE feature #-}
feature :: Int -> Unit -> Feature
feature i = flip (V.!) i . features

{-# INLINE value #-}
value :: Int -> Unit -> Feature.Value
value i = Feature.value . feature i

{-# INLINE withValues #-}
withValues :: Unit -> [Feature.Value] -> Unit
withValues u vs = Unit (unit u) (V.zipWith Feature.setValue (V.fromList vs) (features u))
