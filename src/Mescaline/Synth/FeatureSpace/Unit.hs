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

import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import           Mescaline.Time (Duration, Time)
import           Prelude hiding (id)

data Unit = Unit {
    unit :: Unit.Unit
  , features :: [Feature.Feature]
  } deriving (Show)

cons :: Unit.Unit -> [Feature.Feature] -> Unit
cons = Unit

id :: Unit -> Unique.Id
id = Unit.id . unit

sourceFile :: Unit -> SourceFile
sourceFile = Unit.sourceFile . unit

onset :: Unit -> Time
onset = Unit.onset . unit

duration :: Unit -> Duration
duration = Unit.duration . unit

feature :: Int -> Unit -> Feature
feature i = flip (!!) i . features

value :: Int -> Unit -> Feature.Value
value i = Feature.value . feature i

withValues :: Unit -> [Feature.Value] -> Unit
withValues u vs = cons (unit u) (zipWith Feature.setValue vs (features u))
