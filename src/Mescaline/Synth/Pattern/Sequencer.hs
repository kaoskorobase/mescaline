module Mescaline.Synth.Pattern.Sequencer (
    Sequencer
  , numTracks
  , trackLength
  -- , cons
  , empty
) where

import           Data.Accessor
import qualified Data.IntMap as Map
import           Mescaline.Synth.Pattern.Track (Track)
import qualified Mescaline.Synth.Pattern.Track as Track
import           Prelude hiding (length, lookup)

type Cursor = Int

data Sequencer = Sequencer {
    numTracks   :: Int
  , trackLength :: Int
  , tracks  :: [Track]
  } deriving (Eq, Show)

-- cons :: [Track] -> Sequencer
-- cons = Sequencer

empty :: Int -> Int -> Sequencer
empty n m = Sequencer n m $ replicate n (Track.cons m 0 m 0)
