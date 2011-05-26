module Mescaline.Synth.Sampler.Params
  ( Params(..)
  , defaultParams
  ) where

import           Mescaline (Duration)
import qualified Mescaline.Database as DB

data Params = Params {
    file         :: DB.SourceFile
  , unit         :: DB.Unit
  , offset       :: Duration
  , duration     :: Duration
  , rate         :: Double
  , pan          :: Double
  , attackTime   :: Double
  , releaseTime  :: Double
  , sustainLevel :: Double
  , gateLevel    :: Double
  , sendLevel1   :: Double
  , sendLevel2   :: Double
  , fxParam1     :: Double
  , fxParam2     :: Double
  } deriving (Eq, Show)

defaultParams :: DB.SourceFile -> DB.Unit -> Params
defaultParams f u =
    Params {
        file         = f
      , unit         = u
      , offset       = 0
      , duration     = DB.unitDuration u
      , rate         = 1
      , pan          = 0
      , attackTime   = 0
      , releaseTime  = 0
      , sustainLevel = 1
      , gateLevel    = 0
      , sendLevel1   = 0
      , sendLevel2   = 0
      , fxParam1     = 0
      , fxParam2     = 0
      }
