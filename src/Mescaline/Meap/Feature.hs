module Mescaline.Meap.Feature (
    defaultFeatures
) where

import qualified Sound.Analysis.Meapsoft as Meap

defaultFeatures :: [String]
defaultFeatures = [
      "AvgChroma"
    , "AvgChromaScalar"
    , "AvgChunkPower"
    , "AvgFreqSimple"
    , "AvgMelSpec"
    , "AvgMFCC"
    , "AvgPitch"
    , "AvgSpec"
    , "AvgSpecCentroid"
    , "AvgSpecFlatness"
    , "AvgTonalCentroid"
    , "ChunkLength"
    , "ChunkStartTime"
    , "Entropy"
    , "RMSAmplitude"
    , "SpectralStability"
    ]
