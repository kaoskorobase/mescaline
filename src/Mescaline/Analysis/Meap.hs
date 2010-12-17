module Mescaline.Analysis.Meap (
    Meap
  , analyser
) where

import           Control.Arrow (first)
import           Mescaline.Analysis.Types
import qualified Mescaline.Application.Logger as Log
import           Mescaline.Analysis.Meap.Chain as Chain
import qualified Mescaline.Analysis.Meap.Extractor as Extractor
import           Mescaline.Analysis.Meap.Process (OutputHandler(..))
import qualified Mescaline.Analysis.Meap.Segmenter as Segmenter
import qualified Sound.Analysis.Meapsoft as Meap

meapFeaturePrefix :: String
meapFeaturePrefix = "com.meapsoft."

meapFeatures :: [(String, Int)]
meapFeatures = map (first (meapFeaturePrefix++)) [
  --   ( "AvgChroma"         , 12  )
  -- , ( "AvgChromaScalar"   , 1   )
    ( "AvgChunkPower"     , 1   )
  , ( "AvgFreqSimple"     , 1   )
  -- , ( "AvgMelSpec"        , 40  )
  , ( "AvgMFCC"           , 13  )
  -- , ( "AvgPitch"          , 1   )
  -- , ( "AvgSpecCentroid"   , 1   )
  -- , ( "AvgSpecFlatness"   , 1   )
  -- , ( "AvgTonalCentroid"  , 6   )
  -- , ( "ChunkLength"       , 1   )
  -- , ( "ChunkStartTime"    , 1   )
  -- , ( "Entropy"           , 1   )
  -- , ( "RMSAmplitude"      , 1   )
  -- , ( "SpectralStability" , 1   )
  ]

meapOptions :: Chain.Options
meapOptions =
    Chain.defaultOptions {
        segmenter = Segmenter.defaultOptions {
            Segmenter.outputHandler = outputHandler
          , Segmenter.segmentation = Segmenter.Onset
          , Segmenter.smoothingWindow = 0.01
        }
      , extractor = Extractor.defaultOptions {
            Extractor.outputHandler = outputHandler
          , Extractor.windowSize = 1024
          , Extractor.hopSize = 512
          , Extractor.features  = map fst meapFeatures } }
    where
        outputHandler = OutputHandler (Log.noticeM "Database") (Log.errorM "Database")

convUnit :: (Double, Double) -> Unit
convUnit = uncurry Unit

convDescriptor :: Meap.Feature -> Descriptor
convDescriptor f = Descriptor
                       (meapFeaturePrefix ++ (Meap.feature_name f))
                       (Meap.feature_degree f)

convFeature :: Meap.Feature -> [Double] -> Feature
convFeature f l = Feature (convDescriptor f) v
    where v = take (Meap.feature_degree f) $ drop (Meap.feature_column f) l

meapFrames :: Meap.MEAP -> [[Double]]
meapFrames meap = map (Meap.frame_l meap) [0..Meap.n_frames meap - 1]

convMeap :: Meap.MEAP -> [(Unit, [Feature])]
convMeap meap = zip us (map (\v -> map (flip convFeature v) fs) (meapFrames meap))
    where
        fs = Meap.features meap
        us = map convUnit (Meap.segments_l meap)

data Meap = Meap

analyser :: Meap
analyser = Meap

instance Analyser Meap where    
    -- | List of audio file extensions Meap can handle at the moment.
    fileExtensions = const ["aif", "aiff", "wav"]
    analyse _ path = do
        putStrLn "Meap analysis ..."
        r <- Chain.run meapOptions path
        case r of
            Left e -> fail e
            Right meap -> do
                let a = convMeap meap
                print a
                newAnalysis path a
