{-# LANGUAGE FlexibleContexts #-}
module Sound.Analysis.Segmentation.Statistics (
    lift
  , mean
  , variance
  , skewness
  , kurtosis
) where

import qualified Sound.Analysis.Vector as V
import qualified Data.Vector.Generic as V
import           Sound.Analysis.Segmentation
import qualified Statistics.Sample as S

lift :: V.Vector v Double => (v Double -> Double) -> Segmentation t (v Double) -> (v Double)
{-# INLINE lift #-}
lift f = V.fromList . map f . V.transpose . labels

mean :: V.Vector v Double => Segmentation t (v Double) -> v Double
{-# INLINE mean #-}
mean = lift S.mean

variance :: V.Vector v Double => Segmentation t (v Double) -> v Double
{-# INLINE variance #-}
variance = lift S.variance

skewness :: V.Vector v Double => Segmentation t (v Double) -> v Double
{-# INLINE skewness #-}
skewness = lift S.skewness

kurtosis :: V.Vector v Double => Segmentation t (v Double) -> v Double
{-# INLINE kurtosis #-}
kurtosis = lift S.kurtosis

-- DCT
-- envelopes
