{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Mescaline.Statistics.PCA (
    pca
) where

import Numeric.LinearAlgebra

-- | Vector with the mean value of the columns of a matrix.
mean :: forall a . (Field a) => Matrix a -> Vector a
mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

-- | Covariance matrix of a list of observations stored as rows.
cov :: forall a . (Field a, Num (Vector a)) => Matrix a -> Matrix a
cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
    where xc = x - asRow (mean x)

-- | Creates the compression and decompression functions from the desired number of components.
--
-- The matrix is expected to contain observations stored as rows.
pca :: forall a . (Num (Vector a), Field a) =>
       Int -> Matrix a -> (Vector a -> Vector a, Vector a -> Vector a)
pca n dataSet = (encode, decode)
    where
        encode x = vp <> (x - m)
        decode x = x <> vp + m
        m = mean dataSet
        c = cov dataSet
        (_, v) = eigSH' c
        vp = takeRows n (trans v)
