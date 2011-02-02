{-# LANGUAGE BangPatterns #-}
module Sound.Analysis.Vector (
    transpose
) where

import           Control.Monad
import qualified Control.Monad.ST as ST
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

transpose :: V.Vector v a => [v a] -> [v a]
transpose [] = []
transpose xs = ST.runST $ do
    mvs <- replicateM m (MV.new n)
    loop xs 0 mvs
    mapM V.unsafeFreeze mvs
    where
        n = length xs
        m = V.length (head xs)
        loop [] _ _ = return ()
        loop (v:vs) !i mvs = do
            if V.length v /= m
                then fail $ "Sound.Analysis.Vector.transpose: dimension mismatch: expected " ++ show m ++ ", got " ++ show (V.length v)
                else do
                    zipWithM_ (\j mv -> do { MV.unsafeWrite mv i (v V.! j) }) [0..] mvs
                    loop vs (i+1) mvs
