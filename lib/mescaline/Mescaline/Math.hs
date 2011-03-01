module Mescaline.Math (
    clip
  , wrap
  , fold
) where

clip :: (Ord a) => a -> a -> a -> a
{-# INLINE clip #-}
clip l h x | x < l     = l
           | h < x     = h
           | otherwise = x

wrap :: RealFrac a => a -> a -> a -> a
{-# INLINE wrap #-}
wrap l h x | l <= x && x <= h = x
           | otherwise        = let r = h - l in x - r * fromIntegral (floor ((x - l) / r) :: Int)

fold :: RealFrac a => a -> a -> a -> a
{-# INLINE fold #-}
fold l h x
    | l == h = l
    | otherwise = let x' = x - l
                      r  = h - l
                      r2 = r + r
                      c  = x' - r2 * fromIntegral (floor (x'/r2) :: Int)
                  in if c >= r
                     then l + r2 - c
                     else l + c
