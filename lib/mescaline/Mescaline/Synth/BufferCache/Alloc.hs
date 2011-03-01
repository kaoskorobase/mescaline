module Mescaline.Synth.BufferCache.Alloc (
    Alloc(..)
  , allocFrames
  , allocBytes
) where

data Alloc = Alloc {
    numChannels :: Int
  , numFrames   :: Int
} deriving (Eq, Show)

allocFrames :: Int -> Int -> Alloc
allocFrames = Alloc

allocBytes :: Int -> Int -> Alloc
allocBytes nc = Alloc nc . bytesToFrames nc

bytesToFrames :: Int -> Int -> Int
bytesToFrames nc b = b `div` 4 `div` nc
