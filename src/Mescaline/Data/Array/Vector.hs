{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Mescaline.Data.Array.Vector (
    Vector,
    module Data.Array.Vector
) where

import Data.Array.Vector
import Data.Binary                              (Binary(..), Get, Put)
import qualified Data.Binary                    as Binary
import Data.Binary.Get                          (getWord32be, getWord64be)
import Data.Binary.Put                          (putWord32be, putWord64be)
import Data.ByteString.Lazy                     (ByteString)
import qualified Data.ByteString.Lazy           as ByteString
import qualified Mescaline.Data.ByteString      as ByteString
import Sound.OpenSoundControl.Cast              (f64_i64, i64_f64)

type Vector a = UArr a

instance Binary (Vector Double) where
    put x = do
        putWord32be (fromIntegral (lengthU x))
        mapM_ (putWord64be.fromIntegral.f64_i64) (fromU x)
    get = do
        n <- fromIntegral `fmap` getWord32be
        xs <- sequence (replicate n (getWord64be >>= return.i64_f64.fromIntegral))
        return (toU xs)
