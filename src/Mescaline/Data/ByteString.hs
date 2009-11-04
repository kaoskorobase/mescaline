{-# LANGUAGE MultiParamTypeClasses #-}

module Mescaline.Data.ByteString (
    segment
) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Int (Int64)

segment :: Int64 -> ByteString -> [ByteString]
segment n b =
    if BS.null b
        then []
        else BS.take n b : segment n (BS.drop n b)
