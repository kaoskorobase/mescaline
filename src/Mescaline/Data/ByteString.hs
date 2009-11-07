{-# LANGUAGE MultiParamTypeClasses #-}

module Mescaline.Data.ByteString (
    segment
  , fromLazy
  , toLazy
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Int (Int64)

segment :: Int64 -> BL.ByteString -> [BL.ByteString]
segment n b =
    if BL.null b
        then []
        else BL.take n b : segment n (BL.drop n b)

fromLazy :: BL.ByteString -> BS.ByteString
fromLazy = BS.concat . BL.toChunks

toLazy :: BS.ByteString -> BL.ByteString
toLazy s = BL.fromChunks [s]
