{-# LANGUAGE MultiParamTypeClasses #-}

module Mescaline.Data.ByteString (
    segment
  , fromLazy
  , toLazy
  , sha1Digest
  , sha1DigestFromFile
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.SHA1 as SHA1
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

sha1Digest :: BL.ByteString -> SHA1.Word160
sha1Digest = SHA1.hash . BL.unpack

sha1DigestFromFile :: FilePath -> IO SHA1.Word160
sha1DigestFromFile f = do
    h <- sha1Digest `fmap` BL.readFile f
    return $ h `seq` h
