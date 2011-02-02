{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mescaline.Database.Hash (
    Hash
  , fromString
  , toString
  , fromData
  , fromFile
  , noHash
) where

import           Data.Binary (Binary(..))
import           Control.Monad
import           Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Database.Persist (PersistField(..))
import           Database.Persist.Base (PersistValue(..), SqlType(..))
import           Codec.Digest.SHA

newtype Hash = Hash B.ByteString deriving (Eq, Ord, Read, Show)

-- instance Show Hash where
--     show h = "fromString " ++ show (toString h)

instance Binary Hash where
    get = liftM Hash get
    put (Hash b) = put b

instance PersistField Hash where
    toPersistValue (Hash h) =
        toPersistValue h
    fromPersistValue v =
        case v of
            PersistByteString b -> Right (Hash b)
            p -> Left ("Couldn't create Hash from PersistValue " ++ show p)
    sqlType _ = SqlBlob

-- | Construct a 'Hash' from a hex encoded string.
fromString :: String -> Hash
fromString = Hash . B.pack . map (fromIntegral.digitToInt)

-- | Convert a Hash to a hex encoded string.
toString :: Hash -> String
toString (Hash b) = showBSasHex b

-- | Construct a Hash from data.
fromData :: B.ByteString -> Hash
fromData = Hash . hash SHA384 -- FIXME: SHA512 produces different hashes for the same content.

-- | Construct a Hash from a file.
fromFile :: FilePath -> IO Hash
fromFile = liftM fromData . B.readFile

-- | Placeholder for a zero hash.
noHash :: Hash
noHash = Hash (BC.pack "")
