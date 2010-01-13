{-# LANGUAGE TemplateHaskell #-}

module Mescaline.Database.SourceFile where

import           Data.Accessor.Template (nameDeriveAccessors)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.Digest.SHA1 (Word160(..))
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Word (Word32)
import           Database.HDBC (SqlType(..))
import           Mescaline.Data.ByteString as BS
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.SoundFile as SF
import           Prelude hiding (id)
-- import           Sound.File.Sndfile (Count)
import           Text.Printf (printf)

type URL = String
newtype Hash = Hash String deriving (Binary, Eq, Ord, Read, Show)

-- | Sourcefile.
data SourceFile = SourceFile {
    id          :: Unique.Id,
    url         :: URL,
    hash        :: Hash,

    numChannels :: Int,
    sampleRate  :: Double,
    frames      :: SF.Count
} deriving (Show)

$(nameDeriveAccessors ''SourceFile (return.(++"_")))

instance Binary Word160 where
    put (Word160 w0 w1 w2 w3 w4) = do
        Binary.put w0
        Binary.put w1
        Binary.put w2
        Binary.put w3
        Binary.put w4
    get = do
        w0 <- Binary.get
        w1 <- Binary.get
        w2 <- Binary.get
        w3 <- Binary.get
        w4 <- Binary.get
        return $ Word160 w0 w1 w2 w3 w4

instance SqlType Word160 where
    fromSql = Binary.decode . BS.toLazy . fromSql
    toSql = toSql . BS.fromLazy . Binary.encode

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "d03a5083-8958-484e-a441-b3bf75d68818"

path :: SourceFile -> URL
path = url

instance Eq (SourceFile) where
    a == b = id a == id b

instance Ord (SourceFile) where
    a `compare` b = id a `compare` id b

instance Unique (SourceFile) where
    uuid = id

-- | Contruct a 'Hash' from five 32 bit integers.
fromList :: [Word32] -> Hash
fromList = Hash . intercalate "-" . map (printf "%08x")

-- | Construct a 'Hash' from a 'Word160'.
fromWord160 :: Word160 -> Hash
fromWord160 (Word160 x1 x2 x3 x4 x5) = fromList [x1, x2, x3, x4, x5]

-- | Placeholder for a zero hash.
noHash :: Hash
noHash = fromList [0, 0, 0, 0, 0]

instance SqlType Hash where
    fromSql = Hash . fromSql
    toSql (Hash s) = toSql s

-- | Construct a SourceFile.
-- The id is expected to be valid.
unsafeCons :: Unique.Id -> URL -> Hash -> Int -> Double -> SF.Count -> SourceFile
unsafeCons = SourceFile

-- | Construct a SourceFile.
-- The id is generated from the SourceFile contents
cons :: URL -> Hash -> Int -> Double -> SF.Count -> SourceFile
cons u h nc sr nf = unsafeCons (Unique.fromBinary namespace p) u h nc sr nf
    where p = Binary.put u >> Binary.put h

-- | Create a new SourceFile from a locally available sound file.
newLocal :: FilePath -> IO SourceFile
newLocal path = do
    info <- SF.getInfo path
    hash <- fromWord160 `fmap` BS.sha1DigestFromFile path
    return $ cons path hash
                (SF.channels info)
                (fromIntegral $ SF.samplerate info)
                (SF.frames info)
