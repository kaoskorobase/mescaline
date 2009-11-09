{-# LANGUAGE TemplateHaskell #-}

module Mescaline.Database.SourceFile where

import           Data.Accessor.Template (nameDeriveAccessors)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary
import           Data.Digest.SHA1 (Word160(..))
import           Data.Map as Map
import           Database.HDBC (SqlType(..))
import           Mescaline.Data.ByteString as BS
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
-- import           Mescaline.Database.SqlRow (SqlRow(..), fromSql)
import           Prelude hiding (id)
import           Sound.File.Sndfile (Count)

type URL = String
type Hash = Word160

-- | Sourcefile.
data SourceFile = SourceFile {
    id          :: Unique.Id,
    url         :: URL,
    hash        :: Hash,

    numChannels :: Int,
    sampleRate  :: Double,
    frames      :: Count
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

noHash :: Hash
noHash = Word160 0 0 0 0 0

unsafeCons :: Unique.Id -> URL -> Hash -> Int -> Double -> Count -> SourceFile
unsafeCons = SourceFile

cons :: URL -> Hash -> Int -> Double -> Count -> SourceFile
cons u h nc sr nf = unsafeCons (Unique.fromBinary namespace p) u h nc sr nf
    where p = Binary.put u >> Binary.put h

-- instance SqlRow (SourceFile) where
    -- toSqlRow x = map ($x) [ toSql.id,
    --                         toSql.path,
    --                         toSql.hash
    --                         -- toSql.frames,
    --                         -- toSql.sampleRate,
    --                         -- toSql.numChannels
    --                         ]

    -- fromSqlRow [ _id,
    --              _path,
    --              _hash ] = SourceFile
    --                         (fromSql _id)
    --                         (fromSql _path)
    --                         (fromSql _hash)
    -- fromSqlRow _ = error "SqlRow (SourceFile) conversion failure"
