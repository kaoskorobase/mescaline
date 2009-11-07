module Mescaline.Database.SourceFile where

import Mescaline.Data.Unique as Unique
import Mescaline.Database.SqlRow (SqlRow(..), fromSql)
import Prelude hiding (id)
import Sound.File.Sndfile (Count)

-- | Sourcefile.
data SourceFile = SourceFile {
    id          :: Id,
    url         :: FilePath,
    hash        :: String,

    numChannels :: Int,
    sampleRate  :: Double,
    frames      :: Count
} deriving (Show)

path :: SourceFile -> FilePath
path = url

instance Eq (SourceFile) where
    a == b = id a == id b

instance Ord (SourceFile) where
    a `compare` b = id a `compare` id b

instance Unique (SourceFile) where
    uuid = id

instance SqlRow (SourceFile) where
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
