module Mescaline.Database.Unit where

import Mescaline.Database.Feature (Feature)
import Mescaline.Database.SourceFile (SourceFile)
import Mescaline.Database.Unique as Unique
import Prelude hiding (id)

-- create table unit (
--   id integer primary key,
--   sfid int,
--   onset_time float8,
--   chunk_length float8
-- );

data Unit = Unit {
    id :: Id,
    sourceFile :: SourceFile,
    onsetTime :: Double,
    chunkLength :: Double,
    features :: [Feature]
} deriving (Show)

instance Unique (Unit) where
    uid = id
