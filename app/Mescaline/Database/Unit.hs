module Mescaline.Database.Unit where

import Mescaline.Database.SourceFile (SourceFile)
import Mescaline.Database.Feature (Feature)

-- create table unit (
--   id integer primary key,
--   sfid int,
--   onset_time float8,
--   chunk_length float8
-- );

data Unit = Unit {
    id :: Int,
    sourceFile :: SourceFile,
    onsetTime :: Double,
    chunkLength :: Double,
    features :: [Feature]
}
