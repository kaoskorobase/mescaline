module Mescaline.Database.SourceFile where

import Mescaline.Database.Unique as Unique
import Prelude hiding (id)

-- | Sourcefile.
data SourceFile = SourceFile {
    id  :: Id,
    path :: FilePath,
    hash :: String
} deriving (Show)

instance Unique (SourceFile) where
    uid = id
