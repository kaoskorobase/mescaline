module Mescaline.Database.SourceFile where

-- | Sourcefile.
data SourceFile = SourceFile {
    id :: Int,
    path :: FilePath,
    hash :: String
} deriving (Show)
