module Mescaline.Database.SourceFile where

data SourceFile = SourceFile {
    id :: Int,
    path :: FilePath,
    hash :: String
} deriving (Show)
