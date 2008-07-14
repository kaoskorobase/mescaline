module Mescaline.Database.FeatureData (
    FeatureData(..)
) where

import Mescaline.Database.Feature (Vector)
import Mescaline.Database.Unique (Id)

data FeatureData = FeatureData {
    sourceFileId  :: Id,
    unitId        :: Id,
    onset_time    :: Double,
    chunk_length  :: Double,
    featureId     :: Id, 
    intval        :: Maybe Int, 
    realval       :: Maybe Double, 
    textval       :: Maybe String, 
    arrayval      :: Maybe (Vector Double)
} deriving (Show)
