{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell #-}
module Mescaline.Database.Entity where

import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Database.Persist as DB
import qualified Database.Persist.GenericSql as DB
import           Database.Persist.Quasi
import qualified Database.Persist.TH as DB
import qualified Mescaline.Database.Hash as Hash
import           Mescaline.Database.Vector (Vector)

DB.share2 DB.mkPersist (DB.mkMigrate "migrateAll") [persist|
SourceFile
    url         FilePath Eq
    hash        Hash.Hash Eq
    numChannels Int
    sampleRate  Double
    frames      Int64
    UniqueSourceFile hash
Unit
    sourceFile  SourceFileId Eq
    onset       Double
    duration    Double
Descriptor
    name        String Eq
    degree      Int
    UniqueDescriptor name
Feature
    unit        UnitId Eq Asc
    descriptor  DescriptorId Eq
    value       Vector
    UniqueFeature unit descriptor
|]

type SourceFileMap = Map (DB.Key SourceFile) SourceFile
