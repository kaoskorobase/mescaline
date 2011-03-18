-- {-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification
           , GeneralizedNewtypeDeriving
           , TypeFamilies #-}

module Mescaline.Database.Entity where

import           Control.DeepSeq (NFData(..))
import           Control.Monad.IO.Peel (MonadPeelIO)
import           Data.Either
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Database.Persist as DB
import           Database.Persist.Base
import           Database.Persist.GenericSql
import           Mescaline.Database.Vector (GVector)
import qualified Mescaline.Database.Vector as Vector
import qualified Mescaline.Database.Hash as Hash

-- DB.share2 DB.mkPersist (DB.mkMigrate "migrateAll") [persist|
-- SourceFile
--     url         FilePath Eq
--     hash        Hash.Hash Eq
--     numChannels Int
--     sampleRate  Double
--     frames      Int64
--     UniqueSourceFile hash
-- Unit
--     sourceFile  SourceFileId Eq
--     onset       Double
--     duration    Double
-- Descriptor
--     name        String Eq
--     degree      Int
--     UniqueDescriptor name
-- Feature
--     unit        UnitId Eq Asc
--     descriptor  DescriptorId Eq
--     value       Vector
--     UniqueFeature unit descriptor
-- |]

type SourceFileMap = Map (DB.Key SourceFile) SourceFile

-- q = DB.share2 DB.mkPersist (DB.mkMigrate "migrateAll") [persist|
-- SourceFile
--     url         FilePath Eq
--     hash        Hash.Hash Eq
--     numChannels Int
--     sampleRate  Double
--     frames      Int64
--     UniqueSourceFile hash
-- Unit
--     sourceFile  SourceFileId Eq
--     onset       Double
--     duration    Double
-- Descriptor
--     name        String Eq
--     degree      Int
--     UniqueDescriptor name
-- Feature
--     unit        UnitId Eq Asc
--     descriptor  DescriptorId Eq
--     value       Vector
--     UniqueFeature unit descriptor
-- |]

apE :: Either x (y -> z) -> Either x y -> Either x z
apE (Left x) _ = Left x
apE _ (Left x) = Left x
apE (Right f) (Right y) = Right $ f y

degenerate :: a
degenerate = error "Degenerate case, should never happen"

data SourceFile = SourceFile {
    sourceFileUrl :: FilePath,
    sourceFileHash :: Hash.Hash,
    sourceFileNumChannels :: Int,
    sourceFileSampleRate :: Double,
    sourceFileFrames :: Int64 }
    deriving (Show, Read, Eq)

type SourceFileId = Database.Persist.Base.Key SourceFile

instance NFData SourceFile where
    rnf (SourceFile x1 x2 x3 x4 x5) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` ()

instance Database.Persist.Base.PersistEntity SourceFile where
    newtype Database.Persist.Base.Key SourceFile
            = SourceFileId Data.Int.Int64
              deriving (Show, Read, Num, Integral, Enum, Eq, Ord, Real, NFData, Database.Persist.Base.PersistField {- , Web.Routes.Quasi.Classes.SinglePiece -})
    data Database.Persist.Base.Filter SourceFile
            = SourceFileUrlEq FilePath | SourceFileHashEq Hash.Hash
              deriving (Show, Read, Eq)
    data Database.Persist.Base.Update SourceFile
    data Database.Persist.Base.Order SourceFile
    data Database.Persist.Base.Unique SourceFile
            = UniqueSourceFile Hash.Hash
              deriving (Show, Read, Eq)
    entityDef _ =
        Database.Persist.Base.EntityDef "SourceFile"  []
            [ ("url", "FilePath", ["Eq"])
            , ("hash", "Hash.Hash", ["Eq"])
            , ("numChannels", "Int", [])
            , ("sampleRate", "Double", [])
            , ("frames", "Int64", []) ]
            [ ("UniqueSourceFile", ["hash"]) ]
            [ "Show",  "Read", "Eq" ]
    toPersistFields (SourceFile x_0
                                x_1
                                x_2
                                x_3
                                x_4) = [Database.Persist.Base.SomePersistField x_0,
                                        Database.Persist.Base.SomePersistField x_1,
                                        Database.Persist.Base.SomePersistField x_2,
                                        Database.Persist.Base.SomePersistField x_3,
                                        Database.Persist.Base.SomePersistField x_4]
    fromPersistValues [x_5,
                       x_6,
                       x_7,
                       x_8,
                       x_9] = ((((Right SourceFile `apE` Database.Persist.Base.fromPersistValue x_5) `apE` Database.Persist.Base.fromPersistValue x_6) `apE` Database.Persist.Base.fromPersistValue x_7) `apE` Database.Persist.Base.fromPersistValue x_8) `apE` Database.Persist.Base.fromPersistValue x_9
    fromPersistValues _ = Left "Invalid fromPersistValues input"
    halfDefined = SourceFile undefined undefined undefined undefined undefined
    toPersistKey = fromIntegral
    fromPersistKey = fromIntegral
    showPersistKey = show
    persistOrderToFieldName _ = degenerate
    persistOrderToOrder _ = degenerate
    persistUpdateToFieldName _ = degenerate
    persistUpdateToValue _ = degenerate
    persistUpdateToUpdate _ = degenerate
    persistFilterToFieldName (SourceFileUrlEq {}) = "url"
    persistFilterToFieldName (SourceFileHashEq {}) = "hash"
    persistFilterToValue (SourceFileUrlEq x_10) = (Left . Database.Persist.Base.toPersistValue) x_10
    persistFilterToValue (SourceFileHashEq x_11) = (Left . Database.Persist.Base.toPersistValue) x_11
    persistUniqueToFieldNames (UniqueSourceFile {}) = ["hash"]
    persistUniqueToValues (UniqueSourceFile x_12) = [Database.Persist.Base.toPersistValue x_12]
    persistUniqueKeys (SourceFile _url_13
                                _hash_14
                                _numChannels_15
                                _sampleRate_16
                                _frames_17) = [UniqueSourceFile _hash_14]
    persistFilterToFilter (SourceFileUrlEq {}) = Database.Persist.Base.Eq
    persistFilterToFilter (SourceFileHashEq {}) = Database.Persist.Base.Eq

data Unit = Unit {
    unitSourceFile :: SourceFileId,
    unitOnset :: Double,
    unitDuration :: Double }
    deriving (Show, Read, Eq)

type UnitId = Database.Persist.Base.Key Unit

instance NFData Unit where
    rnf (Unit x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()

instance Database.Persist.Base.PersistEntity Unit where
    newtype Database.Persist.Base.Key Unit
            = UnitId Data.Int.Int64
              deriving (Show, Read, Num, Integral, Enum, Eq, Ord, Real, NFData, Database.Persist.Base.PersistField {- , Web.Routes.Quasi.Classes.SinglePiece -})
    data Database.Persist.Base.Filter Unit
            = UnitSourceFileEq SourceFileId
              deriving (Show, Read, Eq)
    data Database.Persist.Base.Update Unit
    data Database.Persist.Base.Order Unit
    data Database.Persist.Base.Unique Unit
    
    entityDef _ =
        Database.Persist.Base.EntityDef "Unit" []
            [ ("sourceFile", "SourceFileId", ["Eq"])
            , ("onset", "Double", [])
            , ("duration", "Double", []) ]
            []
            ["Show", "Read", "Eq"]
    toPersistFields (Unit x_18
                          x_19
                           x_20) = [Database.Persist.Base.SomePersistField x_18,
                                    Database.Persist.Base.SomePersistField x_19,
                                    Database.Persist.Base.SomePersistField x_20]
    fromPersistValues [x_21,
                       x_22,
                       x_23] = ((Right Unit `apE` Database.Persist.Base.fromPersistValue x_21) `apE` Database.Persist.Base.fromPersistValue x_22) `apE` Database.Persist.Base.fromPersistValue x_23
    fromPersistValues _ = Left "Invalid fromPersistValues input"
    halfDefined = Unit undefined undefined undefined
    toPersistKey = fromIntegral
    fromPersistKey = fromIntegral
    showPersistKey = show
    persistOrderToFieldName _ = degenerate
    persistOrderToOrder _ = degenerate
    persistUpdateToFieldName _ = degenerate
    persistUpdateToValue _ = degenerate
    persistUpdateToUpdate _ = degenerate
    persistFilterToFieldName (UnitSourceFileEq {}) = "sourceFile"
    persistFilterToValue (UnitSourceFileEq x_24) = (Left . Database.Persist.Base.toPersistValue) x_24
    persistUniqueToFieldNames _ = degenerate
    persistUniqueToValues _ = degenerate
    persistUniqueKeys (Unit _sourceFile_25 _onset_26 _duration_27) = []
    persistFilterToFilter (UnitSourceFileEq {}) = Database.Persist.Base.Eq

data Descriptor = Descriptor {
    descriptorName :: String, descriptorDegree :: Int }
    deriving (Show, Read, Eq)

type DescriptorId = Database.Persist.Base.Key Descriptor

instance NFData Descriptor where
    rnf (Descriptor x1 x2) = rnf x1 `seq` rnf x2 `seq` ()

instance Database.Persist.Base.PersistEntity Descriptor where
    newtype Database.Persist.Base.Key Descriptor
            = DescriptorId Data.Int.Int64
              deriving (Show, Read, Num, Integral, Enum, Eq, Ord, Real, NFData, Database.Persist.Base.PersistField {- , Web.Routes.Quasi.Classes.SinglePiece -})
    data Database.Persist.Base.Filter Descriptor
            = DescriptorNameEq String
              deriving (Show, Read, Eq)
    data Database.Persist.Base.Update Descriptor
    data Database.Persist.Base.Order Descriptor
    data Database.Persist.Base.Unique Descriptor
            = UniqueDescriptor String
              deriving (Show, Read, Eq)
    
    entityDef _ =
        Database.Persist.Base.EntityDef "Descriptor" []
            [ ("name", "String", ["Eq"])
            , ("degree", "Int", []) ]
            [ ("UniqueDescriptor", ["name"]) ]
            [ "Show", "Read", "Eq" ]
    toPersistFields (Descriptor x_28
                                x_29) = [Database.Persist.Base.SomePersistField x_28,
                                         Database.Persist.Base.SomePersistField x_29]
    fromPersistValues [x_30,
                       x_31] = (Right Descriptor `apE` Database.Persist.Base.fromPersistValue x_30) `apE` Database.Persist.Base.fromPersistValue x_31
    fromPersistValues _ = Left "Invalid fromPersistValues input"
    halfDefined = Descriptor undefined undefined
    toPersistKey = fromIntegral
    fromPersistKey = fromIntegral
    showPersistKey = show
    persistOrderToFieldName _ = degenerate
    persistOrderToOrder _ = degenerate
    persistUpdateToFieldName _ = degenerate
    persistUpdateToValue _ = degenerate
    persistUpdateToUpdate _ = degenerate
    persistFilterToFieldName (DescriptorNameEq {}) = "name"
    persistFilterToValue (DescriptorNameEq x_32) = (Left . Database.Persist.Base.toPersistValue) x_32
    persistUniqueToFieldNames (UniqueDescriptor {}) = ["name"]
    persistUniqueToValues (UniqueDescriptor x_33) = [Database.Persist.Base.toPersistValue x_33]
    persistUniqueKeys (Descriptor _name_34
                                _degree_35) = [UniqueDescriptor _name_34]
    persistFilterToFilter (DescriptorNameEq {}) = Database.Persist.Base.Eq

data Feature = Feature {
    featureUnit :: UnitId,
    featureDescriptor :: DescriptorId,
    featureValue :: Vector.Vector }
    deriving (Show, Read, Eq)

featureVectorValue :: Feature -> GVector
featureVectorValue = Vector.toVector . featureValue

type FeatureId = Database.Persist.Base.Key Feature

instance NFData Feature where
    rnf (Feature x1 x2 x3) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` ()

instance Database.Persist.Base.PersistEntity Feature where
    newtype Database.Persist.Base.Key Feature
            = FeatureId Data.Int.Int64
              deriving (Show, Read, Num, Integral, Enum, Eq, Ord, Real, NFData, Database.Persist.Base.PersistField {- , Web.Routes.Quasi.Classes.SinglePiece -})
    data Database.Persist.Base.Filter Feature
            = FeatureUnitEq UnitId | FeatureDescriptorEq DescriptorId
              deriving (Show, Read, Eq)
    data Database.Persist.Base.Update Feature
    data Database.Persist.Base.Order Feature
            = FeatureUnitAsc
              deriving (Show, Read, Eq)
    data Database.Persist.Base.Unique Feature
            = UniqueFeature UnitId DescriptorId
              deriving (Show, Read, Eq)
    
    entityDef _ =
        Database.Persist.Base.EntityDef "Feature" []
            [ ("unit", "UnitId", ["Eq", "Asc"])
            , ("descriptor", "DescriptorId", ["Eq"])
            , ("value", "Vector.Vector", []) ]
            [ ("UniqueFeature", ["unit", "descriptor"]) ]
            [ "Show", "Read", "Eq" ]
    toPersistFields (Feature x_36
                             x_37
                             x_38) = [Database.Persist.Base.SomePersistField x_36,
                                      Database.Persist.Base.SomePersistField x_37,
                                      Database.Persist.Base.SomePersistField x_38]
    fromPersistValues [x_39,
                       x_40,
                       x_41] = ((Right Feature `apE` Database.Persist.Base.fromPersistValue x_39) `apE` Database.Persist.Base.fromPersistValue x_40) `apE` Database.Persist.Base.fromPersistValue x_41
    fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
    halfDefined = Feature undefined undefined undefined
    toPersistKey = fromIntegral
    fromPersistKey = fromIntegral
    showPersistKey = show
    persistOrderToFieldName (FeatureUnitAsc {}) = "unit"
    persistOrderToOrder (FeatureUnitAsc {}) = Database.Persist.Base.Asc
    persistUpdateToFieldName _ = degenerate
    persistUpdateToValue _ = degenerate
    persistUpdateToUpdate _ = degenerate
    persistFilterToFieldName (FeatureUnitEq {}) = "unit"
    persistFilterToFieldName (FeatureDescriptorEq {}) = "descriptor"
    persistFilterToValue (FeatureUnitEq x_42) = (Left . Database.Persist.Base.toPersistValue) x_42
    persistFilterToValue (FeatureDescriptorEq x_43) = (Left . Database.Persist.Base.toPersistValue) x_43
    persistUniqueToFieldNames (UniqueFeature {}) = ["unit",
                                                  "descriptor"]
    persistUniqueToValues (UniqueFeature x_44
                                       x_45) = [Database.Persist.Base.toPersistValue x_44,
                                                Database.Persist.Base.toPersistValue x_45]
    persistUniqueKeys (Feature _unit_46
                             _descriptor_47
                             _value_48) = [UniqueFeature _unit_46 _descriptor_47]
    persistFilterToFilter (FeatureUnitEq {}) = Database.Persist.Base.Eq
    persistFilterToFilter (FeatureDescriptorEq {}) = Database.Persist.Base.Eq

-- migrateAll :: forall m . Control.Monad.Invert.MonadInvertIO m =>
--                          Database.Persist.GenericSql.Migration (Database.Persist.GenericSql.SqlPersist m)
migrateAll :: MonadPeelIO m => Database.Persist.GenericSql.Migration (Database.Persist.GenericSql.SqlPersist m)
migrateAll = do Database.Persist.GenericSql.migrate (undefined :: SourceFile)
                Database.Persist.GenericSql.migrate (undefined :: Unit)
                Database.Persist.GenericSql.migrate (undefined :: Descriptor)
                Database.Persist.GenericSql.migrate (undefined :: Feature)
