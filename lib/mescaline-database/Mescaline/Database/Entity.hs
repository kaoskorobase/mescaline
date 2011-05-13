{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Mescaline.Database.Entity where
import qualified Control.Applicative
import qualified Control.Monad.IO.Control
import qualified Data.Either
import qualified Data.Int
import qualified Database.Persist.Base
import qualified Database.Persist.GenericSql
import qualified Database.Persist.GenericSql.Raw
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Err
import qualified GHC.Read
import qualified GHC.Show
import qualified Mescaline.Database.Hash
import qualified Mescaline.Database.Vector
-- Prelude >>>
import Data.Map (Map)
type SourceFileMap = Map (Database.Persist.Base.Key SourceFile) SourceFile
-- <<< Prelude
data SourceFile
    = SourceFile {sourceFileUrl :: FilePath,
                  sourceFileHash :: Mescaline.Database.Hash.Hash,
                  sourceFileNumChannels :: Int,
                  sourceFileSampleRate :: Double,
                  sourceFileFrames :: Data.Int.Int64}
    deriving (Show, Read, Eq)
type SourceFileId = Database.Persist.Base.Key SourceFile
instance Database.Persist.Base.PersistEntity SourceFile
    where newtype Database.Persist.Base.Key SourceFile
            = SourceFileId {unSourceFileId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter SourceFile
              = SourceFileIdIn ([Database.Persist.Base.Key SourceFile])
              | SourceFileUrlEq FilePath
              | SourceFileHashEq Mescaline.Database.Hash.Hash
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update SourceFile
          data Database.Persist.Base.Order SourceFile
          data Database.Persist.Base.Unique SourceFile
              = UniqueSourceFile Mescaline.Database.Hash.Hash
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          entityDef _ = Database.Persist.Base.EntityDef "SourceFile" [] [(['u',
                                                                           'r',
                                                                           'l'],
                                                                          ['F',
                                                                           'i',
                                                                           'l',
                                                                           'e',
                                                                           'P',
                                                                           'a',
                                                                           't',
                                                                           'h'],
                                                                          [['E', 'q']]),
                                                                         (['h', 'a', 's', 'h'],
                                                                          ['M',
                                                                           'e',
                                                                           's',
                                                                           'c',
                                                                           'a',
                                                                           'l',
                                                                           'i',
                                                                           'n',
                                                                           'e',
                                                                           '.',
                                                                           'D',
                                                                           'a',
                                                                           't',
                                                                           'a',
                                                                           'b',
                                                                           'a',
                                                                           's',
                                                                           'e',
                                                                           '.',
                                                                           'H',
                                                                           'a',
                                                                           's',
                                                                           'h',
                                                                           '.',
                                                                           'H',
                                                                           'a',
                                                                           's',
                                                                           'h'],
                                                                          [['E', 'q']]),
                                                                         (['n',
                                                                           'u',
                                                                           'm',
                                                                           'C',
                                                                           'h',
                                                                           'a',
                                                                           'n',
                                                                           'n',
                                                                           'e',
                                                                           'l',
                                                                           's'],
                                                                          ['I', 'n', 't'],
                                                                          []),
                                                                         (['s',
                                                                           'a',
                                                                           'm',
                                                                           'p',
                                                                           'l',
                                                                           'e',
                                                                           'R',
                                                                           'a',
                                                                           't',
                                                                           'e'],
                                                                          ['D',
                                                                           'o',
                                                                           'u',
                                                                           'b',
                                                                           'l',
                                                                           'e'],
                                                                          []),
                                                                         (['f',
                                                                           'r',
                                                                           'a',
                                                                           'm',
                                                                           'e',
                                                                           's'],
                                                                          ['D',
                                                                           'a',
                                                                           't',
                                                                           'a',
                                                                           '.',
                                                                           'I',
                                                                           'n',
                                                                           't',
                                                                           '.',
                                                                           'I',
                                                                           'n',
                                                                           't',
                                                                           '6',
                                                                           '4'],
                                                                          [])] [(['U',
                                                                                  'n',
                                                                                  'i',
                                                                                  'q',
                                                                                  'u',
                                                                                  'e',
                                                                                  'S',
                                                                                  'o',
                                                                                  'u',
                                                                                  'r',
                                                                                  'c',
                                                                                  'e',
                                                                                  'F',
                                                                                  'i',
                                                                                  'l',
                                                                                  'e'],
                                                                                 [['h',
                                                                                   'a',
                                                                                   's',
                                                                                   'h']])] [['S',
                                                                                             'h',
                                                                                             'o',
                                                                                             'w'],
                                                                                            ['R',
                                                                                             'e',
                                                                                             'a',
                                                                                             'd'],
                                                                                            ['E',
                                                                                             'q']]
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
                             x_9] = ((((Data.Either.Right SourceFile Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_5) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_6) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_7) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_8) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_9
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = SourceFile undefined undefined undefined undefined undefined
          toPersistKey = SourceFileId
          fromPersistKey = unSourceFileId
          persistOrderToFieldName _ = error "Degenerate case, should never happen"
          persistOrderToOrder _ = error "Degenerate case, should never happen"
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (SourceFileIdIn {}) = "id"
          persistFilterToFieldName (SourceFileUrlEq {}) = "url"
          persistFilterToFieldName (SourceFileHashEq {}) = "hash"
          persistFilterToValue (SourceFileIdIn x_10) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_10
          persistFilterToValue (SourceFileUrlEq x_11) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_11
          persistFilterToValue (SourceFileHashEq x_12) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_12
          persistUniqueToFieldNames (UniqueSourceFile {}) = ["hash"]
          persistUniqueToValues (UniqueSourceFile x_13) = [Database.Persist.Base.toPersistValue x_13]
          persistUniqueKeys (SourceFile _url_14
                                        _hash_15
                                        _numChannels_16
                                        _sampleRate_17
                                        _frames_18) = [UniqueSourceFile _hash_15]
          persistFilterToFilter (SourceFileIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (SourceFileUrlEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileHashEq {}) = Database.Persist.Base.Eq
data Unit
    = Unit {unitSourceFile :: SourceFileId,
            unitOnset :: Double,
            unitDuration :: Double}
    deriving (Show, Read, Eq)
type UnitId = Database.Persist.Base.Key Unit
instance Database.Persist.Base.PersistEntity Unit
    where newtype Database.Persist.Base.Key Unit
            = UnitId {unUnitId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter Unit
              = UnitIdIn ([Database.Persist.Base.Key Unit])
              | UnitSourceFileEq SourceFileId
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Unit
          data Database.Persist.Base.Order Unit
          data Database.Persist.Base.Unique Unit
          entityDef _ = Database.Persist.Base.EntityDef "Unit" [] [(['s',
                                                                     'o',
                                                                     'u',
                                                                     'r',
                                                                     'c',
                                                                     'e',
                                                                     'F',
                                                                     'i',
                                                                     'l',
                                                                     'e'],
                                                                    ['S',
                                                                     'o',
                                                                     'u',
                                                                     'r',
                                                                     'c',
                                                                     'e',
                                                                     'F',
                                                                     'i',
                                                                     'l',
                                                                     'e',
                                                                     'I',
                                                                     'd'],
                                                                    [['E', 'q']]),
                                                                   (['o', 'n', 's', 'e', 't'],
                                                                    ['D', 'o', 'u', 'b', 'l', 'e'],
                                                                    []),
                                                                   (['d',
                                                                     'u',
                                                                     'r',
                                                                     'a',
                                                                     't',
                                                                     'i',
                                                                     'o',
                                                                     'n'],
                                                                    ['D', 'o', 'u', 'b', 'l', 'e'],
                                                                    [])] [] [['S', 'h', 'o', 'w'],
                                                                             ['R', 'e', 'a', 'd'],
                                                                             ['E', 'q']]
          toPersistFields (Unit x_19
                                x_20
                                x_21) = [Database.Persist.Base.SomePersistField x_19,
                                         Database.Persist.Base.SomePersistField x_20,
                                         Database.Persist.Base.SomePersistField x_21]
          fromPersistValues [x_22,
                             x_23,
                             x_24] = ((Data.Either.Right Unit Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_22) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_23) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_24
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Unit undefined undefined undefined
          toPersistKey = UnitId
          fromPersistKey = unUnitId
          persistOrderToFieldName _ = error "Degenerate case, should never happen"
          persistOrderToOrder _ = error "Degenerate case, should never happen"
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (UnitIdIn {}) = "id"
          persistFilterToFieldName (UnitSourceFileEq {}) = "sourceFile"
          persistFilterToValue (UnitIdIn x_25) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_25
          persistFilterToValue (UnitSourceFileEq x_26) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_26
          persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
          persistUniqueToValues _ = error "Degenerate case, should never happen"
          persistUniqueKeys (Unit _sourceFile_27 _onset_28 _duration_29) = []
          persistFilterToFilter (UnitIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (UnitSourceFileEq {}) = Database.Persist.Base.Eq
data Descriptor
    = Descriptor {descriptorName :: String, descriptorDegree :: Int}
    deriving (Show, Read, Eq)
type DescriptorId = Database.Persist.Base.Key Descriptor
instance Database.Persist.Base.PersistEntity Descriptor
    where newtype Database.Persist.Base.Key Descriptor
            = DescriptorId {unDescriptorId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter Descriptor
              = DescriptorIdIn ([Database.Persist.Base.Key Descriptor])
              | DescriptorNameEq String
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Descriptor
          data Database.Persist.Base.Order Descriptor
          data Database.Persist.Base.Unique Descriptor
              = UniqueDescriptor String
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          entityDef _ = Database.Persist.Base.EntityDef "Descriptor" [] [(['n',
                                                                           'a',
                                                                           'm',
                                                                           'e'],
                                                                          ['S',
                                                                           't',
                                                                           'r',
                                                                           'i',
                                                                           'n',
                                                                           'g'],
                                                                          [['E', 'q']]),
                                                                         (['d',
                                                                           'e',
                                                                           'g',
                                                                           'r',
                                                                           'e',
                                                                           'e'],
                                                                          ['I', 'n', 't'],
                                                                          [])] [(['U',
                                                                                  'n',
                                                                                  'i',
                                                                                  'q',
                                                                                  'u',
                                                                                  'e',
                                                                                  'D',
                                                                                  'e',
                                                                                  's',
                                                                                  'c',
                                                                                  'r',
                                                                                  'i',
                                                                                  'p',
                                                                                  't',
                                                                                  'o',
                                                                                  'r'],
                                                                                 [['n',
                                                                                   'a',
                                                                                   'm',
                                                                                   'e']])] [['S',
                                                                                             'h',
                                                                                             'o',
                                                                                             'w'],
                                                                                            ['R',
                                                                                             'e',
                                                                                             'a',
                                                                                             'd'],
                                                                                            ['E',
                                                                                             'q']]
          toPersistFields (Descriptor x_30
                                      x_31) = [Database.Persist.Base.SomePersistField x_30,
                                               Database.Persist.Base.SomePersistField x_31]
          fromPersistValues [x_32,
                             x_33] = (Data.Either.Right Descriptor Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_32) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_33
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Descriptor undefined undefined
          toPersistKey = DescriptorId
          fromPersistKey = unDescriptorId
          persistOrderToFieldName _ = error "Degenerate case, should never happen"
          persistOrderToOrder _ = error "Degenerate case, should never happen"
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (DescriptorIdIn {}) = "id"
          persistFilterToFieldName (DescriptorNameEq {}) = "name"
          persistFilterToValue (DescriptorIdIn x_34) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_34
          persistFilterToValue (DescriptorNameEq x_35) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_35
          persistUniqueToFieldNames (UniqueDescriptor {}) = ["name"]
          persistUniqueToValues (UniqueDescriptor x_36) = [Database.Persist.Base.toPersistValue x_36]
          persistUniqueKeys (Descriptor _name_37
                                        _degree_38) = [UniqueDescriptor _name_37]
          persistFilterToFilter (DescriptorIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (DescriptorNameEq {}) = Database.Persist.Base.Eq
data Feature
    = Feature {featureUnit :: UnitId,
               featureDescriptor :: DescriptorId,
               featureValue :: Mescaline.Database.Vector.Vector}
    deriving (Show, Read, Eq)
type FeatureId = Database.Persist.Base.Key Feature
instance Database.Persist.Base.PersistEntity Feature
    where newtype Database.Persist.Base.Key Feature
            = FeatureId {unFeatureId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter Feature
              = FeatureIdIn ([Database.Persist.Base.Key Feature])
              | FeatureUnitEq UnitId
              | FeatureDescriptorEq DescriptorId
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Feature
          data Database.Persist.Base.Order Feature
              = FeatureUnitAsc
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Unique Feature
              = UniqueFeature UnitId DescriptorId
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          entityDef _ = Database.Persist.Base.EntityDef "Feature" [] [(['u',
                                                                        'n',
                                                                        'i',
                                                                        't'],
                                                                       ['U',
                                                                        'n',
                                                                        'i',
                                                                        't',
                                                                        'I',
                                                                        'd'],
                                                                       [['E', 'q'],
                                                                        ['A', 's', 'c']]),
                                                                      (['d',
                                                                        'e',
                                                                        's',
                                                                        'c',
                                                                        'r',
                                                                        'i',
                                                                        'p',
                                                                        't',
                                                                        'o',
                                                                        'r'],
                                                                       ['D',
                                                                        'e',
                                                                        's',
                                                                        'c',
                                                                        'r',
                                                                        'i',
                                                                        'p',
                                                                        't',
                                                                        'o',
                                                                        'r',
                                                                        'I',
                                                                        'd'],
                                                                       [['E', 'q']]),
                                                                      (['v', 'a', 'l', 'u', 'e'],
                                                                       ['M',
                                                                        'e',
                                                                        's',
                                                                        'c',
                                                                        'a',
                                                                        'l',
                                                                        'i',
                                                                        'n',
                                                                        'e',
                                                                        '.',
                                                                        'D',
                                                                        'a',
                                                                        't',
                                                                        'a',
                                                                        'b',
                                                                        'a',
                                                                        's',
                                                                        'e',
                                                                        '.',
                                                                        'V',
                                                                        'e',
                                                                        'c',
                                                                        't',
                                                                        'o',
                                                                        'r',
                                                                        '.',
                                                                        'V',
                                                                        'e',
                                                                        'c',
                                                                        't',
                                                                        'o',
                                                                        'r'],
                                                                       [])] [(['U',
                                                                               'n',
                                                                               'i',
                                                                               'q',
                                                                               'u',
                                                                               'e',
                                                                               'F',
                                                                               'e',
                                                                               'a',
                                                                               't',
                                                                               'u',
                                                                               'r',
                                                                               'e'],
                                                                              [['u', 'n', 'i', 't'],
                                                                               ['d',
                                                                                'e',
                                                                                's',
                                                                                'c',
                                                                                'r',
                                                                                'i',
                                                                                'p',
                                                                                't',
                                                                                'o',
                                                                                'r']])] [['S',
                                                                                          'h',
                                                                                          'o',
                                                                                          'w'],
                                                                                         ['R',
                                                                                          'e',
                                                                                          'a',
                                                                                          'd'],
                                                                                         ['E', 'q']]
          toPersistFields (Feature x_39
                                   x_40
                                   x_41) = [Database.Persist.Base.SomePersistField x_39,
                                            Database.Persist.Base.SomePersistField x_40,
                                            Database.Persist.Base.SomePersistField x_41]
          fromPersistValues [x_42,
                             x_43,
                             x_44] = ((Data.Either.Right Feature Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_42) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_43) Control.Applicative.<*> Database.Persist.Base.fromPersistValue x_44
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Feature undefined undefined undefined
          toPersistKey = FeatureId
          fromPersistKey = unFeatureId
          persistOrderToFieldName (FeatureUnitAsc {}) = "unit"
          persistOrderToOrder (FeatureUnitAsc {}) = Database.Persist.Base.Asc
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (FeatureIdIn {}) = "id"
          persistFilterToFieldName (FeatureUnitEq {}) = "unit"
          persistFilterToFieldName (FeatureDescriptorEq {}) = "descriptor"
          persistFilterToValue (FeatureIdIn x_45) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_45
          persistFilterToValue (FeatureUnitEq x_46) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_46
          persistFilterToValue (FeatureDescriptorEq x_47) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_47
          persistUniqueToFieldNames (UniqueFeature {}) = ["unit",
                                                          "descriptor"]
          persistUniqueToValues (UniqueFeature x_48
                                               x_49) = [Database.Persist.Base.toPersistValue x_48,
                                                        Database.Persist.Base.toPersistValue x_49]
          persistUniqueKeys (Feature _unit_50
                                     _descriptor_51
                                     _value_52) = [UniqueFeature _unit_50 _descriptor_51]
          persistFilterToFilter (FeatureIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (FeatureUnitEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (FeatureDescriptorEq {}) = Database.Persist.Base.Eq
migrateAll :: forall m . Control.Monad.IO.Control.MonadControlIO m =>
                         Database.Persist.GenericSql.Migration (Database.Persist.GenericSql.Raw.SqlPersist m)
migrateAll = do Database.Persist.GenericSql.migrate (GHC.Err.undefined :: SourceFile)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Unit)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Descriptor)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Feature)
