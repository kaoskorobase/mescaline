{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Mescaline.Database.Entity where
import qualified Control.Monad.IO.Control
import qualified Data.Either
import qualified Data.Int
import qualified Database.Persist.Base
import qualified Database.Persist.GenericSql
import qualified Database.Persist.GenericSql.Raw
import qualified Database.Persist.TH.Library
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Err
import qualified GHC.Read
import qualified GHC.Show
import qualified Mescaline.Database.Hash
-- Prelude >>>
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
              | SourceFileUrlNe FilePath
              | SourceFileHashEq Mescaline.Database.Hash.Hash
              | SourceFileHashNe Mescaline.Database.Hash.Hash
              | SourceFileNumChannelsEq Int
              | SourceFileNumChannelsNe Int
              | SourceFileNumChannelsLt Int
              | SourceFileNumChannelsLe Int
              | SourceFileNumChannelsGt Int
              | SourceFileNumChannelsGe Int
              | SourceFileNumChannelsIn ([Int])
              | SourceFileSampleRateEq Double
              | SourceFileSampleRateNe Double
              | SourceFileSampleRateLt Double
              | SourceFileSampleRateLe Double
              | SourceFileSampleRateGt Double
              | SourceFileSampleRateGe Double
              | SourceFileSampleRateIn ([Double])
              | SourceFileFramesEq Data.Int.Int64
              | SourceFileFramesNe Data.Int.Int64
              | SourceFileFramesLt Data.Int.Int64
              | SourceFileFramesLe Data.Int.Int64
              | SourceFileFramesGt Data.Int.Int64
              | SourceFileFramesGe Data.Int.Int64
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
                                                                          [['E', 'q'], ['N', 'e']]),
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
                                                                          [['E', 'q'], ['N', 'e']]),
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
                                                                          [['E', 'q'],
                                                                           ['N', 'e'],
                                                                           ['L', 't'],
                                                                           ['L', 'e'],
                                                                           ['G', 't'],
                                                                           ['G', 'e'],
                                                                           ['I', 'n']]),
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
                                                                          [['E', 'q'],
                                                                           ['N', 'e'],
                                                                           ['L', 't'],
                                                                           ['L', 'e'],
                                                                           ['G', 't'],
                                                                           ['G', 'e'],
                                                                           ['I', 'n']]),
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
                                                                          [['E', 'q'],
                                                                           ['N', 'e'],
                                                                           ['L', 't'],
                                                                           ['L', 'e'],
                                                                           ['G', 't'],
                                                                           ['G', 'e']])] [(['U',
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
                             x_9] = ((((Data.Either.Right SourceFile `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_5) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_6) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_7) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_8) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_9
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
          persistFilterToFieldName (SourceFileUrlNe {}) = "url"
          persistFilterToFieldName (SourceFileHashEq {}) = "hash"
          persistFilterToFieldName (SourceFileHashNe {}) = "hash"
          persistFilterToFieldName (SourceFileNumChannelsEq {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsNe {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsLt {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsLe {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsGt {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsGe {}) = "numChannels"
          persistFilterToFieldName (SourceFileNumChannelsIn {}) = "numChannels"
          persistFilterToFieldName (SourceFileSampleRateEq {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateNe {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateLt {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateLe {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateGt {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateGe {}) = "sampleRate"
          persistFilterToFieldName (SourceFileSampleRateIn {}) = "sampleRate"
          persistFilterToFieldName (SourceFileFramesEq {}) = "frames"
          persistFilterToFieldName (SourceFileFramesNe {}) = "frames"
          persistFilterToFieldName (SourceFileFramesLt {}) = "frames"
          persistFilterToFieldName (SourceFileFramesLe {}) = "frames"
          persistFilterToFieldName (SourceFileFramesGt {}) = "frames"
          persistFilterToFieldName (SourceFileFramesGe {}) = "frames"
          persistFilterToValue (SourceFileIdIn x_10) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_10
          persistFilterToValue (SourceFileUrlEq x_11) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_11
          persistFilterToValue (SourceFileUrlNe x_12) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_12
          persistFilterToValue (SourceFileHashEq x_13) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_13
          persistFilterToValue (SourceFileHashNe x_14) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_14
          persistFilterToValue (SourceFileNumChannelsEq x_15) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_15
          persistFilterToValue (SourceFileNumChannelsNe x_16) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_16
          persistFilterToValue (SourceFileNumChannelsLt x_17) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_17
          persistFilterToValue (SourceFileNumChannelsLe x_18) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_18
          persistFilterToValue (SourceFileNumChannelsGt x_19) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_19
          persistFilterToValue (SourceFileNumChannelsGe x_20) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_20
          persistFilterToValue (SourceFileNumChannelsIn x_21) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_21
          persistFilterToValue (SourceFileSampleRateEq x_22) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_22
          persistFilterToValue (SourceFileSampleRateNe x_23) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_23
          persistFilterToValue (SourceFileSampleRateLt x_24) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_24
          persistFilterToValue (SourceFileSampleRateLe x_25) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_25
          persistFilterToValue (SourceFileSampleRateGt x_26) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_26
          persistFilterToValue (SourceFileSampleRateGe x_27) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_27
          persistFilterToValue (SourceFileSampleRateIn x_28) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_28
          persistFilterToValue (SourceFileFramesEq x_29) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_29
          persistFilterToValue (SourceFileFramesNe x_30) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_30
          persistFilterToValue (SourceFileFramesLt x_31) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_31
          persistFilterToValue (SourceFileFramesLe x_32) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_32
          persistFilterToValue (SourceFileFramesGt x_33) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_33
          persistFilterToValue (SourceFileFramesGe x_34) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_34
          persistUniqueToFieldNames (UniqueSourceFile {}) = ["hash"]
          persistUniqueToValues (UniqueSourceFile x_35) = [Database.Persist.Base.toPersistValue x_35]
          persistUniqueKeys (SourceFile _url_36
                                        _hash_37
                                        _numChannels_38
                                        _sampleRate_39
                                        _frames_40) = [UniqueSourceFile _hash_37]
          persistFilterToFilter (SourceFileIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (SourceFileUrlEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileUrlNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (SourceFileHashEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileHashNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (SourceFileNumChannelsEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileNumChannelsNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (SourceFileNumChannelsLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (SourceFileNumChannelsLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (SourceFileNumChannelsGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (SourceFileNumChannelsGe {}) = Database.Persist.Base.Ge
          persistFilterToFilter (SourceFileNumChannelsIn {}) = Database.Persist.Base.In
          persistFilterToFilter (SourceFileSampleRateEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileSampleRateNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (SourceFileSampleRateLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (SourceFileSampleRateLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (SourceFileSampleRateGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (SourceFileSampleRateGe {}) = Database.Persist.Base.Ge
          persistFilterToFilter (SourceFileSampleRateIn {}) = Database.Persist.Base.In
          persistFilterToFilter (SourceFileFramesEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (SourceFileFramesNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (SourceFileFramesLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (SourceFileFramesLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (SourceFileFramesGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (SourceFileFramesGe {}) = Database.Persist.Base.Ge
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
              | UnitSourceFileNe SourceFileId
              | UnitSourceFileIn ([SourceFileId])
              | UnitOnsetEq Double
              | UnitOnsetNe Double
              | UnitOnsetLt Double
              | UnitOnsetLe Double
              | UnitOnsetGt Double
              | UnitOnsetGe Double
              | UnitDurationEq Double
              | UnitDurationNe Double
              | UnitDurationLt Double
              | UnitDurationLe Double
              | UnitDurationGt Double
              | UnitDurationGe Double
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Unit
          data Database.Persist.Base.Order Unit
              = UnitOnsetAsc | UnitOnsetDesc | UnitDurationAsc | UnitDurationDesc
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
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
                                                                    [['E', 'q'],
                                                                     ['N', 'e'],
                                                                     ['I', 'n']]),
                                                                   (['o', 'n', 's', 'e', 't'],
                                                                    ['D', 'o', 'u', 'b', 'l', 'e'],
                                                                    [['E', 'q'],
                                                                     ['N', 'e'],
                                                                     ['L', 't'],
                                                                     ['L', 'e'],
                                                                     ['G', 't'],
                                                                     ['G', 'e'],
                                                                     ['A', 's', 'c'],
                                                                     ['D', 'e', 's', 'c']]),
                                                                   (['d',
                                                                     'u',
                                                                     'r',
                                                                     'a',
                                                                     't',
                                                                     'i',
                                                                     'o',
                                                                     'n'],
                                                                    ['D', 'o', 'u', 'b', 'l', 'e'],
                                                                    [['E', 'q'],
                                                                     ['N', 'e'],
                                                                     ['L', 't'],
                                                                     ['L', 'e'],
                                                                     ['G', 't'],
                                                                     ['G', 'e'],
                                                                     ['A', 's', 'c'],
                                                                     ['D',
                                                                      'e',
                                                                      's',
                                                                      'c']])] [] [['S',
                                                                                   'h',
                                                                                   'o',
                                                                                   'w'],
                                                                                  ['R',
                                                                                   'e',
                                                                                   'a',
                                                                                   'd'],
                                                                                  ['E', 'q']]
          toPersistFields (Unit x_41
                                x_42
                                x_43) = [Database.Persist.Base.SomePersistField x_41,
                                         Database.Persist.Base.SomePersistField x_42,
                                         Database.Persist.Base.SomePersistField x_43]
          fromPersistValues [x_44,
                             x_45,
                             x_46] = ((Data.Either.Right Unit `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_44) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_45) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_46
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Unit undefined undefined undefined
          toPersistKey = UnitId
          fromPersistKey = unUnitId
          persistOrderToFieldName (UnitOnsetAsc {}) = "onset"
          persistOrderToFieldName (UnitOnsetDesc {}) = "onset"
          persistOrderToFieldName (UnitDurationAsc {}) = "duration"
          persistOrderToFieldName (UnitDurationDesc {}) = "duration"
          persistOrderToOrder (UnitOnsetAsc {}) = Database.Persist.Base.Asc
          persistOrderToOrder (UnitOnsetDesc {}) = Database.Persist.Base.Desc
          persistOrderToOrder (UnitDurationAsc {}) = Database.Persist.Base.Asc
          persistOrderToOrder (UnitDurationDesc {}) = Database.Persist.Base.Desc
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (UnitIdIn {}) = "id"
          persistFilterToFieldName (UnitSourceFileEq {}) = "sourceFile"
          persistFilterToFieldName (UnitSourceFileNe {}) = "sourceFile"
          persistFilterToFieldName (UnitSourceFileIn {}) = "sourceFile"
          persistFilterToFieldName (UnitOnsetEq {}) = "onset"
          persistFilterToFieldName (UnitOnsetNe {}) = "onset"
          persistFilterToFieldName (UnitOnsetLt {}) = "onset"
          persistFilterToFieldName (UnitOnsetLe {}) = "onset"
          persistFilterToFieldName (UnitOnsetGt {}) = "onset"
          persistFilterToFieldName (UnitOnsetGe {}) = "onset"
          persistFilterToFieldName (UnitDurationEq {}) = "duration"
          persistFilterToFieldName (UnitDurationNe {}) = "duration"
          persistFilterToFieldName (UnitDurationLt {}) = "duration"
          persistFilterToFieldName (UnitDurationLe {}) = "duration"
          persistFilterToFieldName (UnitDurationGt {}) = "duration"
          persistFilterToFieldName (UnitDurationGe {}) = "duration"
          persistFilterToValue (UnitIdIn x_47) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_47
          persistFilterToValue (UnitSourceFileEq x_48) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_48
          persistFilterToValue (UnitSourceFileNe x_49) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_49
          persistFilterToValue (UnitSourceFileIn x_50) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_50
          persistFilterToValue (UnitOnsetEq x_51) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_51
          persistFilterToValue (UnitOnsetNe x_52) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_52
          persistFilterToValue (UnitOnsetLt x_53) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_53
          persistFilterToValue (UnitOnsetLe x_54) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_54
          persistFilterToValue (UnitOnsetGt x_55) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_55
          persistFilterToValue (UnitOnsetGe x_56) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_56
          persistFilterToValue (UnitDurationEq x_57) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_57
          persistFilterToValue (UnitDurationNe x_58) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_58
          persistFilterToValue (UnitDurationLt x_59) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_59
          persistFilterToValue (UnitDurationLe x_60) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_60
          persistFilterToValue (UnitDurationGt x_61) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_61
          persistFilterToValue (UnitDurationGe x_62) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_62
          persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
          persistUniqueToValues _ = error "Degenerate case, should never happen"
          persistUniqueKeys (Unit _sourceFile_63 _onset_64 _duration_65) = []
          persistFilterToFilter (UnitIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (UnitSourceFileEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (UnitSourceFileNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (UnitSourceFileIn {}) = Database.Persist.Base.In
          persistFilterToFilter (UnitOnsetEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (UnitOnsetNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (UnitOnsetLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (UnitOnsetLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (UnitOnsetGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (UnitOnsetGe {}) = Database.Persist.Base.Ge
          persistFilterToFilter (UnitDurationEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (UnitDurationNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (UnitDurationLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (UnitDurationLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (UnitDurationGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (UnitDurationGe {}) = Database.Persist.Base.Ge
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
              | DescriptorNameNe String
              | DescriptorNameIn ([String])
              | DescriptorDegreeEq Int
              | DescriptorDegreeNe Int
              | DescriptorDegreeLt Int
              | DescriptorDegreeLe Int
              | DescriptorDegreeGt Int
              | DescriptorDegreeGe Int
              | DescriptorDegreeIn ([Int])
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
                                                                          [['E', 'q'],
                                                                           ['N', 'e'],
                                                                           ['I', 'n']]),
                                                                         (['d',
                                                                           'e',
                                                                           'g',
                                                                           'r',
                                                                           'e',
                                                                           'e'],
                                                                          ['I', 'n', 't'],
                                                                          [['E', 'q'],
                                                                           ['N', 'e'],
                                                                           ['L', 't'],
                                                                           ['L', 'e'],
                                                                           ['G', 't'],
                                                                           ['G', 'e'],
                                                                           ['I', 'n']])] [(['U',
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
          toPersistFields (Descriptor x_66
                                      x_67) = [Database.Persist.Base.SomePersistField x_66,
                                               Database.Persist.Base.SomePersistField x_67]
          fromPersistValues [x_68,
                             x_69] = (Data.Either.Right Descriptor `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_68) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_69
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
          persistFilterToFieldName (DescriptorNameNe {}) = "name"
          persistFilterToFieldName (DescriptorNameIn {}) = "name"
          persistFilterToFieldName (DescriptorDegreeEq {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeNe {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeLt {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeLe {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeGt {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeGe {}) = "degree"
          persistFilterToFieldName (DescriptorDegreeIn {}) = "degree"
          persistFilterToValue (DescriptorIdIn x_70) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_70
          persistFilterToValue (DescriptorNameEq x_71) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_71
          persistFilterToValue (DescriptorNameNe x_72) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_72
          persistFilterToValue (DescriptorNameIn x_73) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_73
          persistFilterToValue (DescriptorDegreeEq x_74) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_74
          persistFilterToValue (DescriptorDegreeNe x_75) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_75
          persistFilterToValue (DescriptorDegreeLt x_76) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_76
          persistFilterToValue (DescriptorDegreeLe x_77) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_77
          persistFilterToValue (DescriptorDegreeGt x_78) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_78
          persistFilterToValue (DescriptorDegreeGe x_79) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_79
          persistFilterToValue (DescriptorDegreeIn x_80) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_80
          persistUniqueToFieldNames (UniqueDescriptor {}) = ["name"]
          persistUniqueToValues (UniqueDescriptor x_81) = [Database.Persist.Base.toPersistValue x_81]
          persistUniqueKeys (Descriptor _name_82
                                        _degree_83) = [UniqueDescriptor _name_82]
          persistFilterToFilter (DescriptorIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (DescriptorNameEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (DescriptorNameNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (DescriptorNameIn {}) = Database.Persist.Base.In
          persistFilterToFilter (DescriptorDegreeEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (DescriptorDegreeNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (DescriptorDegreeLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (DescriptorDegreeLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (DescriptorDegreeGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (DescriptorDegreeGe {}) = Database.Persist.Base.Ge
          persistFilterToFilter (DescriptorDegreeIn {}) = Database.Persist.Base.In
data Feature
    = Feature {featureUnit :: UnitId,
               featureDescriptor :: DescriptorId}
    deriving (Show, Read, Eq)
type FeatureId = Database.Persist.Base.Key Feature
instance Database.Persist.Base.PersistEntity Feature
    where newtype Database.Persist.Base.Key Feature
            = FeatureId {unFeatureId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter Feature
              = FeatureIdIn ([Database.Persist.Base.Key Feature])
              | FeatureUnitEq UnitId
              | FeatureUnitNe UnitId
              | FeatureUnitLt UnitId
              | FeatureUnitLe UnitId
              | FeatureUnitGt UnitId
              | FeatureUnitGe UnitId
              | FeatureUnitIn ([UnitId])
              | FeatureDescriptorEq DescriptorId
              | FeatureDescriptorNe DescriptorId
              | FeatureDescriptorIn ([DescriptorId])
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Feature
          data Database.Persist.Base.Order Feature
              = FeatureUnitAsc | FeatureUnitDesc
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
                                                                        ['N', 'e'],
                                                                        ['L', 't'],
                                                                        ['L', 'e'],
                                                                        ['G', 't'],
                                                                        ['G', 'e'],
                                                                        ['I', 'n'],
                                                                        ['A', 's', 'c'],
                                                                        ['D', 'e', 's', 'c']]),
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
                                                                       [['E', 'q'],
                                                                        ['N', 'e'],
                                                                        ['I', 'n']])] [(['U',
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
                                                                                        [['u',
                                                                                          'n',
                                                                                          'i',
                                                                                          't'],
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
                                                                                                   ['E',
                                                                                                    'q']]
          toPersistFields (Feature x_84
                                   x_85) = [Database.Persist.Base.SomePersistField x_84,
                                            Database.Persist.Base.SomePersistField x_85]
          fromPersistValues [x_86,
                             x_87] = (Data.Either.Right Feature `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_86) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_87
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Feature undefined undefined
          toPersistKey = FeatureId
          fromPersistKey = unFeatureId
          persistOrderToFieldName (FeatureUnitAsc {}) = "unit"
          persistOrderToFieldName (FeatureUnitDesc {}) = "unit"
          persistOrderToOrder (FeatureUnitAsc {}) = Database.Persist.Base.Asc
          persistOrderToOrder (FeatureUnitDesc {}) = Database.Persist.Base.Desc
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (FeatureIdIn {}) = "id"
          persistFilterToFieldName (FeatureUnitEq {}) = "unit"
          persistFilterToFieldName (FeatureUnitNe {}) = "unit"
          persistFilterToFieldName (FeatureUnitLt {}) = "unit"
          persistFilterToFieldName (FeatureUnitLe {}) = "unit"
          persistFilterToFieldName (FeatureUnitGt {}) = "unit"
          persistFilterToFieldName (FeatureUnitGe {}) = "unit"
          persistFilterToFieldName (FeatureUnitIn {}) = "unit"
          persistFilterToFieldName (FeatureDescriptorEq {}) = "descriptor"
          persistFilterToFieldName (FeatureDescriptorNe {}) = "descriptor"
          persistFilterToFieldName (FeatureDescriptorIn {}) = "descriptor"
          persistFilterToValue (FeatureIdIn x_88) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_88
          persistFilterToValue (FeatureUnitEq x_89) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_89
          persistFilterToValue (FeatureUnitNe x_90) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_90
          persistFilterToValue (FeatureUnitLt x_91) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_91
          persistFilterToValue (FeatureUnitLe x_92) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_92
          persistFilterToValue (FeatureUnitGt x_93) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_93
          persistFilterToValue (FeatureUnitGe x_94) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_94
          persistFilterToValue (FeatureUnitIn x_95) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_95
          persistFilterToValue (FeatureDescriptorEq x_96) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_96
          persistFilterToValue (FeatureDescriptorNe x_97) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_97
          persistFilterToValue (FeatureDescriptorIn x_98) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_98
          persistUniqueToFieldNames (UniqueFeature {}) = ["unit",
                                                          "descriptor"]
          persistUniqueToValues (UniqueFeature x_99
                                               x_100) = [Database.Persist.Base.toPersistValue x_99,
                                                         Database.Persist.Base.toPersistValue x_100]
          persistUniqueKeys (Feature _unit_101
                                     _descriptor_102) = [UniqueFeature _unit_101 _descriptor_102]
          persistFilterToFilter (FeatureIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (FeatureUnitEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (FeatureUnitNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (FeatureUnitLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (FeatureUnitLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (FeatureUnitGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (FeatureUnitGe {}) = Database.Persist.Base.Ge
          persistFilterToFilter (FeatureUnitIn {}) = Database.Persist.Base.In
          persistFilterToFilter (FeatureDescriptorEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (FeatureDescriptorNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (FeatureDescriptorIn {}) = Database.Persist.Base.In
data Value
    = Value {valueFeature :: FeatureId,
             valueIndex :: Int,
             valueValue :: Double}
    deriving (Show, Read, Eq)
type ValueId = Database.Persist.Base.Key Value
instance Database.Persist.Base.PersistEntity Value
    where newtype Database.Persist.Base.Key Value
            = ValueId {unValueId :: Database.Persist.Base.PersistValue}
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq, Database.Persist.Base.PersistField, GHC.Classes.Ord)
          data Database.Persist.Base.Filter Value
              = ValueIdIn ([Database.Persist.Base.Key Value])
              | ValueFeatureEq FeatureId
              | ValueFeatureNe FeatureId
              | ValueFeatureIn ([FeatureId])
              | ValueValueEq Double
              | ValueValueNe Double
              | ValueValueLt Double
              | ValueValueLe Double
              | ValueValueGt Double
              | ValueValueGe Double
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Update Value
          data Database.Persist.Base.Order Value
              = ValueIndexAsc
              deriving (GHC.Show.Show, GHC.Read.Read, GHC.Classes.Eq)
          data Database.Persist.Base.Unique Value
          entityDef _ = Database.Persist.Base.EntityDef "Value" [] [(['f',
                                                                      'e',
                                                                      'a',
                                                                      't',
                                                                      'u',
                                                                      'r',
                                                                      'e'],
                                                                     ['F',
                                                                      'e',
                                                                      'a',
                                                                      't',
                                                                      'u',
                                                                      'r',
                                                                      'e',
                                                                      'I',
                                                                      'd'],
                                                                     [['E', 'q'],
                                                                      ['N', 'e'],
                                                                      ['I', 'n']]),
                                                                    (['i', 'n', 'd', 'e', 'x'],
                                                                     ['I', 'n', 't'],
                                                                     [['A', 's', 'c']]),
                                                                    (['v', 'a', 'l', 'u', 'e'],
                                                                     ['D', 'o', 'u', 'b', 'l', 'e'],
                                                                     [['E', 'q'],
                                                                      ['N', 'e'],
                                                                      ['L', 't'],
                                                                      ['L', 'e'],
                                                                      ['G', 't'],
                                                                      ['G', 'e']])] [] [['S',
                                                                                         'h',
                                                                                         'o',
                                                                                         'w'],
                                                                                        ['R',
                                                                                         'e',
                                                                                         'a',
                                                                                         'd'],
                                                                                        ['E', 'q']]
          toPersistFields (Value x_103
                                 x_104
                                 x_105) = [Database.Persist.Base.SomePersistField x_103,
                                           Database.Persist.Base.SomePersistField x_104,
                                           Database.Persist.Base.SomePersistField x_105]
          fromPersistValues [x_106,
                             x_107,
                             x_108] = ((Data.Either.Right Value `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_106) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_107) `Database.Persist.TH.Library.apE` Database.Persist.Base.fromPersistValue x_108
          fromPersistValues _ = Data.Either.Left "Invalid fromPersistValues input"
          halfDefined = Value undefined undefined undefined
          toPersistKey = ValueId
          fromPersistKey = unValueId
          persistOrderToFieldName (ValueIndexAsc {}) = "index"
          persistOrderToOrder (ValueIndexAsc {}) = Database.Persist.Base.Asc
          persistUpdateToFieldName _ = error "Degenerate case, should never happen"
          persistUpdateToValue _ = error "Degenerate case, should never happen"
          persistUpdateToUpdate _ = error "Degenerate case, should never happen"
          persistFilterToFieldName (ValueIdIn {}) = "id"
          persistFilterToFieldName (ValueFeatureEq {}) = "feature"
          persistFilterToFieldName (ValueFeatureNe {}) = "feature"
          persistFilterToFieldName (ValueFeatureIn {}) = "feature"
          persistFilterToFieldName (ValueValueEq {}) = "value"
          persistFilterToFieldName (ValueValueNe {}) = "value"
          persistFilterToFieldName (ValueValueLt {}) = "value"
          persistFilterToFieldName (ValueValueLe {}) = "value"
          persistFilterToFieldName (ValueValueGt {}) = "value"
          persistFilterToFieldName (ValueValueGe {}) = "value"
          persistFilterToValue (ValueIdIn x_109) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_109
          persistFilterToValue (ValueFeatureEq x_110) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_110
          persistFilterToValue (ValueFeatureNe x_111) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_111
          persistFilterToValue (ValueFeatureIn x_112) = (Data.Either.Right GHC.Base.. GHC.Base.map Database.Persist.Base.toPersistValue) x_112
          persistFilterToValue (ValueValueEq x_113) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_113
          persistFilterToValue (ValueValueNe x_114) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_114
          persistFilterToValue (ValueValueLt x_115) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_115
          persistFilterToValue (ValueValueLe x_116) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_116
          persistFilterToValue (ValueValueGt x_117) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_117
          persistFilterToValue (ValueValueGe x_118) = (Data.Either.Left GHC.Base.. Database.Persist.Base.toPersistValue) x_118
          persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
          persistUniqueToValues _ = error "Degenerate case, should never happen"
          persistUniqueKeys (Value _feature_119 _index_120 _value_121) = []
          persistFilterToFilter (ValueIdIn {}) = Database.Persist.Base.In
          persistFilterToFilter (ValueFeatureEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (ValueFeatureNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (ValueFeatureIn {}) = Database.Persist.Base.In
          persistFilterToFilter (ValueValueEq {}) = Database.Persist.Base.Eq
          persistFilterToFilter (ValueValueNe {}) = Database.Persist.Base.Ne
          persistFilterToFilter (ValueValueLt {}) = Database.Persist.Base.Lt
          persistFilterToFilter (ValueValueLe {}) = Database.Persist.Base.Le
          persistFilterToFilter (ValueValueGt {}) = Database.Persist.Base.Gt
          persistFilterToFilter (ValueValueGe {}) = Database.Persist.Base.Ge
migrateAll :: forall m . Control.Monad.IO.Control.MonadControlIO m =>
                         Database.Persist.GenericSql.Migration (Database.Persist.GenericSql.Raw.SqlPersist m)
migrateAll = do Database.Persist.GenericSql.migrate (GHC.Err.undefined :: SourceFile)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Unit)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Descriptor)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Feature)
                Database.Persist.GenericSql.migrate (GHC.Err.undefined :: Value)
