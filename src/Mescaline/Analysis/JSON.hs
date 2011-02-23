{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Mescaline.Analysis.JSON where

import           Control.Applicative
import           Data.Aeson as J
import qualified Data.Text as Text
import           Mescaline.Analysis.Types
import qualified Mescaline.Database.Hash as Hash

instance ToJSON SoundFile where
    toJSON x = object [
        -- "path" .= "foo"
        "hash"        .= Text.pack (Hash.toString (hash x))
      , "numChannels" .= numChannels x
      , "sampleRate"  .= sampleRate x
      , "frames"      .= frames x
      ]

instance FromJSON SoundFile where
    fromJSON (Object v) =
        SoundFile <$>
            (Text.unpack <$> v .: "url") <*>
            ((Hash.fromString . Text.unpack) <$> v .: "hashvalue") <*>
            v .: "channels" <*>
            v .: "sampleRate" <*>
            v .: "frames"
    fromJSON _ = empty

instance ToJSON Unit where
    toJSON x = object [
        "onset"    .= onset x
      , "duration" .= duration x
      ]

instance FromJSON Unit where
    fromJSON (Object v) = Unit <$>
                            v .: "onset" <*>
                            v .: "duration"
    fromJSON _          = empty

instance ToJSON Descriptor where
    toJSON x = object [
        "name"   .= Text.pack (name x)
      , "degree" .= degree x
      ]

instance FromJSON Descriptor where
    fromJSON (Object v) = Descriptor <$>
                            (Text.unpack <$> v .: "name") <*>
                            v .: "degree"
    fromJSON _          = empty

instance ToJSON Feature where
    toJSON x = object [
        "descriptor" .= descriptor x
      , "value"      .= value x
      ]

instance FromJSON Feature where
    fromJSON o@(Object v) = Feature <$>
                                fromJSON o <*>
                                v .: "value"
    fromJSON _          = empty

newtype UnitFeaturePair = UFP { unUFP :: (Unit, [Feature]) }

instance ToJSON UnitFeaturePair where
    toJSON (UFP (u, fs)) = object [ "unit" .= u, "features" .= fs ]
        
instance FromJSON UnitFeaturePair where
    fromJSON o@(Object v) = UFP <$> ((,) <$> fromJSON o <*> v .: "features")
    fromJSON _ = empty

instance ToJSON Analysis where
    toJSON x = object [
        "soundFile" .= soundFile x
      , "units"     .= map UFP (units x)
      ]
      -- where
      --     mkPair (u, fs) = object [ "unit" .= u, "features" .= fs ]

newtype WrappedAnalysis = WrappedAnalysis { unWrapAnalysis :: Analysis }

instance FromJSON WrappedAnalysis where
    fromJSON o@(Object v) = WrappedAnalysis <$> (Analysis <$> fromJSON o <*> (map unUFP <$> v .: "units"))
    fromJSON _ = empty

instance FromJSON Analysis where
    fromJSON (Object v) = unWrapAnalysis <$> v .: "soundfile"
    fromJSON _ = empty
