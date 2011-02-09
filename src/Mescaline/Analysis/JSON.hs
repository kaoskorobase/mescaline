{-# LANGUAGE OverloadedStrings #-}
module Mescaline.Analysis.JSON where

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

instance ToJSON Unit where
    toJSON x = object [
        "onset"    .= onset x
      , "duration" .= duration x
      ]

instance ToJSON Descriptor where
    toJSON x = object [
        "name"   .= Text.pack (name x)
      , "degree" .= degree x
      ]

instance ToJSON Feature where
    toJSON x = object [
        "descriptor" .= descriptor x
      , "value"      .= value x
      ]

instance ToJSON Analysis where
    toJSON x = object [
        "soundFile" .= soundFile x
      , "units"     .= units x
      ]
