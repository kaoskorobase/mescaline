{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Mescaline.Analysis.JSON where

import           Control.Applicative
import           Data.Aeson as J
import qualified Data.Text as Text
import           Mescaline.Analysis.Types
import qualified Mescaline.Database.Hash as Hash

instance ToJSON SoundFile where
    toJSON x = object [
        "hash"        .= Hash.toString (hash x)
      , "numChannels" .= numChannels x
      , "sampleRate"  .= sampleRate x
      , "frames"      .= frames x
      ]

v .% s = (v .: s) <|> fail ("Couldn't access " ++ (Text.unpack s))

instance FromJSON SoundFile where
    parseJSON (Object v) =
        SoundFile
            <$> v .% "url"
            <*> (Hash.fromString <$> v .% "hashvalue")
            <*> v .% "channels"
            <*> v .% "sampleRate"
            <*> v .% "frames"
    parseJSON _ = fail "SoundFile"

instance ToJSON Unit where
    toJSON x = object [
        "onset"    .= onset x
      , "duration" .= duration x
      ]

instance FromJSON Unit where
    parseJSON (Object v) = Unit
                            <$> v .: "onset"
                            <*> v .: "duration"
    parseJSON _          = fail "Unit"

instance ToJSON Descriptor where
    toJSON x = object [
        "name"   .= name x
      , "degree" .= degree x
      ]

instance FromJSON Descriptor where
    parseJSON (Object v) = Descriptor
                            <$> v .% "name"
                            <*> v .% "degree"
    parseJSON _          = fail "Descriptor"

instance ToJSON Feature where
    toJSON x = object [
        "descriptor" .= descriptor x
      , "value"      .= value x
      ]

instance FromJSON Feature where
    parseJSON o@(Object v) = Feature
                                <$> parseJSON o
                                <*> v .% "value"
    parseJSON _          = fail "Feature"

instance ToJSON Analysis where
    toJSON x = object [
        "soundFile" .= soundFile x
      , "units"     .= map mkPair (units x)
      ]
      where
          mkPair (u, fs) = object [ "unit" .= u, "features" .= fs ]

instance FromJSON Analysis where
    parseJSON (Object o) = do
		sf <- parseJSON =<< o .% "soundfile"
		o' <- o .% "soundfile"
		us <- mapM mkPair =<< (o' .% "units")
		return $ Analysis sf us
		where
			mkPair o@(Object v) = do
				u <- parseJSON o
				fs <- v .% "features"
				return (u, fs)
			mkPair _ = fail "mkPair"
    parseJSON _ = fail "Analysis"
