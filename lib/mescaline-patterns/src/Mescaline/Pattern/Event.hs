{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Mescaline.Pattern.Event (
    Event(..)
  , field
  , isRest
  , Field
  , ToField(..)
  , FromField(..)
  , f_double
  , f_vector
  , f_string
  , f_ctrl
  , sound
) where

import           Control.Lens
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.String (IsString(..))
import           GHC.Exts (IsList(..))
import           Mescaline.Time (Duration)
import qualified Mescaline.Time as Time
import           Prelude hiding (lookup)
import           Sound.SC3.Lang.Pattern.P

data Field =
    F_Double { _f_double :: Double }
  | F_String { _f_string :: String }
  | F_Vector { _f_vector :: [Field] }
  | F_Controller { _f_ctrl :: Int }
  deriving (Eq, Show)

makeLenses ''Field

class ToField a where
  toField :: a -> Field

instance ToField Bool where toField = F_Double . fromIntegral . fromEnum
instance ToField Int where toField = F_Double . fromIntegral
instance ToField Double where toField = F_Double
instance ToField String where toField = F_String
instance ToField Field where toField = id

class FromField a where
  fromField :: Field -> Maybe a

instance FromField Bool where
  fromField (F_Double d) = Just (d /= 0)
  fromField _ = Nothing

instance FromField Int where
  fromField (F_Double d) = Just (truncate d)
  fromField _ = Nothing

instance FromField Double where
  fromField (F_Double d) = Just d
  fromField _ = Nothing

instance FromField String where
  fromField (F_String s) = Just s
  fromField _ = Nothing

instance FromField Field where
  fromField = Just

instance IsString Field where
  fromString = F_String

instance IsList Field where
  type Item Field = Field
  fromList = F_Vector
  toList f = f ^. f_vector

check1 :: Show a => a -> Maybe b -> b
check1 a Nothing = error $ "Type mismatch: " ++ show a
check1 _ (Just b) = b

f_unop :: (Double -> Double) -> Field -> Field
f_unop f a = F_Double . check1 a $ f <$> (a ^? f_double)

check2 :: Show a => a -> a -> Maybe b -> b
check2 a1 a2 Nothing = error $ "Type mismatch: " ++ show a1 ++ ", " ++ show a2
check2 _ _ (Just b) = b

f_binop :: (Double -> Double -> Double) -> Field -> Field -> Field
f_binop f a b = F_Double . check2 a b $ f <$> (a ^? f_double) <*> (b ^? f_double)

instance Num Field where
  (*) = f_binop (*)
  (+) = f_binop (+)
  (-) = f_binop (-)
  negate = f_unop negate
  abs = f_unop abs
  signum = f_unop signum
  fromInteger = F_Double . fromInteger

type Key = String

newtype Event =
  Event {
    _fields :: Map.Map Key Field
  } deriving (Eq, Show)

makeLenses ''Event

type instance Index Event = Key
type instance IxValue Event = Field

instance Ixed Event where
  ix i f (Event m) = fmap Event (ix i f m)

instance At Event where
  at k = fields . at k

-- FIXME: Make this an Iso'?
fieldish :: (FromField a, ToField a) => Lens' (Maybe Field) (Maybe a)
fieldish = lens (>>= fromField) (\_ a -> toField <$> a)

field :: (FromField a, ToField a) => Key -> Lens' Event (Maybe a)
field key = at key . fieldish

instance IsList Event where
  type Item Event = (Key, Field)
  fromList = Event . Map.fromList
  toList = Map.toList . view fields

instance Time.HasDelta (Event) where
  -- FIXME: How to define delta by composing lenses?
  delta = lens (\e -> e ^. field "delta" . non 1)
               (\e d -> e & field "delta" .~ Just d)

isRest :: Event -> Bool
isRest e = e ^. field "rest" . non False

sound :: String -> Event
sound s = fromList [("sound", toField s)]

instance IsString Event where
  fromString = sound

