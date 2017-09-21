{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Mescaline.Pattern.Event (
    Event(..)
  , fields
  , lookup
  , isRest
  , Field
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
import GHC.Exts (IsList(..))
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
instance ToField [Char] where toField = F_String
instance ToField Field where toField = id

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

data Event =
  Event {
    _fields :: Map.Map String Field
  } deriving (Eq, Show)

makeLenses ''Event

instance IsList Event where
  type Item Event = (String, Field)
  fromList = Event . Map.fromList
  toList = Map.toList . view fields

instance Time.HasDelta (Event) where
  delta = lens (\e -> maybe 1 id $ e ^? (fields . at "delta" . non 1 . f_double))
               (\e d -> e & fields . at "delta" .~ Just (F_Double d))

lookup :: String -> Event -> Maybe Field
lookup key = view (fields . at key)

isRest :: Event -> Bool
isRest e = maybe False (/= 0) $ do
  f <- lookup "rest" e
  f ^? f_double

sound :: String -> Event
sound s = Event $ Map.fromList [("sound", toField s)]

instance IsString Event where
  fromString = sound

instance IsString (P Event) where
  fromString = return . sound

