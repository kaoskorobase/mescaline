{-# LANGUAGE FlexibleContexts #-}
module Mescaline.Database.Vector (
    Vector
  , GVector
  , fromVector
  , toVector
  , fromList
  , toList
) where

import           Control.DeepSeq (NFData(..))
import           Database.Persist.Sqlite
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import           Database.Persist (PersistField(..))
import qualified Database.Persist as DB
import           Database.Persist.Base (PersistValue(..), SqlType(..))
import qualified Sound.OpenSoundControl.Byte as OSC
import           Text.Read

type GVector = SV.Vector Double
newtype Vector = Vector { toVector :: GVector } deriving (Eq)

instance Show Vector where
    show x = "fromList " ++ show (V.toList (toVector x))

instance Read Vector where
    readsPrec = readPrec_to_S $ do
        i <- lexP
        case i of
            Ident "fromList" -> do
                xs <- readListPrec
                return $ Vector (V.fromList xs)
            _ -> pfail

instance NFData Vector where
    rnf (Vector x1) = x1 `seq` ()

instance PersistField Vector where
    toPersistValue (Vector v) = toPersistValue $
        BS.concat $
            B.toChunks (OSC.encode_i32 (V.length v))
         ++ concatMap (B.toChunks.OSC.encode_f64) (V.toList v)
    fromPersistValue v =
        case v of
            PersistByteString bs ->
                let b = B.fromChunks [bs]
                    -- n = OSC.decode_i32 (B.take 4 b)
                    v = V.fromList (map OSC.decode_f64 (segment 8 (B.drop 4 b)))
                in Right (Vector v)
            p -> Left ("Couldn't create Vector from PersistValue " ++ show p)
        where
            segment n b =
                if B.null b
                    then []
                    else B.take n b : segment n (B.drop n b)
    sqlType _ = SqlBlob

fromVector :: V.Vector v Double => v Double -> Vector
fromVector = Vector . V.unstream . V.stream

fromList :: [Double] -> Vector
fromList = Vector . V.fromList

toList :: Vector -> [Double]
toList = V.toList . toVector
