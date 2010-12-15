{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes #-}
module Mescaline.Database.Entity where

import           Database.Persist.Sqlite
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
-- import Control.Monad.IO.Class (liftIO)
import           Database.Persist (PersistField(..), PersistBackend)
import qualified Database.Persist as DB
import           Database.Persist.Base (PersistValue(..), SqlType(..))
import qualified Database.Persist.GenericSql as DB
import qualified Mescaline.Data.ByteString as B
import qualified Mescaline.Database.Hash as Hash
import qualified Sound.OpenSoundControl.Byte as OSC
import           Text.Read

newtype Vector = Vector { toVector :: SV.Vector Double } deriving (Eq)

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

instance PersistField Vector where
    toPersistValue (Vector v) = toPersistValue $
        BS.concat $
            B.toChunks (OSC.encode_i32 (V.length v))
         ++ concatMap (B.toChunks.OSC.encode_f64) (V.toList v)
    fromPersistValue v =
        case v of
            PersistByteString bs ->
                let b = B.fromChunks [bs]
                    n = OSC.decode_i32 (B.take 4 b)
                    v = V.fromList (map OSC.decode_f64 (B.segment 8 (B.drop 4 b)))
                in Right (Vector v)
            p -> Left ("Couldn't create Vector from PersistValue " ++ show p)
    sqlType _ = SqlBlob

fromVector :: SV.Vector Double -> Vector
fromVector = Vector

fromList :: [Double] -> Vector
fromList = Vector . V.fromList

mkPersist [$persist|
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

type SourceFileMap = Map (Key SourceFile) SourceFile

migrate :: DB.SqlPersist IO ()
migrate = DB.runMigration $ do
    DB.migrate (undefined :: SourceFile)
    DB.migrate (undefined :: Unit)
    DB.migrate (undefined :: Descriptor)
    DB.migrate (undefined :: Feature)

getDescriptor :: PersistBackend m => String -> Int -> m DescriptorId
getDescriptor name degree = do
    x <- getBy (UniqueDescriptor name)
    case x of
        Nothing -> insert (Descriptor name degree)
        Just (d, _) -> return d
