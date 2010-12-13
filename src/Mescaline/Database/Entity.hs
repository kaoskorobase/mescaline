{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, QuasiQuotes #-}
module Mescaline.Database.Entity where

import           Database.Persist.Sqlite
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import           Data.Int (Int64)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
-- import Control.Monad.IO.Class (liftIO)
import           Database.Persist (PersistField(..))
import           Database.Persist.Base (PersistValue(..), SqlType(..))
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

fromList :: [Double] -> Vector
fromList = Vector . V.fromList

mkPersist [$persist|
SourceFile
    url         FilePath Eq
    hash        Hash.Hash Eq
    numChannels Int
    sampleRate  Double
    frames      Int64
    UniqueSourceFile url
Unit
    sourceFile  SourceFileId Eq
    onset       Double
    duration    Double
Descriptor
    name        String
    degree      Int
Feature
    unit        UnitId Eq
    descriptor  DescriptorId Eq
    value       Vector
|]

-- main :: IO ()
-- main = withSqliteConn "test.db" $ runSqlConn go
-- 
-- go :: SqlPersist IO ()
-- go = do
--     runMigration $ migrate (undefined :: SourceFile) >> migrate (undefined :: Unit)
--     let sf0 = SourceFile "/User/sk/Music/dam.wav" 2
--     b <- checkUnique sf0
--     if b
--         then do
--             sf <- insert sf0
--             liftIO $ print sf
--             mapM_ insert [Unit sf 0.2 0.8, Unit sf 1.0 0.1, Unit sf 1.1 0.6]
--             -- liftIO $ print u
--         else do
--             Just (sfId, sf) <- getBy (UniqueSourceFile "/User/sk/Music/dam.wav")
--             liftIO $ print (sfId, sf)
--             us <- run_ (select [UnitSourceFileEq sfId] [] 0 0 $$ consume)
--             liftIO $ print us

  -- p1 <- get key
  -- liftIO $ print p1
  -- update key [PersonAge 26]
  -- p2 <- get key
  -- liftIO $ print p2
  -- p3 <- selectList [PersonNameEq "Michael"] [] 0 0
  -- liftIO $ print p3
  -- delete key
  -- p4 <- selectList [PersonNameEq "Michael"] [] 0 0
  -- liftIO $ print p4
