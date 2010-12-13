{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Mescaline.Database.SourceFile where

-- import           Data.Accessor.Template (nameDeriveAccessors)
import qualified Data.Binary as Binary
import           Data.List (intercalate)
import           Data.Word (Word32)
import           Database.HDBC (SqlType(..))
import           Mescaline.Data.ByteString as BS
import           Mescaline.Data.Unique (Unique)
import qualified Mescaline.Data.Unique as Unique
import           Mescaline.Database.Hash (Hash)
import qualified Mescaline.Database.Hash as Hash
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Util as SF
import           Prelude hiding (id)
-- import           Sound.File.Sndfile (Count)
import           Text.Printf (printf)

type URL = String

-- | Sourcefile.
data SourceFile = SourceFile {
    id          :: !Unique.Id
  , url         :: !URL
  , hash        :: !Hash.Hash

  , numChannels :: {-# UNPACK #-} !Int
  , sampleRate  :: {-# UNPACK #-} !Double
  , frames      :: {-# UNPACK #-} !SF.Count
} deriving (Show)

-- $(nameDeriveAccessors ''SourceFile (return.(++"_")))

namespace :: Unique.Namespace
namespace = Unique.mkNamespace "d03a5083-8958-484e-a441-b3bf75d68818"

path :: SourceFile -> URL
path = url

instance Eq (SourceFile) where
    a == b = id a == id b

instance Ord (SourceFile) where
    a `compare` b = id a `compare` id b

instance Unique (SourceFile) where
    uuid = id

-- | Construct a SourceFile.
-- The id is expected to be valid.
unsafeCons :: Unique.Id -> URL -> Hash -> Int -> Double -> SF.Count -> SourceFile
unsafeCons = SourceFile

-- | Construct a SourceFile.
-- The id is generated from the SourceFile contents
cons :: URL -> Hash -> Int -> Double -> SF.Count -> SourceFile
cons u h nc sr nf = unsafeCons (Unique.fromBinary namespace p) u h nc sr nf
    where p = Binary.put u -- >> Binary.put h

-- | Create a new SourceFile from a locally available sound file.
newLocal :: FilePath -> IO SourceFile
newLocal path = do
    info <- SF.getInfo path
    hash <- Hash.fromFile path
    return $ cons path hash
                (SF.channels info)
                (fromIntegral $ SF.samplerate info)
                (SF.frames info)
