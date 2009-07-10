module Mescaline.Data.ByteString (
    segment
) where

import Data.ByteString.Lazy             (ByteString)
import qualified Data.ByteString.Lazy   as ByteString
import Database.HDBC                    (SqlType(..), SqlValue(..))
import Data.Int                         (Int64)

instance SqlType (ByteString) where
    toSql _              = undefined
    -- fromSql (SqlByteString x) =  B.pack (BS.unpack x)
    fromSql (SqlString s) =  ByteString.pack (map (toEnum . fromEnum) s)
    fromSql _             = error "fromSql: cannot convert to ByteString"

segment :: Int64 -> ByteString -> [ByteString]
segment n b =
    if ByteString.null b
        then []
        else ByteString.take n b : segment n (ByteString.drop n b)
