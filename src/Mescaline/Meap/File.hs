module Mescaline.Meap.File (
    Record, Content,
    parse
) where

import Data.ByteString                      (ByteString)
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Lex.Double as ByteString
import Data.Char                            (ord)
import Data.Maybe

type Record  = [Double]
type Content = [Record]

bhash  = fromIntegral (ord '#')
bnl    = fromIntegral (ord '\n')
bspace = fromIntegral (ord ' ')

readDouble :: ByteString -> Maybe Double
readDouble s = case ByteString.readDouble s of
                    Just (v, _) -> Just v
                    _           -> Nothing

readRecord :: ByteString -> Maybe Record
readRecord s =
    if ByteString.null s || ByteString.head s == bhash
    then Just []
    else mapM readDouble (tail {- drop file name -} (filter (not.ByteString.null) (ByteString.split bspace s)))

parse :: ByteString -> Maybe Content
parse s = filter (not.null) `fmap` (mapM readRecord (ByteString.split bnl s))
