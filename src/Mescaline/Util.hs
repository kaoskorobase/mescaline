module Mescaline.Util (
    readMaybe
) where

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                [(a, "")] -> return a
                _         -> Nothing
