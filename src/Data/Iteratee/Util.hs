module Data.Iteratee.Util where

import Data.Iteratee as I
import Data.Iteratee.Base.StreamChunk (StreamChunk)
import Prelude hiding (until)

find :: (StreamChunk s el, Monad m) => (el -> Bool) -> IterateeG s el m el
find f = I.dropWhile (not.f) >> I.head
