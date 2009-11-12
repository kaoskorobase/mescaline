module Sound.SC3.Server.Iteratee.Base where

import Data.Iteratee as I
import Data.Iteratee.Base.StreamChunk (StreamChunk)

find :: (StreamChunk s el, Monad m) => (el -> Bool) -> IterateeG s el m el
find f = I.dropWhile (not.f) >> I.head
