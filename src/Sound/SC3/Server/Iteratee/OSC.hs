{-# LANGUAGE FlexibleContexts #-}
module Sound.SC3.Server.Iteratee.OSC where

import Data.Iteratee
import Data.Iteratee.Base.StreamChunk (StreamChunk)
import Sound.OpenSoundControl (OSC(..))
import Sound.SC3.Server.Iteratee.Base as I

-- | Wait for an OSC message where the supplied function does not give
--   Nothing, discarding intervening messages.
waitFor :: (StreamChunk s OSC, Monad m) => (OSC -> Bool) -> IterateeG s OSC m OSC
waitFor = I.find

-- -- | Wait for an OSC message matching a specific address.
wait :: (StreamChunk s OSC, Monad m) => String -> IterateeG s OSC m OSC
wait = waitFor . has_address
    where
        has_address x (Message y _) = x == y
        has_address x (Bundle _ xs) = any (has_address x) xs
