{-# LANGUAGE FlexibleContexts #-}
module Data.Iteratee.OSC (
    waitFor
  , wait
) where

import Data.Iteratee as I
import Data.Iteratee.Base.StreamChunk (StreamChunk)
import Sound.OpenSoundControl (OSC(..))

-- | Wait for an OSC message according to the supplied predicate.
waitFor :: (StreamChunk s OSC, Monad m) => (OSC -> Maybe a) -> IterateeG s OSC m a
waitFor f = do
    h <- I.head
    case f h of
        Just a  -> return a
        Nothing -> waitFor f

-- | Wait for an OSC message matching a specific address.
wait :: (StreamChunk s OSC, Monad m) => String -> IterateeG s OSC m OSC
wait = waitFor . has_address
    where
        has_address x m@(Message y _) | x == y = Just m
        has_address _ _                        = Nothing
