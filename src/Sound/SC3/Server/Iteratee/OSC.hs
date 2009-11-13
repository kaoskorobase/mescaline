{-# LANGUAGE FlexibleContexts #-}
module Sound.SC3.Server.Iteratee.OSC (
    waitFor
  , wait
) where

import Data.Iteratee as I
import Data.Iteratee.Base.StreamChunk (StreamChunk)
import Sound.OpenSoundControl (OSC(..))
import Sound.SC3.Server.State (NodeId)

-- | Wait for an OSC message according to the supplied predicate.
waitFor :: (StreamChunk s OSC, Monad m) => (OSC -> Maybe a) -> IterateeG s OSC m a
waitFor f = do
    h <- I.head
    case f h of
        Just a  -> return a
        Nothing -> waitFor f

-- | Wait for trigger message from a SendTrig UGen, matching the node id and,
-- optionally, the trigger id.
-- waitForTrig :: (StreamChunk s OSC, Monad m) => NodeId -> Maybe Int -> IterateeG s OSC m Double
-- waitForTrig n i = waitFor (tr n i)

-- | Wait for an OSC message matching a specific address.
wait :: (StreamChunk s OSC, Monad m) => String -> IterateeG s OSC m OSC
wait = waitFor . has_address
    where
        has_address x m@(Message y _) | x == y = Just m
        has_address _ _                        = Nothing
