module Data.Signal.SF.Event (
    Event
  , event
  , eventToMaybe
  , maybeToEvent
  , merge
  , split
) where

import Control.Applicative

-- data Event a = NoEvent | Event a deriving (Eq, Read, Show)
type Event = Maybe

event :: b -> (a -> b) -> Event a -> b
-- event b _ NoEvent   = b
-- event _ f (Event a) = f a
event = maybe

eventToMaybe :: Event a -> Maybe a
-- eventToMaybe = event Nothing Just
eventToMaybe = id

maybeToEvent :: Maybe a -> Event a
-- maybeToEvent = maybe NoEvent Event
maybeToEvent = id

merge :: (a -> a -> a) -> Event a -> Event a -> Event a
-- merge _       NoEvent      NoEvent      = NoEvent
-- merge _       le@(Event _) NoEvent      = le
-- merge _       NoEvent      re@(Event _) = re
-- merge resolve (Event l)    (Event r)    = Event (resolve l r)
merge resolve (Just l) (Just r) = Just (resolve l r)
merge _ l r = l <|> r

split :: Event (a, b) -> (Event a, Event b)
split Nothing = (Nothing, Nothing)
split (Just (a, b)) = (Just a, Just b)

-- instance Functor Event where
--     fmap _ NoEvent   = NoEvent
--     fmap f (Event a) = Event (f a)
-- 
-- instance Applicative Event where
--     pure = Event
--     NoEvent   <*> _         = NoEvent
--     _         <*> NoEvent   = NoEvent
--     (Event f) <*> (Event a) = Event (f a)
-- 
-- instance Alternative Event where
--     empty = NoEvent
--     NoEvent <|> p = p
--     Event a <|> _ = Event a
-- 
-- instance Monad Event where
--     (Event x) >>= k = k x
--     NoEvent   >>= _ = NoEvent
-- 
--     (Event _) >>  k = k
--     NoEvent   >>  _ = NoEvent
-- 
--     return          = Event
--     fail _          = NoEvent
