module Reactive.Banana.EventSource
  ( EventSource
  , newEventSource
  , addHandler
  , fromEventSource
  , fire
  ) where

import Data.IORef
import Reactive.Banana

{-----------------------------------------------------------------------------
Event sources
------------------------------------------------------------------------------}

-- Event Sources - allows you to register event handlers
-- Your GUI framework should provide something like this for you 

data EventSource a = EventSource {
    setHandler :: (a -> IO ()) -> IO ()
  , getHandler :: IO (a -> IO ())
  }

newEventSource :: IO (EventSource a)
newEventSource = do
    ref <- newIORef (const $ return ())
    return EventSource { setHandler = writeIORef ref
                       , getHandler = readIORef ref }

addHandler :: EventSource a -> AddHandler a
addHandler es k = do
    handler <- getHandler es
    setHandler es (\x -> handler x >> k x)

fromEventSource :: Typeable a => EventSource a -> Prepare (Event a)
fromEventSource = fromAddHandler . addHandler

fire :: EventSource a -> a -> IO ()
fire es x = getHandler es >>= ($x)
