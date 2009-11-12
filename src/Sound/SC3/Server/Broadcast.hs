-- | Response processing through iteratees.
--  
-- The implementation is based on the idea of a Broadcast that broadcasts
-- messages to any number of registered consumers. The consumers are defined
-- in terms of iteratees, i.e. they reduce the stream of replies to some
-- return value when they finally terminate.
--  
-- The implementation is based on Control.Concurrent.Chan, which makes it
-- unnecessary to handle consumer management explicitly: A consumer registers
-- an IO action that is supposed to initiate some communication with an
-- external entity and returns an iteratee to "consume" the generated replies.
-- Internally, the main communication channel is duplicated, so that anything
-- delivered will be available on the consumer's channel, too. The IO action
-- is invoked after the channel is duplicated, eliminating a possible race
-- condition, where the request is sent and the reply is received before the
-- channel has been duplicated.
--  
-- An enumerator is provided for enumerating a Control.Concurrent.Chan, as
-- well as some iteratees specific to OSC message processing.
module Sound.SC3.Server.Broadcast (
    Broadcast
  , Consumer
  , new
  , close
  , broadcastList
  , broadcast
  , consume
) where

import Control.Concurrent.Chan
import Data.Iteratee (IterateeG, run)
import Sound.SC3.Server.Iteratee (ChanMsg(..), enumChan)

-- Internal data type used for message queue elements.

-- | Dispatcher dispatching objects of type a.
newtype Broadcast a = Broadcast {
    messages :: Chan (ChanMsg [a])
}

broadcastMsg :: Broadcast a -> ChanMsg [a] -> IO ()
broadcastMsg (Broadcast c) m = writeChan c m >> readChan c >> return ()

new :: IO (Broadcast a)
new = Broadcast `fmap` newChan

close :: Broadcast a -> IO ()
close = flip broadcastMsg CloseChan

broadcastList :: Broadcast a -> [a] -> IO ()
broadcastList d = broadcastMsg d . ChanMsg

broadcast :: Broadcast a -> a -> IO ()
broadcast d a = broadcastList d [a]

-- | Consumer defined in terms of a stream processor (iteratee).
type Consumer a b = IterateeG [] a IO b

consume :: Broadcast a -> IO (Consumer a b) -> IO b
consume d a = do
    c <- dupChan (messages d)
    a >>= enumChan c >>= run
