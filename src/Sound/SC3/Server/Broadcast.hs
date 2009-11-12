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
