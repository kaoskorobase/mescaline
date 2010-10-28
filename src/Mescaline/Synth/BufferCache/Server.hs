module Mescaline.Synth.BufferCache.Server (
    BufferCache
  , Buffer
  , uid
  , numChannels
  , numFrames
  , Alloc
  , allocBytes
  , allocFrames
  , new
  , release
  , allocBuffer
  , freeBuffer
) where

import           Control.Concurrent.MonadIO (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad (forM)
import qualified Data.Set as Set
import qualified Mescaline.Application.Logger as Log
import           Mescaline.Synth.BufferCache (Alloc, Buffer, uid, numChannels, numFrames, allocBytes, allocFrames)
import qualified Mescaline.Synth.BufferCache as BC
import qualified Mescaline.Synth.BufferCache.Alloc as Alloc
import           Sound.OpenSoundControl (OSC, Transport)
import           Sound.SC3 hiding (free)
import           Sound.SC3.Server.Command.Completion
import           Sound.SC3.Server.Monad as S
import qualified Sound.SC3.Server.State as State

type BufferCache = MVar BC.BufferCache

new :: [Alloc] -> Server BufferCache
new allocs = do
    buffers <- forM allocs $ \a -> do
        bid <- S.alloc bufferId
        let buf = BC.fromAlloc bid a
        S.sync $ S.send $ b_alloc (fromIntegral bid)
                                  (BC.numFrames buf)
                                  (BC.numChannels buf)
        return buf
    newMVar $ BC.fromList buffers

release :: BufferCache -> Server ()
release bc = do
    cache <- takeMVar bc
    mapM_
        (S.sync . S.send . b_free . fromIntegral . BC.uid)
        (Set.elems (BC.usedBuffers cache)
            ++ Set.elems (BC.freeBuffers cache))
    putMVar bc BC.empty

allocBuffer :: (Buffer -> Maybe OSC) -> BufferCache -> Alloc -> Server Buffer
allocBuffer completion bc alloc = do
    cache <- takeMVar bc
    (cache', buf) <-
        case BC.allocBuffer cache alloc of
            Nothing -> do
                -- Allocate buffer id
                bid <- S.alloc bufferId
                liftIO $ Log.infoM "BufferCache" $ "cache miss, allocating new buffer " ++ show bid
                let buf = BC.fromAlloc bid alloc
                    msg = maybe b_alloc (($) b_alloc') (completion buf)
                -- Allocate buffer
                S.sync $ S.send $ msg (fromIntegral bid)
                                      (BC.numFrames buf)
                                      (BC.numChannels buf)
                -- Insert into used
                return (BC.insertBuffer cache buf, buf)
            Just (cache', buf) -> do
                maybe (return ()) (S.async . S.send) (completion buf)
                return (cache', buf)
    putMVar bc cache'
    return buf

freeBuffer :: BufferCache -> Buffer -> Server ()
freeBuffer bc buf = do
    cache <- takeMVar bc
    let cache' = BC.freeBuffer cache buf
    putMVar bc cache'
