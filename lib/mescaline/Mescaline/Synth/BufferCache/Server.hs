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

import           Control.Concurrent.MVar
import           Control.Monad (forM, forM_)
import qualified Data.Set as Set
import           Mescaline.Synth.BufferCache (Alloc, Buffer, uid, numChannels, numFrames, allocBytes, allocFrames)
import qualified Mescaline.Synth.BufferCache as BC
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3 (b_alloc, b_free)
import           Sound.SC3.Server.Command.Completion (b_alloc')
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as S

type BufferCache = MVar BC.BufferCache

new :: [Alloc] -> Server BufferCache
new allocs = do
    buffers <- forM allocs $ \a -> do
        bid <- S.alloc S.bufferId
        let buf = BC.fromAlloc bid a
        S.sync $ b_alloc (fromIntegral bid)
                         (BC.numFrames buf)
                         (BC.numChannels buf)
        return buf
    S.liftIO $ newMVar $ BC.fromList buffers

release :: BufferCache -> Server ()
release bc = do
    cache <- S.liftIO $ takeMVar bc
    forM_ (Set.elems (BC.usedBuffers cache)
        ++ Set.elems (BC.freeBuffers cache)) $ \b -> do
            S.free S.bufferId (BC.uid b)
            S.sync $ b_free (fromIntegral (BC.uid b))
    S.liftIO $ putMVar bc BC.empty

allocBuffer :: (Buffer -> Maybe OSC) -> BufferCache -> Alloc -> Server Buffer
allocBuffer completion bc alloc = do
    cache <- S.liftIO $ takeMVar bc
    (cache', buf) <-
        case BC.allocBuffer cache alloc of
            Nothing -> do
                -- Allocate buffer id
                bid <- S.alloc S.bufferId
                let buf = BC.fromAlloc bid alloc
                    msg = maybe b_alloc (($) b_alloc') (completion buf)
                -- Allocate buffer
                S.sync $ msg (fromIntegral bid)
                             (BC.numFrames buf)
                             (BC.numChannels buf)
                -- Insert into used
                return (BC.insertBuffer cache buf, buf)
            Just (cache', buf) -> do
                maybe (return ()) S.async (completion buf)
                return (cache', buf)
    cache' `seq` S.liftIO $ putMVar bc cache'
    return buf

freeBuffer :: BufferCache -> Buffer -> Server ()
freeBuffer bc buf = do
    cache <- S.liftIO $ takeMVar bc
    let cache' = BC.freeBuffer cache buf
    cache' `seq` S.liftIO $ putMVar bc cache'
