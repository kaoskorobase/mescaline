{-# LANGUAGE ExistentialQuantification #-}
module Data.Iteratee.Chan where

import Control.Concurrent.Chan
import Data.Iteratee
import Data.Iteratee.Base.StreamChunk

data ChanMsg a = CloseChan
               | ChanMsg a

enumChan :: forall a s el . Chan (ChanMsg (s el)) -> EnumeratorGM s el IO a
enumChan c = loop
    where
        loop i = do
            m <- readChan c
            case m of
                CloseChan -> return i
                ChanMsg s -> do   
                    igv <- runIter i (Chunk s)
                    check igv
        check (Done x _)        = return . return $ x
        check (Cont i' Nothing) = loop i'
        check (Cont _ (Just e)) = return $ throwErr e 
