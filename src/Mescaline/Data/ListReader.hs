{-# LANGUAGE FlexibleContexts #-}
module Mescaline.Data.ListReader (
    ListReader
  , runListReader
  , execListReader
  , head
  , take
  , tail
  , drop
  , ListWriter
  , runListWriter
  , execListWriter
  , put
) where

import           Control.Arrow (second)
import           Control.Monad
import           Control.Monad.Error (ErrorT, MonadError, throwError)
import qualified Control.Monad.Error as Error
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Trans (lift)
import qualified Data.Foldable as Seq
import qualified Data.Sequence as Seq
import 			 Prelude hiding (drop, head, tail, take)

type ListReader a b = ErrorT String (State [a]) b

runListReader :: MonadError String m => ListReader a b -> [a] -> m (b, [a])
runListReader a l = do
    case State.runState (Error.runErrorT a) l of
        (Left e, _)   -> throwError e
        (Right b, as) -> return (b, as)

execListReader :: MonadError String m => ListReader a b -> [a] -> m b
execListReader r = liftM fst . runListReader r

take :: Int -> ListReader a [a]
take n = do
    l <- lift (State.get)
    let (r, l') = splitAt n l
    lift (State.put l')
    return r

head :: ListReader a a
head = do
	l <- lift (State.get)
	let (r:l') = l
	-- TODO: error handling
	lift (State.put l')
	return r

tail :: ListReader a ()
tail = drop 1

drop :: Int -> ListReader a ()
drop n = take n >> return ()

type ListWriter a b = Writer (Seq.Seq a) b

runListWriter :: ListWriter a b -> (b, [a])
runListWriter = second Seq.toList . Writer.runWriter

execListWriter :: ListWriter a b -> [a]
execListWriter = snd . runListWriter

put :: a -> ListWriter a ()
put = Writer.tell . Seq.singleton
