module Mescaline.Data.ListReader (
    ListReader,
    head, take,
    runListReader
) where

import Control.Monad.Error              (ErrorT)
import qualified Control.Monad.Error    as Error
import Control.Monad.State              (State)
import qualified Control.Monad.State    as State
import Control.Monad.Trans              (lift)
import 			 Prelude hiding (head, take)

type ListReader a b = ErrorT String (State [a]) b

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

runListReader :: ListReader a b -> [a] -> Either String b
runListReader a l = State.evalState (Error.runErrorT a) l
