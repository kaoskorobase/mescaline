module Mescaline.Data.ListReader (
    ListReader,
    get,
    runListReader
) where

import Control.Monad.Error              (ErrorT)
import qualified Control.Monad.Error    as Error
import Control.Monad.State              (State)
import qualified Control.Monad.State    as State
import Control.Monad.Trans              (lift)

type ListReader a b = ErrorT String (State [a]) b

get :: Int -> ListReader a [a]
get n = do
    l <- lift (State.get)
    let (r, l') = splitAt n l
    lift (State.put l')
    return r

runListReader :: ListReader a b -> [a] -> Either String b
runListReader a l = State.evalState (Error.runErrorT a) l
