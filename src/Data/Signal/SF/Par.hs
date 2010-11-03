module Data.Signal.SF.Par (
    par
) where

import           Control.Arrow
import           Data.Accessor
import           Data.List (foldl')
import           Data.Signal.SF as SF
import qualified Euterpea.Signal.SF as Base
import qualified Mescaline.Data.PriorityQueue as PQ
import           Mescaline.Time

-- | Parallel pattern composition
par :: HasDelta b => [SF s a (Maybe b)] -> SF s a (Maybe b)
par ps = (arr id &&& fetch) >>> liftState (Base.SF (f x0)) >>> second store >>> arr fst
    where
        x0 = (0, foldl' (\pq p -> PQ.insert 0 (elimState p) pq) PQ.empty ps)
        f (t, pq) (a, s) =
            case PQ.minViewWithKey pq of
                Nothing -> ((Nothing, s), arr (first (const Nothing)))
                Just ((t', p), pq') ->
                    case Base.runSF p (a, s) of
                        ((Nothing, s'), _) ->
                            f (t, pq') (a, s')
                        ((Just b, s'), p') ->
                            let pq'' = PQ.insert (t' + b ^. delta) p' pq'
                                dt   = maybe 0 (\t_next -> t_next - t') (PQ.minKey pq'')
                                b'   = delta ^= dt $ b
                            in ((Just b', s'), Base.SF (f (t', pq'')))
