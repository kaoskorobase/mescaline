module Mescaline.Synth.Pattern.Ppar (
    ppar
) where

import           Data.Accessor
import           Data.List (foldl')
import qualified Mescaline.Data.PriorityQueue as PQ
import           Mescaline.Synth.Pattern.Base
import           Mescaline.Time

-- | Parallel pattern composition
ppar :: HasDuration a => [P s a] -> P s a
ppar ps = punfoldr f x0
    where
        x0 = (0, foldl' (\pq p -> PQ.insert 0 p pq) PQ.empty ps)
        f s (t, pq) =
            case PQ.minViewWithKey pq of
                Nothing -> (s, Nothing)
                Just ((t', p), pq') ->
                    case step s p of
                        Done s' ->
                            f s' (t, pq')
                        Result s' a p' ->
                            let pq'' = PQ.insert (t' + getVal duration a) p' pq'
                                dt   = maybe 0 (\t_next -> t_next - t') (PQ.minKey pq'')
                                a'   = setVal duration dt a
                            in (s', Just (a', (t', pq'')))
