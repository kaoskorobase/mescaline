module Mescaline.Pattern.Ppar (
    ptpar
  , ppar
) where

import           Control.Lens
import           Data.List (foldl', unfoldr)
import qualified Mescaline.Data.PriorityQueue as PQ
import           Sound.SC3.Lang.Pattern.P
import           Mescaline.Time

-- | Parallel pattern composition
ptpar :: HasDelta a => [(Time, P a)] -> P a
ptpar ps = toP $ unfoldr f x0
    where
        x0 = (0, foldl' (\pq (t, p) -> PQ.insert t p pq) PQ.empty ps)
        f (t, pq) =
            case PQ.minViewWithKey pq of
                Nothing -> Nothing
                Just ((t', p), pq') ->
                    case unP p of
                        [] ->
                            f (t, pq')
                        (a:as) ->
                            let p'   = toP as
                                pq'' = PQ.insert (t' + a ^. delta) p' pq'
                                dt   = maybe 0 (\t_next -> t_next - t') (PQ.minKey pq'')
                                a'   = a & delta .~ dt
                            in Just (a', (t', pq''))

ppar :: HasDelta a => [P a] -> P a
ppar = ptpar . zip (repeat 0)
