module Mescaline.Synth.Pattern.Ppar (
    ppar
) where

import           Control.Arrow
import           Data.Accessor
import           Data.List (foldl')
import           Data.Signal.SF as SF
import qualified Mescaline.Data.PriorityQueue as PQ
import           Mescaline.Synth.Pattern
import           Mescaline.Time

-- | Parallel pattern composition
-- scanl :: (b -> a -> b) -> b -> SF a b

ppar :: HasDelta b => [SF (s, a) (s, Maybe b)] -> SF (s, a) (s, Maybe b)
ppar ps = SF (f x0) -- SF.scanl f (f (undefined, x0)) >>> arr fst
    where
        x0 = (0, foldl' (\pq p -> PQ.insert 0 p pq) PQ.empty ps)
        -- f :: (Duration,
        --       PQ.PQueue Duration (SF (s, a) (s, Maybe b)))
        --   -> (s, a)
        --   -> ((s, Maybe b), SF (s, a) (s, Maybe n))
        f (t, pq) (s, a) =
            case PQ.minViewWithKey pq of
                Nothing -> ((s, Nothing), arr (second (const Nothing)))
                Just ((t', p), pq') ->
                    case runSF p (s, a) of
                        ((s', Nothing), _) ->
                            f (t, pq') (s', a)
                        ((s', Just b), p') ->
                            let pq'' = PQ.insert (t' + b ^. delta) p' pq'
                                dt   = maybe 0 (\t_next -> t_next - t') (PQ.minKey pq'')
                                b'   = delta ^= dt $ b
                            in ((s', Just b'), SF (f (t', pq'')))
