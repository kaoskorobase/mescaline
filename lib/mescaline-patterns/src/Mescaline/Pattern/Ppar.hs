module Mescaline.Pattern.Ppar (
    ptpar
  , ppar
  , takeBeats
) where

import           Control.Lens
import qualified Data.IntPSQ as PQ
import           Data.List (foldl', unfoldr)
import           Sound.SC3.Lang.Pattern.P
import           Mescaline.Time (Beats, HasDelta(..))
import           Mescaline.Pattern.Event (Event)

minPrio :: Ord p => PQ.IntPSQ p a -> Maybe p
minPrio pq = do
  (_, p, _) <- PQ.findMin pq
  return p

-- | Parallel pattern composition
ptpar :: HasDelta a => [(Double, P a)] -> P a
ptpar ps = toP $ unfoldr f x0
  where
    x0 = (0, foldl' (\pq (i, (t, p)) -> PQ.insert i t (unP p) pq)
                    PQ.empty
                    (zip [0..] ps))
    f (t, pq) =
      case PQ.minView pq of
        Nothing -> Nothing
        Just (i, t', p, pq') ->
          case p of
            [] -> f (t, pq')
            (a:p') ->
              let pq'' = PQ.insert i (t' + a ^. delta) p' pq'
                  dt   = maybe 0 (\t_next -> t_next - t') (minPrio pq'')
                  a'   = a & delta .~ dt
              in Just (a', (t', pq''))

ppar :: HasDelta a => [P a] -> P a
ppar = ptpar . zip (repeat 0)

-- |
-- >>> :set -XOverloadedStrings -XOverloadedLists
-- >>> unP $ takeBeats 1.5 ["bd", "sd"]
-- []
takeBeats :: Beats -> P Event -> P Event
takeBeats b = toP . f 0 . unP
  where
    f t _ | t >= b = []
    f _ [] = []
    f t (a:as) =
      let t' = t + realToFrac (a ^. delta)
          a' = if t' >= b
               then a & delta .~ realToFrac (t' - b)
               else a
      in a' : f t' as
