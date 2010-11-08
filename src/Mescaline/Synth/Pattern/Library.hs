module Mescaline.Synth.Pattern.Library (
    -- *Generators
    metro
  , sequencer
    -- *Filters
  , region
  , stepCursor
) where

import           Control.Applicative
import           Control.Monad (join)
import qualified Control.Monad.Random as R
import           Data.Accessor
import qualified Data.Accessor.Monad.MTL.State as Accessor
import           Data.Maybe (isJust, listToMaybe, maybeToList)
import           Mescaline (Duration, Time)
import           Mescaline.Synth.Pattern (Pattern)
import qualified Mescaline.Synth.Pattern.AST as AST
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (Event, delta, duration, rest)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import           Mescaline.Synth.Pattern
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Unit as FeatureSpace
import           Data.List as L

import Debug.Trace

pmaybe :: b -> (a -> b) -> P s (Maybe a) -> P s b
pmaybe b f = fmap (maybe b f)

ptranspose :: [P s a] -> P s [a]
ptranspose = punfoldr $ \s ps ->
                let (s', as, ps') = foldl (\(s, as, ps) p  ->
                                        case step s p of
                                            Done s'        -> (s', as, ps)
                                            Result s' a p' -> (s', a:as, p':ps))
                                          (s, [], [])
                                          ps
                in if length ps == length ps'
                   then (s', Just (reverse as, reverse ps'))
                   else (s', Nothing)

pchooseFrom :: (R.RandomGen s) =>
    P s [a] -> P s (Maybe a)
pchooseFrom = runRand . fmap choose
    where
        choose [] = return Nothing
        choose as = do
            i <- R.getRandomR (0, length as - 1)
            return $ Just $ as !! i

getCursor :: Int -> Pattern Event.Cursor
getCursor i = fmap (\s -> case Sequencer.getCursor (max 0 (min (length (Sequencer.cursors s) - 1) i)) s of
                                Nothing -> error "getCursor: This shouldn't happen"
                                Just c  -> case Sequencer.lookupCursor c s of
                                            Nothing -> Event.Cursor i (Sequencer.row c, Sequencer.column c) 0
                                            Just v  -> Event.Cursor i (Sequencer.row c, Sequencer.column c) v)
                       (askA (Environment.sequencer))

-- | Generate events on a time grid.
metro :: Int -> Pattern Double -> Pattern Event
metro i = pzipWith Event.rest (getCursor i)

metroThresh :: Int -> Pattern Double -> Pattern Double -> Pattern Event
metroThresh i thresh = pzipWith f thresh . metro i
    where
        f x e = 
            if Event.cursorValue (e ^. Event.cursor) > x
            then e
            else Event.synth ^= Nothing $ e

-- | Generate events on a time grid depending on the values in the matrix sequencer.
sequencer :: Int -> Pattern Double -> Pattern Event
sequencer i = stepCursor (pure 0) (pure 1) . metroThresh i (pure 0)

-- | Generate events on a time grid depending on the values in the matrix sequencer.
gridSequencer :: (Double, Double) -> (Double, Double, Double, Double) -> (Double, Double, Double, Double) -> Int -> Pattern Event
gridSequencer = undefined

regionUnits :: Pattern Double -> Pattern [FeatureSpace.Unit]
regionUnits i = pzipWith (FeatureSpace.regionUnits . truncate) i (askA (Environment.featureSpace))

region :: AST.RegionIterator -> Pattern Double -> Pattern Event -> Pattern Event
region AST.Uniform i = pzipWith f (pchooseFrom (regionUnits i))
    where f u e = Event.synth ^: (fmap Event.defaultSynth u <*) $ e

data CursorBehavior = WrapCursor | MirrorCursor

stepCursor :: Pattern Double -> Pattern Double -> Pattern Event -> Pattern Event
stepCursor rowInc colInc = pzipWith3 f (pzip rowInc colInc) (askA Environment.sequencer)
    where
        f (ri,ci) s e =
            let cursor   = e ^. Event.cursor
                (r, c)   = Event.cursorPosition cursor
                (r', c') = (r + signum (round ri), c + signum (round ci))
                r''      = if r' < 0
                            then Sequencer.rows s + r'
                            else if r' >= Sequencer.rows s
                                 then r' - Sequencer.rows s
                                 else r'
                c''      = if c' < 0
                            then Sequencer.cols s + c'
                            else if c' >= Sequencer.cols s
                                 then c' - Sequencer.cols s
                                 else c'
            in Event.cursor ^= cursor { Event.cursorPosition = (r'',c'') } $ e

-- advanceCursor :: Pattern (Maybe Int) -> Pattern ()
-- advanceCursor = join . fmap f
--     where
--         f Nothing  = return ()
--         f (Just i) = Accessor.modify Environment.sequencer (advance i)
--         advance i s = Sequencer.modifyCursor f i s
--             where
--                 f (Sequencer.Cursor r c) =
--                     let c' = c + 1
--                     in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c')
-- 
-- defaultPatch1 :: Patch.Patch
-- defaultPatch1 = Patch.fromPattern $ \i ->
--     -- fmap unit2event $ join $ (return . head) `fmap` units
--     -- ppar [
--         -- fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 3
--     --   , fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 2
--     -- ]
--     -- regionUnits 3 `pzip` prrand
--         let p = pzipWith
--                     (\b e -> if b then e else rest tick)
--                     (isJust `fmap` cursorValue i)
--                     (fmap (delta ^= tick) . pmaybe (rest tick) Event.synthEvent . pchooseFrom {- . fmap (\e -> traceShow e e) -} $ (regionUnits i))
--                     -- (fmap (delta ^= tick) . maybeEvent (rest tick) . pchooseFrom . fmap (\e -> traceShow e e) $ prepeat [])
--                     -- (fmap (maybe (rest tick) id) . fmap (\e -> traceShow e e) $ pchooseFrom $ prepeat [(rest tick)])
--         in advanceCursor (pure (Just i)) *> p
--     where
--         tick = 0.125
-- 
-- defaultPatch2 :: Patch.Patch
-- defaultPatch2 = Patch.fromPattern $ \i ->
--         let p = prepeat $ rest tick
--         in advanceCursor (prepeat (Just i)) *> p
--     where
--         tick = 0.125
--         n = length FeatureSpace.defaultRegions
-- 
-- defaultPatch :: Patch.Patch
-- defaultPatch = defaultPatch1
