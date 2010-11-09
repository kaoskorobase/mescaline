{-# LANGUAGE DeriveDataTypeable #-}
module Mescaline.Synth.Pattern.Library (
    -- *Generators
    closest
  , region
    -- *Filters
  , filterE
  , step
    -- *Coordinates
  , coord
  , polar
  , constrain
    -- *Regions
  , center
  , radius
  -- *Randomness
  , prrand_
  , pexprand_
) where

import           Control.Applicative
import           Control.Exception
import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Data.Accessor
import qualified Data.Accessor.Monad.MTL.State as Accessor
import qualified Data.Complex as C
import           Data.Maybe (isJust, listToMaybe, maybeToList)
import           Data.Typeable (Typeable)
import           Mescaline (Duration, Time)
import           Mescaline.Synth.Pattern (Pattern)
import qualified Mescaline.Synth.Pattern.AST as AST
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (Event, delta, duration, rest)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import           Mescaline.Synth.Pattern hiding (step)
import qualified Mescaline.Synth.Pattern as Pattern
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Unit as FeatureSpace
import           Data.List as L

import Debug.Trace

pmaybe :: b -> (a -> b) -> P s (Maybe a) -> P s b
pmaybe b f = fmap (maybe b f)

punzip :: P s (a, b) -> (P s a, P s b)
punzip p = (fmap fst p, fmap snd p)

ptranspose :: [P s a] -> P s [a]
ptranspose = punfoldr $ \s ps ->
                let (s', as, ps') = foldl (\(s, as, ps) p  ->
                                        case Pattern.step s p of
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

prrand_ :: (R.RandomGen s, R.Random a) => 
    P s a -> P s a -> P s a
prrand_ l r = M.join (pzipWith prrand l r)

pexprand_ :: (R.RandomGen s, Floating a, R.Random a) => 
    P s a -> P s a -> P s a
pexprand_ l r = M.join (pzipWith prrandexp l r)

data Error = RuntimeError String deriving (Show, Typeable)

instance Exception Error

forceMaybe :: String -> Maybe a -> a
forceMaybe e Nothing  = throw (RuntimeError e)
forceMaybe _ (Just a) = a

getCursor :: Int -> Pattern Event.Cursor
getCursor i = fmap (\s -> case Sequencer.getCursor (max 0 (min (length (Sequencer.cursors s) - 1) i)) s of
                                Nothing -> error "getCursor: This shouldn't happen"
                                Just c  -> case Sequencer.lookupCursor c s of
                                            Nothing -> Event.Cursor i (Sequencer.row c, Sequencer.column c) 0
                                            Just v  -> Event.Cursor i (Sequencer.row c, Sequencer.column c) v)
                       (askA Environment.sequencer)

getRegion :: Pattern Double -> Pattern FeatureSpace.Region
getRegion = pzipWith (\fs i -> let r = truncate i
                               in forceMaybe ("Invalid region " ++ show r) (FeatureSpace.lookupRegion r fs))
                     (askA Environment.featureSpace)

center :: Pattern Double -> Pattern (Double, Double)
center = fmap FeatureSpace.center2D . getRegion

radius :: Pattern Double -> Pattern Double
radius = fmap FeatureSpace.radius . getRegion

coord :: Pattern Double -> Pattern Double -> Pattern (Double, Double)
coord = pzip

polar :: Pattern (Double, Double) -> Pattern Double -> Pattern Double -> Pattern (Double, Double)
polar = pzipWith3 f
    where f (x, y) mag phi = let c = C.mkPolar mag phi in (x + C.realPart c, y + C.imagPart c)

-- | 
constrain :: Pattern (Double, Double) -> Pattern Double -> Pattern (Double, Double) -> Pattern (Double, Double)
constrain = pzipWith3 f
    where
        f c r p | C.realPart (abs (uncurry (C.:+) c - uncurry (C.:+) p)) <= r = p
                | otherwise = (-1, -1)

closest :: Int -> Pattern Double -> Pattern (Double, Double) -> Pattern Double -> Pattern Event
closest i = pzipWith4 f (pzip (askA Environment.featureSpace) (getCursor i))
    where
        f (fs, c) dt (x, y) r =
            let rest = Event.rest c dt
            in case FeatureSpace.closest2D (x, y) fs of
                Nothing -> rest
                Just (u, d) -> if d <= r
                               then Event.synthEvent c u
                               else rest

filterE :: Pattern Bool -> Pattern Event -> Pattern Event
filterE = pzipWith f
    where
        f True e = e
        f False e = Event.synth ^= Nothing $ e

regionUnits :: Pattern Double -> Pattern [FeatureSpace.Unit]
regionUnits i = pzipWith (FeatureSpace.regionUnits . truncate) i (askA (Environment.featureSpace))

region :: AST.RegionIterator -> Int -> Pattern Double -> Pattern Double -> Pattern Event
region AST.Uniform i dt r = pzipWith3 f (getCursor i) dt (pchooseFrom (regionUnits r))
    where
        f c dt = maybe (Event.rest c dt) (Event.synthEvent c)
        -- mke c Nothing = Event.rest c
        -- mke
        -- Event.synth ^: (fmap Event.defaultSynth u <*) $ e

data CursorBehavior = WrapCursor | MirrorCursor

step :: Pattern Double -> Pattern Double -> Pattern Event -> Pattern Event
step rowInc colInc = pzipWith3 f (pzip rowInc colInc) (askA Environment.sequencer)
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
