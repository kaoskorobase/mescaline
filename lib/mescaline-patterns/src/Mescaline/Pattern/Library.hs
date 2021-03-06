{-# LANGUAGE DeriveDataTypeable #-}
module Mescaline.Pattern.Library (
    -- *Generators
    closest
  , region
    -- *Filters
  , takeDur
  , filterE
  , step
    -- *Coordinates
  , coord
  , polar
    -- *Regions
  , center
  , radius
  -- *Randomness
  , prrand_
  , pexprand_
  , pgaussian_
  , pbrown_
) where

import           Control.Applicative
import           Control.Exception
import qualified Control.Monad as M
import qualified Control.Monad.Random as R
import           Data.Accessor
import qualified Data.Accessor.Monad.MTL.State as Accessor
import qualified Data.Complex as C
import qualified Data.Monoid as M
-- import qualified Data.Packed.Random as R
import           Data.Typeable (Typeable)
import qualified Mescaline.Pattern.Environment as Environment
import           Mescaline.Pattern.Event (Event)
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Pattern.Sequencer as Sequencer
import           Mescaline.Pattern hiding (step)
import qualified Mescaline.Pattern as Pattern
import qualified Mescaline.FeatureSpace.Model as FeatureSpace

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

-- pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P s y -> P s a
-- pscanl :: (a -> y -> a) -> a -> P s y -> P s a
-- pcontinue :: P s x -> (x -> P s x -> P s a) -> P s a

pbrown_ :: (R.RandomGen s, R.Random a, RealFrac a) => 
    (a -> a -> a -> a) -> P s a -> P s a -> P s a -> P s a -> P s a
pbrown_ f sl sr l r = pcontinue (prrand_ l r) $ \x _ -> pscanl next x (pzip3 (prrand_ sl sr) l r)
    where next x (dx, xl, xr) = f xl xr (x + dx)

pgaussian_ :: (R.RandomGen s, R.Random a, Floating a) => 
    P s a -> P s a -> P s a
pgaussian_ mean var = mean + fmap sqrt var * pzipWith f r r
    where
        r = prrand_ (pure (-1)) (pure 1)
        f u v = let s = u*u + v*v in u * sqrt ((-2 * log s) / s)

data Error = RuntimeError String deriving (Show, Typeable)

instance Exception Error

forceMaybe :: String -> Maybe a -> a
forceMaybe e Nothing  = throw (RuntimeError e)
forceMaybe _ (Just a) = a

-- getCursor :: Int -> Pattern Event.Cursor
-- getCursor i = fmap (\s -> case Sequencer.getCursor (max 0 (min (length (Sequencer.cursors s) - 1) i)) s of
--                                 Nothing -> error "getCursor: This shouldn't happen"
--                                 Just c  -> case Sequencer.lookupCursor c s of
--                                             Nothing -> Event.Cursor i (Sequencer.row c, Sequencer.column c) 0
--                                             Just v  -> Event.Cursor i (Sequencer.row c, Sequencer.column c) v)
--                        (askA Environment.sequencer)

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

closest :: Event.Cursor -> Pattern Double -> Pattern Double -> Pattern (Double, Double) -> Pattern Event
closest c = pzipWith4 f (askA Environment.featureSpace)
    where
        f fs dt r (x, y) =
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

-- pscan :: (x -> y -> (x, a)) -> Maybe (x -> a) -> x -> P s y -> P s a
-- punfoldr :: (s -> x -> (s, Maybe (a, x))) -> x -> P s a

takeDur :: Double -> Pattern Event -> Pattern Event
takeDur dur pattern = punfoldr f (0, pattern)
    where
        f s (t, p) =
            case Pattern.step s p of
                Done s' -> (s', Nothing)
                Result s' e p' ->
                    let t' = t + e ^. Event.delta
                    in if t' >= dur
                        then (s', Just (Event.delta ^= dur - t $ e, (t', M.mempty)))
                        else (s', Just (e, (t', p')))

regionUnits :: Pattern Int -> Pattern [FeatureSpace.Unit]
regionUnits i = pzipWith (FeatureSpace.regionUnits) i (askA (Environment.featureSpace))

region :: Event.Cursor -> Pattern Double -> Pattern Double -> Pattern Event
region c dt it = pzipWith3 f dt it (regionUnits (pure c))
    where
        f dt _ [] = Event.rest c dt
        f _ i us  = let n = length us
                        j = max 0 $ min (n-1) $ truncate (i * fromIntegral n)
                    in Event.synthEvent c (us !! j)
        -- Event.synth ^: (fmap Event.defaultSynth u <*) $ e

data CursorBehavior = WrapCursor | MirrorCursor

step :: Pattern Double -> Pattern Double -> Pattern Event -> Pattern Event
step rowInc colInc e = M.join (pzipWith3 f (fmap (getVal Event.cursor) e) rowInc colInc) *> e
    where
        f i ri ci =
            Accessor.modify
                Environment.sequencer $
                    \s -> Sequencer.modifyCursor (
                        \cursor ->
                            let (r, c)   = Sequencer.position cursor
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
                            in Sequencer.Cursor r'' c'') i s
                           -- Event.cursor ^= cursor { Event.cursorPosition = (r'',c'') } $ e
                           -- (Sequencer.setCursor (e ^. Event.cursor) (Sequencer.Cursor r'' c''))
                           --  *> return e

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
