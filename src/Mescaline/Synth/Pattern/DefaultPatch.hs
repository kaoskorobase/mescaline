{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Mescaline.Synth.Pattern.DefaultPatch (
    defaultPatch
) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad (join)
import qualified Control.Monad.Random as R
import           Data.Accessor
-- import qualified Data.Accessor.Monad.MTL.State as Accessor
import           Data.Maybe (isJust, listToMaybe, maybeToList)
import           Data.Signal.SF
import qualified Data.Zip as Zip
import           Mescaline (Duration, Time)
import           Mescaline.Synth.Pattern (Pattern, askA)
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (delta, duration, rest)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Patch as Patch
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import           Mescaline.Synth.Pattern
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Unit as FeatureSpace
import           Data.List as L

import Debug.Trace

featureSpace :: Pattern a FeatureSpace.FeatureSpace
featureSpace = askA (Environment.featureSpace)

sequencer :: Pattern a Sequencer.Sequencer
sequencer = askA (Environment.sequencer)

units :: Pattern a [FeatureSpace.Unit]
units = fmap (second FeatureSpace.units) featureSpace

regionUnits :: FeatureSpace.RegionId -> Pattern a [FeatureSpace.Unit]
regionUnits i = fmap (second $ FeatureSpace.regionUnits i) featureSpace

unitToEvent :: Pattern FeatureSpace.Unit Event.Event
unitToEvent = arrP Event.fromUnit

pmaybe :: b -> (a -> b) -> Pattern (Maybe a) b
pmaybe b f = arr (second (maybe b f))

-- maybeEvent :: Event.Event -> P s (Maybe FeatureSpace.Unit) -> P s Event.Event
-- maybeEvent e = pmaybe e unitToEvent

pchooseFrom :: (R.RandomGen s) => SF (s, [a]) (s, Maybe a)
pchooseFrom = arr (second choose) >>> runRand
    where
        choose l = do
            if null l
                then return Nothing
                else do
                    i <- R.getRandomR (0, length l - 1)
                    return $ Just $ l !! i
    
cursorValue :: Int -> Pattern () (Maybe Double)
-- cursorValue c = fmap (\s -> join $ fmap (flip Sequencer.lookupCursor s) (Sequencer.getCursor c s))  sequencer
cursorValue c = sequencer >>> arrP (\s -> join $ fmap (flip Sequencer.lookupCursor s) (Sequencer.getCursor c s))

-- advanceCursor :: Int -> Pattern ()
-- advanceCursor i = Accessor.modify Environment.sequencer (advance i)
--     where
--         advance i s = Sequencer.modifyCursor f i s
--             where
--                 f (Sequencer.Cursor r c) =
--                     let c' = c + 1
--                     in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c')

advanceCursor :: Pattern (Maybe Int) ()
-- advanceCursor tr = pcontinue tr $ \x tr' ->
--     case x of
--         Nothing -> pcons () (advanceCursor tr')
--         Just i  -> prp $ \e -> (pcons () (advanceCursor tr'), Environment.sequencer ^: advance i $ e)
--     where
--         advance i s = Sequencer.modifyCursor f i s
--             where
--                 f (Sequencer.Cursor r c) =
--                     let c' = c + 1
--                     in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c')
advanceCursor = SF f
    where
        f (e, Nothing) = ((e, ()), SF f)
        f (e, Just i)  = ((e', ()), SF f)
            where
                e'  = Environment.sequencer ^: g $ e
                g s = Sequencer.modifyCursor
                        (\(Sequencer.Cursor r c) ->
                            let c' = c + 1
                            in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c'))
                        i s

pzip :: Pattern a b -> Pattern a c -> Pattern a (b, c)
pzip sfb sfc = SF f
    where
        f (s, a) =
            let ((s', b), sfa') = runSF sfb (s, a)
                ((s'', c), sfb') = runSF sfc (s', a)
            in ((s'', (b, c)), SF f)

pzipWith :: (b -> c -> d) -> Pattern a b -> Pattern a c -> Pattern a d
pzipWith f sfb sfc = pzip sfb sfc >>> arrP (uncurry f)

defaultPatch1 :: Patch.Patch
defaultPatch1 = Patch.fromPattern $ \i ->
    -- fmap unit2event $ join $ (return . head) `fmap` units
    -- ppar [
        -- fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 3
    --   , fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 2
    -- ]
    -- regionUnits 3 `pzip` prrand
        let p = pzipWith
                    (\b e -> Just $ if b then e else rest tick)
                    (fmap (second isJust) (cursorValue i))
                    (fmap (second (delta ^= tick)) (pmaybe (rest tick) Event.fromUnit <<< pchooseFrom <<< {- . fmap (\e -> traceShow e e) -} regionUnits i))
                    -- (fmap (delta ^= tick) . maybeEvent (rest tick) . pchooseFrom . fmap (\e -> traceShow e e) $ prepeat [])
                    -- (fmap (maybe (rest tick) id) . fmap (\e -> traceShow e e) $ pchooseFrom $ prepeat [(rest tick)])
        in arr (second (const (Just i))) >>> advanceCursor >>> p
    where
        tick = 0.125

defaultPatch2 :: Patch.Patch
-- defaultPatch2 = Patch.fromPattern $ \i ->
--         let p = prepeat $ rest tick
--         in advanceCursor (prepeat (Just i)) *> p
--     where
--         tick = 0.125
--         n = length FeatureSpace.defaultRegions
defaultPatch2 = undefined

defaultPatch :: Patch.Patch
defaultPatch = defaultPatch1
