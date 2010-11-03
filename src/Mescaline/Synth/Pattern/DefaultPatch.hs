{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Mescaline.Synth.Pattern.DefaultPatch (
    defaultPatch
) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad (join)
import qualified Control.Monad.Random as R
import           Data.Accessor
import           Data.Maybe (isJust, listToMaybe, maybeToList)
import           Data.Signal.SF
import           Data.Zip (zip, zipWith)
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
import qualified Data.List as L
import           Prelude hiding (zip, zipWith)

import Debug.Trace

featureSpace :: Pattern a FeatureSpace.FeatureSpace
featureSpace = askA (Environment.featureSpace)

sequencer :: Pattern a Sequencer.Sequencer
sequencer = askA (Environment.sequencer)

units :: Pattern a [FeatureSpace.Unit]
units = fmap FeatureSpace.units featureSpace

regionUnits :: FeatureSpace.RegionId -> Pattern a [FeatureSpace.Unit]
regionUnits i = fmap (FeatureSpace.regionUnits i) featureSpace

unitToEvent :: Pattern FeatureSpace.Unit Event.Event
unitToEvent = arr Event.fromUnit

pmaybe :: b -> (a -> b) -> Pattern (Maybe a) b
pmaybe b f = arr (maybe b f)

-- maybeEvent :: Event.Event -> P s (Maybe FeatureSpace.Unit) -> P s Event.Event
-- maybeEvent e = pmaybe e unitToEvent

chooseFrom :: (R.RandomGen s) => SF s [a] (Maybe a)
chooseFrom = arr choose >>> runRand
    where
        choose l = do
            if null l
                then return Nothing
                else do
                    i <- R.getRandomR (0, length l - 1)
                    return $ Just $ l !! i
-- pchooseFrom = ((arr (\(s, _) -> (s, (0, 1))) >>> rrand) &&& arr id) >>> arr (\(s, (r, l)) -> (s, choose r l))
--     where
--         choose r l =
--             case l of
--                 [] -> Nothing
--                 as -> Just $ as !! round (r * fromIntegral (length as - 1))

cursorValue :: Int -> Pattern () (Maybe Double)
-- cursorValue c = fmap (\s -> join $ fmap (flip Sequencer.lookupCursor s) (Sequencer.getCursor c s))  sequencer
cursorValue c = sequencer >>> arr (\s -> join $ fmap (flip Sequencer.lookupCursor s) (Sequencer.getCursor c s))

advanceCursor :: Pattern (Maybe Int) ()
advanceCursor = modify f
    where
        f Nothing e = ((), e)
        f (Just i) e = ((), e')
            where
                e'  = Environment.sequencer ^: g $ e
                g s = Sequencer.modifyCursor
                        (\(Sequencer.Cursor r c) ->
                            let c' = c + 1
                            in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c'))
                        i s

defaultPatch :: Patch.Patch
defaultPatch = Patch.mkDefault $ \i ->
    let tick = 0.125
        p = zipWith
                (\b e -> Just $ if b then e else rest tick)
                (fmap isJust (cursorValue i))
                (fmap (delta ^= tick) (pmaybe (rest tick) Event.fromUnit <<< chooseFrom <<< {- . fmap (\e -> traceShow e e) -} regionUnits i))
                -- (fmap (delta ^= tick) . maybeEvent (rest tick) . pchooseFrom . fmap (\e -> traceShow e e) $ prepeat [])
                -- (fmap (maybe (rest tick) id) . fmap (\e -> traceShow e e) $ pchooseFrom $ prepeat [(rest tick)])
    in pure (Just i) >>> advanceCursor >>> p
