module Mescaline.Synth.Pattern.DefaultPatch (
    defaultPatch
) where

import           Control.Applicative
import           Control.Monad (join)
import qualified Control.Monad.Random as R
import           Data.Accessor
import           Mescaline (Duration, Time)
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (rest)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Patch as Patch
import           Mescaline.Synth.Pattern
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import           Data.List as L

featureSpace :: Patch.Pattern FeatureSpace.FeatureSpace
featureSpace = asks Environment.featureSpace

units :: Patch.Pattern [FeatureSpace.Unit]
units = fmap FeatureSpace.units featureSpace

regionUnits :: FeatureSpace.RegionId -> Patch.Pattern [FeatureSpace.Unit]
regionUnits i = fmap (FeatureSpace.regionUnits i) featureSpace

unitToEvent :: FeatureSpace.Unit -> Event.Event
unitToEvent = Event.fromUnit . FeatureSpace.unit

unitEvent :: P s FeatureSpace.Unit -> P s Event.Event
unitEvent = fmap unitToEvent

maybeEvent :: Event.Event -> P s (Maybe FeatureSpace.Unit) -> P s Event.Event
maybeEvent e = fmap (maybe e unitToEvent)

pchooseFrom :: (R.RandomGen s) =>
    P s [a] -> P s (Maybe a)
pchooseFrom = runRand . fmap choose
    where
        choose [] = return Nothing
        choose as = do
            i <- R.getRandomR (0, length as - 1)
            return $ Just $ as !! i

defaultPatch :: Patch.Patch
defaultPatch = Patch.fromPattern $
    -- fmap unit2event $ join $ (return . head) `fmap` units
    -- ppar [
        -- fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 3
    --   , fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 2
    -- ]
    -- regionUnits 3 `pzip` prrand
    maybeEvent (rest 0.125) $ pchooseFrom $ regionUnits 3
