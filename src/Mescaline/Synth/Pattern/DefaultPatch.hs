module Mescaline.Synth.Pattern.DefaultPatch (
    defaultPatch
) where

import           Control.Applicative
import           Control.Monad (join)
import qualified Control.Monad.Random as R
import           Data.Accessor
import qualified Data.Accessor.Monad.MTL.State as Accessor
import           Data.Maybe (isJust, maybeToList)
import           Mescaline (Duration, Time)
import           Mescaline.Synth.Pattern (Pattern)
import qualified Mescaline.Synth.Pattern.Environment as Environment
import           Mescaline.Synth.Pattern.Event (delta, duration, rest)
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Pattern.Patch as Patch
import qualified Mescaline.Synth.Pattern.Sequencer as Sequencer
import           Mescaline.Synth.Pattern
import qualified Mescaline.Synth.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.Synth.FeatureSpace.Unit as FeatureSpace
import           Data.List as L

featureSpace :: Pattern FeatureSpace.FeatureSpace
featureSpace = askA (Environment.featureSpace)

sequencer :: Pattern Sequencer.Sequencer
sequencer = askA (Environment.sequencer)

units :: Pattern [FeatureSpace.Unit]
units = fmap FeatureSpace.units featureSpace

regionUnits :: FeatureSpace.RegionId -> Pattern [FeatureSpace.Unit]
regionUnits i = fmap (FeatureSpace.regionUnits i) featureSpace

unitToEvent :: FeatureSpace.Unit -> Event.Event
unitToEvent = Event.fromUnit

unitEvent :: P s FeatureSpace.Unit -> P s Event.Event
unitEvent = fmap unitToEvent

pmaybe :: b -> (a -> b) -> P s (Maybe a) -> P s b
pmaybe b f = fmap (maybe b f)

maybeEvent :: Event.Event -> P s (Maybe FeatureSpace.Unit) -> P s Event.Event
maybeEvent e = pmaybe e unitToEvent

pchooseFrom :: (R.RandomGen s) =>
    P s [a] -> P s (Maybe a)
pchooseFrom = runRand . fmap choose
    where
        choose [] = return Nothing
        choose as = do
            i <- R.getRandomR (0, length as - 1)
            return $ Just $ as !! i

cursorValue :: Int -> Pattern (Maybe Double)
cursorValue c = fmap (\s -> join $ fmap (flip Sequencer.lookupCursor s) (Sequencer.getCursor c s))  sequencer

-- advanceCursor :: Int -> Pattern ()
-- advanceCursor i = Accessor.modify Environment.sequencer (advance i)
--     where
--         advance i s = Sequencer.modifyCursor f i s
--             where
--                 f (Sequencer.Cursor r c) =
--                     let c' = c + 1
--                     in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c')

advanceCursor :: Pattern (Maybe Int) -> Pattern ()
advanceCursor tr = pcontinue tr $ \x tr' ->
    case x of
        Nothing -> pcons () (advanceCursor tr')
        Just i  -> prp $ \e -> (pcons () (advanceCursor tr'), Environment.sequencer ^: advance i $ e)
    where
        advance i s = Sequencer.modifyCursor f i s
            where
                f (Sequencer.Cursor r c) =
                    let c' = c + 1
                    in Sequencer.Cursor r (if c' >= (Sequencer.cols s) then 0 else c')

defaultPatch :: Patch.Patch
defaultPatch = Patch.fromPattern $
    -- fmap unit2event $ join $ (return . head) `fmap` units
    -- ppar [
        -- fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 3
    --   , fmap unit2event $ join $ (flip pseq 1 . map return . L.take 1) `fmap` regionUnits 2
    -- ]
    -- regionUnits 3 `pzip` prrand
    flip map [0..n-1] $ \i ->
        let p = pzipWith
                    (\b e -> if b then e else (rest tick))
                    (isJust `fmap` cursorValue i)
                    (fmap (delta ^= tick) . maybeEvent (rest tick) . pchooseFrom $ regionUnits i)
        in advanceCursor (prepeat (Just i)) *> p
    where
        tick = 0.125
        n = length FeatureSpace.defaultRegions
