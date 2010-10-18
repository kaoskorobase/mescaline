{-# LANGUAGE BangPatterns #-}
module Mescaline.Synth.FeatureSpace.Process (
    FeatureSpace
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Arrow (second)
import           Control.Concurrent.Process
import           Control.Monad.Reader
import qualified Data.Vector.Generic as V
import qualified Database.HDBC as DB
import           Mescaline (Time)
import qualified Mescaline.Database as DB
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.SqlQuery as Sql
import qualified Mescaline.Database.Unit as Unit
import qualified Mescaline.Synth.Database.Model as DB
import qualified Mescaline.Synth.FeatureSpace.Model as Model
import qualified System.Random as Random

-- type Property a = Either a (a -> m b, Listener b)

data Input =
    LoadDatabase    !FilePath !String
  | ActivateUnit    !Time !Model.Unit
  | DeactivateUnit  !Time !Unit.Unit
  | AddRegion       !Double !Double !Double
  | UpdateRegion    !Model.Region
  | ActivateRegion  !Time !Model.RegionId
  deriving (Show)

data Output =
    DatabaseLoaded  [Model.Unit]
  | UnitActivated   Time Model.Unit
  | UnitDeactivated Time Unit.Unit
  | RegionAdded     Model.Region
  | RegionChanged   Model.Region
  deriving (Show)

type FeatureSpace = Handle Input Output

getUnits :: FilePath
         -> String
         -> [Feature.Descriptor]
         -> IO [(Unit.Unit, [Feature.Feature])]
getUnits dbFile pattern features = do
    (units, _) <- DB.query dbFile Unit.Onset pattern features
    case units of
        Left e   -> putStrLn ("ERROR[DB]: " ++ e) >> return []
        Right us -> return us

new :: IO FeatureSpace
new = do
    rgen <- Random.getStdGen
    spawn $ loop (Model.fromList rgen [])
    where
        loop !f = do
            x <- recv
            f' <- case x of
                    LoadDatabase path pattern -> do
                        units <- liftIO $ fmap (map (second head)) $ getUnits path pattern [Feature.consDescriptor "es.globero.mescaline.spectral" 2]
                        let f' = Model.setFeatureSpace f units
                        notify $ DatabaseLoaded (Model.units f')
                        return $ f'
                    AddRegion x y ra -> do
                        let i = Model.nextRegionId f
                            r = Model.mkRegion i (V.fromList [x, y]) ra
                        notify $ RegionAdded r
                        return $ Model.addRegion r f
                    UpdateRegion r -> do
                        notify $ RegionChanged r
                        -- io $ putStrLn $ "UpdateRegion " ++ show r
                        return $ Model.updateRegion r f
                    ActivateRegion t i -> do
                        let (u, f') = Model.activateRegion i f
                        case u of
                            Nothing -> return ()
                            Just u  -> notify $ UnitActivated t u
                        return f'
                    ActivateUnit t u -> do
                        notify $ UnitActivated t u
                        return $ Model.activateUnit (Model.unit u) f
                        return f
                    DeactivateUnit t u -> do
                        notify $ UnitDeactivated t u
                        return $ Model.deactivateUnit u f
            loop f'
