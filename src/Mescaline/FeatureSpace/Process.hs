{-# LANGUAGE BangPatterns #-}
module Mescaline.FeatureSpace.Process (
    Handle
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Seq
import qualified Mescaline.Application.Logger as Log
import qualified Mescaline.Database.Entity as DB
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.Unit as DBUnit
import qualified Mescaline.Synth.Database.Model as DB
import qualified Mescaline.FeatureSpace.Model as Model
import qualified Mescaline.FeatureSpace.Unit as Unit
import qualified System.Random as Random

data Input =
    LoadDatabase    FilePath String
  | UpdateRegion    Model.Region
  -- | ActivateRegion  Time Model.RegionId
  | GetModel        (Query Model.FeatureSpace)

data Output =
    DatabaseLoaded  [Unit.Unit]
  | RegionChanged   Model.Region

type Handle = Process.Handle Input Output

getUnits :: FilePath
         -> String
         -> [Feature.Descriptor]
         -> IO [Unit.Unit]
getUnits dbFile pattern features = do
    (sfs, us) <- DB.query dbFile
                    pattern
                    (map Feature.name features)
    let us' = map (\(i, u, fs) -> Unit.cons sfs i u fs) us
    seqList rseq us' `seq` return us'

new :: IO Handle
new = do
    rgen <- Random.getStdGen
    spawn $ loop (Model.fromList rgen [])
    where
        loop !f = do
            msg <- recv
            f' <- case msg of
                    LoadDatabase path pattern -> do
                        units <- io $ getUnits path pattern [
                            Feature.consDescriptor "es.globero.mescaline.spectral" 2 ]
                          -- , Feature.consDescriptor "com.meapsoft.AvgChunkPower" 1
                          -- , Feature.consDescriptor "com.meapsoft.AvgFreqSimple" 1 ]
                        let f' = Model.setUnits f units
                        notify $ DatabaseLoaded (Model.units f')
                        return $ f'
                    UpdateRegion r -> do
                        notify $ RegionChanged r
                        io $ Log.debugM "FeatureSpace" $ "UpdateRegion: " ++ show r
                        return $ Model.updateRegion r f
                    -- ActivateRegion t i -> do
                    --     let (u, f') = Model.activateRegion i f
                    --     case u of
                    --         Nothing -> return ()
                    --         Just u  -> notify $ UnitActivated t u
                    --     return f'
                    GetModel q -> do
                        answer q f
                        return f
            loop f'
