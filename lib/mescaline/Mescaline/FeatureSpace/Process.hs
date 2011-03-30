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
import qualified Mescaline.Database as DB
import qualified Mescaline.FeatureSpace.Model as Model
import qualified Mescaline.FeatureSpace.Unit as Unit

data Input =
    LoadDatabase    FilePath String
  | UpdateRegion    Model.Region
  -- | ActivateRegion  Time Model.RegionId
  | GetModel        (Query Model.FeatureSpace)

data Output =
    DatabaseLoaded  [Unit.Unit]
  | RegionChanged   Model.Region

type Handle = Process.Handle Input Output

new :: IO Handle
new = do
    spawn $ loop Model.empty
    where
        loop !f = do
            msg <- recv
            f' <- case msg of
                    LoadDatabase path pattern -> do
                        units <- io $ DB.withDatabase path
                                    $ Unit.getUnits pattern [
                                        "es.globero.mescaline.spectral" ]
                          -- , "com.meapsoft.AvgChunkPower"
                          -- , "com.meapsoft.AvgFreqSimple" ]
                        let f' = Model.setUnits f (seqList rseq units `seq` units)
                        notify $ DatabaseLoaded (Model.units f')
                        return $ f'
                    UpdateRegion r -> do
                        notify $ RegionChanged r
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
