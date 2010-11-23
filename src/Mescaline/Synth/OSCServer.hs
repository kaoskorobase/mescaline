module Mescaline.Synth.OSCServer (
    new
) where

import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Data.Accessor
import qualified Mescaline.Synth.FeatureSpace.Process as FSpace
import qualified Mescaline.Synth.FeatureSpace.Model as FSpace
import qualified Mescaline.Synth.Pattern.Event as Event
import qualified Mescaline.Synth.Sampler.Process as Synth
import qualified Sound.OpenSoundControl as OSC
import qualified System.Random as Random

data Input = Input
data Output = Output
type Handle = Process.Handle Input Output

setEnv = setVal Event.attackTime 0.001 . setVal Event.releaseTime 0.001

activateRegion i f g = (u, g')
    where
        us      = FSpace.regionUnits i f
        (j, g') = Random.randomR (0, length us - 1) g
        u       = if null us then Nothing else Just (us !! j)

playUnit synth = maybe (return ()) (sendTo synth . Synth.PlayUnit (-1) . setEnv . Event.defaultSynth)

loop socket synth fspaceP g = do
    osc <- io $ OSC.recv socket
    fspace <- query fspaceP FSpace.GetModel
    g' <- case osc of
            OSC.Message "/closest" [OSC.Float x, OSC.Float y] -> do
                -- io $ putStrLn $ "/closest "  ++ show x ++ " " ++ show y
                playUnit synth (fmap fst (FSpace.closest2D (x, y) fspace))
                return g
            OSC.Message "/region" [OSC.Int r] -> do
                -- io $ putStrLn $ "/region "  ++ show r
                let (unit, g') = activateRegion r fspace g
                playUnit synth unit
                return g'
            _ -> return g
    loop socket synth fspaceP g'

new :: Int -> Synth.Handle -> FSpace.Handle -> IO Handle
new port synth fspace = do
    t <- OSC.udpServer "127.0.0.1" port
    g <- Random.getStdGen
    spawn $ loop t synth fspace g
