import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBChan
import			 Control.Monad.IO.Class (MonadIO(..))
import			 Control.Monad.IO.Control (MonadControlIO)
import qualified Control.Monad.Trans.State as State
import qualified Mescaline.Database as DB
import           Mescaline.FeatureSpace.Model (FeatureSpace)
import qualified Mescaline.FeatureSpace.Model as FeatureSpace
import qualified Mescaline.FeatureSpace.Unit as Unit
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Pattern.Sequencer as Sequencer
import           Mescaline (Time)
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Process.Monad
import           Sound.OpenSoundControl (Datum(..), OSC(..))
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.Transport.UDP as OSC

data Sound = Sound {
    region :: FeatureSpace.RegionId
  , sequencer :: Sequencer.Sequencer
  }

data RTState = RTState {
    rt_time :: Time
  , rt_featureSpace :: FeatureSpace.FeatureSpace
  , rt_sounds :: [(FeatureSpace.RegionId, Sound)]
  , rt_output :: [RTOutput]
  }

type RTModel a = State.StateT RTState (ServerT IO) a

time :: RTModel Time
time = State.gets rt_time

data RTInput = RTInput
data RTOutput =
    Synth Time Event.Synth

rtEngine :: [RTInput] -> RTModel ()
rtEngine = undefined

data RTEngine = RTEngine {
    rt_func :: [RTInput] -> RTModel ()
  , rt_sampleInterval :: Double
  , rt_inputChan :: TBChan RTInput
  , rt_outputChan :: TChan RTOutput
  }

runRT :: RTEngine -> RTState -> ServerT IO ()
runRT = undefined
-- runRT e = loop
--     where
--         loop s = do
--             is <- tryTakeMVar
--             s' <- State.execStateT (f (maybe [] reverse is)) s
--             let t' = rt_time s' + dt
--             OSC.pauseThreadUntil t'
--             loop s' { rt_time = t' }

data NRTState = NRTState {
    nrt_featureSpace :: FeatureSpace
  , nrt_output :: [NRTOutput]
  , nrt_rtInput :: [RTInput]
  }

type NRTModel a = State.StateT NRTState IO a

data NRTInput =
    LoadDatabase FilePath String
  | UpdateRegion FeatureSpace.Region
  deriving (Show)

data NRTOutput = NRTOutput

nrtEngine :: NRTInput -> NRTModel ()
nrtEngine e =
    case e of
        LoadDatabase path pattern -> do
            us <- DB.withDatabase path
                    $ Unit.getUnits pattern [
                        "es.globero.mescaline.spectral" ]
            liftIO $ print $ length us
            State.modify $ \s -> s { nrt_featureSpace = FeatureSpace.setUnits (nrt_featureSpace s) us }
        UpdateRegion r ->
            State.modify $ \s -> s { nrt_featureSpace = FeatureSpace.updateRegion r (nrt_featureSpace s) }

data NRTEngine = NRTEngine {
    nrt_func :: NRTInput -> NRTModel ()
  , nrt_inputChan :: TChan NRTInput
  , nrt_outputChan :: TChan NRTOutput
  , nrt_rtChan :: TBChan RTInput
  }

runNRT :: NRTEngine -> NRTState -> IO ()
runNRT e = loop
    where
        loop s = do
            x <- atomically $ readTChan (nrt_inputChan e)
            s' <- State.execStateT (nrt_func e x) s
            atomically $ mapM_ (writeTBChan (nrt_rtChan e)) (nrt_rtInput s')
            atomically $ mapM_ (writeTChan (nrt_outputChan e)) (nrt_output s')
            loop s' { nrt_output = [], nrt_rtInput = [] }

mkServer :: String -> Int -> TChan NRTInput -> TBChan RTInput -> IO ()
mkServer host port nrtChan rtChan = loop =<< OSC.udpServer host port
    where
        to_nrt = atomically . writeTChan nrtChan
        to_rt = atomically . writeTBChan rtChan
        loop t = do
            msg <- OSC.recv t
            case msg of
                Message "/loadDatabase" [String path, String pattern] ->
                    to_nrt $ LoadDatabase path pattern
                Message "/updateRegion" [Int i, Float x, Float y, Float r] ->
                    to_nrt $ UpdateRegion (FeatureSpace.mkRegion i (x,y) r)
                _ -> putStrLn $ "Unknown command: " ++ show msg
            loop t

displayFeatureSpace :: FeatureSpace -> IO ()
displayFeatureSpace fs = print (length $ FeatureSpace.units fs, FeatureSpace.regions fs)

main = do
    putStrLn "Fuck that shit"
    nrte <- atomically $ NRTEngine nrtEngine <$> newTChan <*> newTChan <*> newTBChan 256
    let nrts = NRTState FeatureSpace.empty [] []
    forkIO $ mkServer "127.0.0.1" 7800 (nrt_inputChan nrte) (nrt_rtChan nrte)
    runNRT nrte nrts
