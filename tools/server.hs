import           Control.Applicative
import           Control.Concurrent
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
    rt_time :: Double
  , rt_featureSpace :: FeatureSpace.FeatureSpace
  , rt_sounds :: [(FeatureSpace.RegionId, Sound)]
  , rt_output :: [Output]
  }

type Engine a = State.StateT RTState (ServerT IO) a

time :: Engine Double
time = State.gets s_time

-- | Update regions from current feature space.
-- updateRegions :: Engine ()
-- updateRegions = do
--     rs <- fmap (\r -> (FeatureSpace.regionId r, r)) . FeatureSpace.regions <$> State.gets s_featureSpace
--     State.modify $ \s ->
--         s { s_sounds = fmap (\(rid, s) -> (rid, s { region = maybe (region s) id (lookup rid rs) })) (s_sounds s) }
--     return ()

data Input = Input
data Output =
    Synth Time Event.Synth

engine :: [Input] -> Engine ()
engine = undefined

runEngine :: ([Input] -> Engine ()) -> Double -> MVar [Input] -> RTState -> ServerT IO ()
runEngine f dt mvar = loop
    where
        loop s = do
            is <- tryTakeMVar
            s' <- State.execStateT (f (maybe [] reverse is)) s
            let t' = rt_time s' + dt
            OSC.pauseThreadUntil t'
            loop s' { rt_time = t' }

data NRTState = NRTState {
    nrt_featureSpace :: FeatureSpace
  }

type Model a = State.StateT NRTState IO a

data NRTEvent =
    LoadDatabase FilePath String
  | UpdateRegion FeatureSpace.Region
  deriving (Show)

model :: NRTEvent -> Model ()
model e =
    case e of
        LoadDatabase path pattern -> do
            us <- DB.withDatabase path
                    $ Unit.getUnits pattern [
                        "es.globero.mescaline.spectral" ]
            State.modify (flip FeatureSpace.setUnits us)
        UpdateRegion r ->
            State.modify (FeatureSpace.updateRegion r)

runModel :: (NRTEvent -> Model ()) -> NRTState -> Chan NRTEvent -> MVar Input -> IO ()
runModel f 

serverLoop :: Chan FeatureSpaceEvent -> IO ()
serverLoop src = do
    t <- liftIO $ OSC.udpServer "127.0.0.1" 7800
    loop t src
    where
        loop t src = do
            msg <- OSC.recv t
            let e = case msg of
                        Message "/loadDatabase" [String path, String pattern] ->
                            LoadDatabase path pattern
                        Message "/updateRegion" [Int i, Float x, Float y, Float r] ->
                            UpdateRegion (FeatureSpace.mkRegion i (x,y) r)
            liftIO $ print e
            writeChan src e
            loop t src

mkServer :: Prepare m (Chan FeatureSpaceEvent, IO ())
mkServer = do
    src <- newChan
    return (src, serverLoop src)

displayFeatureSpace :: FeatureSpace -> IO ()
displayFeatureSpace fs = print (length $ FeatureSpace.units fs, FeatureSpace.regions fs)

main = do
    putStrLn "Fuck that shit"
    (server, loop) <- mkServer
    forkIO loop
    reactimate =<< R.map displayFeatureSpace =<< mkFeatureSpace FeatureSpace.empty server
    withDefaultInternal $ do
        