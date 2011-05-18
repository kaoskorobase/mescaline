import           Control.Applicative
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
import           Reactive as R
import           Sound.OpenSoundControl (Datum(..), OSC(..))
import qualified Sound.OpenSoundControl as OSC
import qualified Sound.OpenSoundControl.Transport.UDP as OSC

data FeatureSpaceEvent =
    LoadDatabase FilePath String
  | UpdateRegion FeatureSpace.Region
  deriving (Show)

data Sound = Sound {
    region :: FeatureSpace.RegionId
  , sequencer :: Sequencer.Sequencer
  }

data State = State {
    s_time :: Double
  , s_featureSpace :: FeatureSpace.FeatureSpace
  , s_sounds :: [(FeatureSpace.RegionId, Sound)]
  , s_output :: [Output]
  }

type Engine a = State.StateT State (ServerT IO) a

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

mkFeatureSpace :: MonadControlIO m => FeatureSpace -> Event m FeatureSpaceEvent -> Prepare m (Behavior m FeatureSpace)
mkFeatureSpace = accumulateM $ \e fs ->
    case e of
        LoadDatabase path pattern -> do
            us <- DB.withDatabase path
                    $ Unit.getUnits pattern [
                        "es.globero.mescaline.spectral" ]
            return $ FeatureSpace.setUnits fs us
        UpdateRegion r ->
            return $ FeatureSpace.updateRegion r fs

serverLoop :: MonadIO m => EventSource m FeatureSpaceEvent -> m ()
serverLoop src = do
    t <- liftIO $ OSC.udpServer "127.0.0.1" 7800    
    loop t src
    where
        loop t src = do
            msg <- liftIO $ OSC.recv t
            let e = case msg of
                        Message "/loadDatabase" [String path, String pattern] ->
                            LoadDatabase path pattern
                        Message "/updateRegion" [Int i, Float x, Float y, Float r] ->
                            UpdateRegion (FeatureSpace.mkRegion i (x,y) r)
            liftIO $ print e
            fire src e
            loop t src

mkServer :: MonadIO m => Prepare m (Event m FeatureSpaceEvent, Prepare m ())
mkServer = do
    src <- newEventSource
    return (fromEventSource src, serverLoop src)

displayFeatureSpace :: FeatureSpace -> IO ()
displayFeatureSpace fs = print (length $ FeatureSpace.units fs, FeatureSpace.regions fs)

main = do
    putStrLn "Fuck that shit"
    (server, loop) <- mkServer
    reactimate =<< R.map displayFeatureSpace =<< mkFeatureSpace FeatureSpace.empty server
    loop
