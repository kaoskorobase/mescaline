{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
-- module Mescaline.EngineSF where

import           Control.Applicative
import           Control.Arrow ((>>>), first, second)
import           Control.Concurrent (ThreadId, forkIO, forkOS, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.Chan.Chunked as CChan
import           Control.Monad (forever, liftM)
import           Control.Monad.Fix (fix)
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import           Data.Monoid (mappend, mempty)
import           Mescaline (Time)
import qualified Mescaline.Data.PriorityQueue as PQ
import           Mescaline.Data.Signal
import           Mescaline.FeatureSpace.Model (FeatureSpace, RegionId, Region)
import qualified Mescaline.FeatureSpace.Model as FeatureSpace
import           Mescaline.Pattern (Pattern)
import           Mescaline.Pattern.Event (Event)
import           Sound.OpenSoundControl.Time (pauseThreadUntil, utcr)
import qualified System.Console.SimpleLineEditor as RL

-- data Input =
--     UpdateRegion RegionId Region
--   | PatternControl RegionId Control Value
--   | SetPattern RegionId Pattern
-- 
-- data Output =
--     RegionChanged RegionId Region
--   | PatternChanged PatternOutput
--   | UnitStarted Unit
--   | UnitStopped Unit

-- instance Show FeatureSpace where
--     show _ = "FeatureSpace"

-- data Scheduler m = Scheduler (PQ.PQueue Time (m ()))
-- 
-- instance Show (Scheduler m) where
--     show _ = "Scheduler"
-- 
-- scheduler :: Scheduler m
-- scheduler = Scheduler PQ.empty
-- 
-- schedule :: Monad m => Time -> m () -> Scheduler m -> Scheduler m
-- schedule time action (Scheduler pq) = Scheduler (PQ.insert time action pq)
-- 
-- tick :: Monad m => Time -> Scheduler m -> m (Scheduler m)
-- tick time (Scheduler pq) = do
--     let (as, pq') = PQ.popUntil pq time
--     sequence as
--     return (Scheduler pq')

type UnitId = Int
data Unit = Unit deriving Show
-- type FeatureSpace = [Unit]

-- type RegionId = Int
-- data Region = Region (Double, Double) Double deriving (Read, Show)

-- data PatternRate
-- data Pattern a = Pattern (EventS TickRate a) deriving (Show)

data UnitFilter =
    Exclude UnitId
    deriving (Show)

data Filter = Filter {
    files :: [FilePath]
  , unitFilter :: [UnitFilter]
  } deriving (Show)

type Vector a = [a]

class IsMapping a where
    map2D :: a -> Vector Double -> Vector Double

instance IsMapping (Vector Double -> Vector Double) where
    map2D f = f

data Mapping = forall a . IsMapping a => Mapping {
    features :: [String]
  , featureMapping :: a
  }

instance Show Mapping where
    show _ = "Mapping"

data View = View {
    filter :: Filter
  , mapping :: Mapping
  } deriving (Show)

type RegionMap = Map.Map RegionId Region

type ProcessId = Int
type Process = Pattern Event
type ProcessMap = Map.Map ProcessId Process

instance Show FeatureSpace where
    show _ = "FeatureSpace"

type SceneId = Int

data Scene = Scene {
    view :: View
  , featureSpace :: FeatureSpace
  , processes :: PQ.PriorityQueue Time (RegionId, Process)
  } deriving (Show)

defaultScene :: Scene
defaultScene = Scene (View (Filter [] []) (Mapping [] (id :: Vector Double -> Vector Double))) FeatureSpace.empty Map.empty

type SceneMap = Map.Map SceneId Scene

data State = State {
    time :: !Time
  , scenes :: SceneMap
  , currentScene :: Maybe Scene
  } deriving (Show)

makeState :: Time -> State
makeState t0 = State t0 Map.empty Nothing

data TransportChange = Start | Pause | Reset deriving (Eq, Read, Show)

data Input =
    GetState
  | AddScene SceneId
    -- Regions
  | AddRegion SceneId RegionId (Double,Double) Double
  | SetPattern SceneId RegionId ()
  | UpdateRegion SceneId RegionId (Double,Double) Double
    -- Patterns
    -- Transport
  | Transport TransportChange
    deriving (Read, Show)

data Output =
    GetState' State
  | AddScene' SceneId Scene
    -- Regions
  | AddRegion' SceneId RegionId
  | UpdateRegion' Scene RegionId (Double,Double) Double
    -- Synths
  | Synth' SynthCommand
    -- Transport
  | Transport' TransportChange
    deriving (Show)

data SynthCommand =
    StartUnit Time Event
  | StopUnit Time
  deriving (Show)

data TickRate

type Update a = State.State ([Output], State) a

runUpdate :: Update a -> State -> ([Output], State)
runUpdate u s = State.execState u (mempty, s)

get :: Update State
get = liftM snd State.get

gets :: (State -> a) -> Update a
gets f = liftM f get

modify :: (State -> State) -> Update ()
modify f = State.modify (\(o, s) -> (o, f s))

output :: Output -> Update ()
output o = State.modify (\(os, s) -> (os `mappend` [o], s))

update :: Input -> Update ()
update GetState = output =<< gets GetState'
update (AddScene si) = do
    let sc = defaultScene
    modify (\s -> s { scenes = Map.insert si sc (scenes s) })
    output (AddScene' si sc)
update (AddRegion si ri c r) = do
    modify $ \s ->
        case Map.lookup si (scenes s) of
            Nothing -> s
            Just sc ->
                s {
                    scenes =
                        Map.insert si (sc {
                            featureSpace = FeatureSpace.updateRegion (FeatureSpace.mkRegion ri c r) (featureSpace sc)
                            })
                            (scenes s) }
    output $ AddRegion' si ri
update (Transport t) =
    output $ Transport' t

-- combine :: [a -> ([b], a)] -> [b] -> a -> ([b], a)
-- combine fs bs a = foldl (\(bs, a) f -> let (bs', a') = f a in (bs ++ bs', a')) (bs, a) fs

-- eventS :: Signal [a] -> EventS [a]
-- eventS = fmap f
--     where
--         f [] -> Nothing
--         f as -> Just as
-- 
-- regionS :: Signal ([Input], State) -> Signal ([Output], Maybe (RegionMap -> RegionMap))
-- regionS s =
--     let (inputs, state) = unzipS
--         events = eventS inputs
--         f (is, s) = 
--     in fmap f (events `snapshot` state)

tick :: Time -> State -> State
tick dt s = s { time = time s + dt }

engine :: Double -> Signal TickRate ([Input], State) -> Signal TickRate ([Output], State)
engine dt = fmap (\(inputs, state) -> runUpdate (mapM_ update inputs) state)
        >>> fmap (second (tick dt))

mkEngine :: Double -> IO ([Input] -> IO (), IO [Output])
mkEngine dt = do
    ichan <- CChan.newChan
    (inputS, addInput) <- makeStream
    ochan <- Chan.newChan
    forkOS $ do
        t0 <- utcr
        let state0 = makeState t0
            (outputS, stateS) = unzipS (engine dt (zipS inputS (initS state0 stateS)))
            getInput = CChan.readChanAvailable ichan >>= addInput
            putOutput = Chan.writeChan ochan
            run (inp:inps) (outp:outps) (s:ss) = do
                -- print (inp, outp, s)
                putOutput outp
                -- putStrLn "sent output"
                -- FIXME: `pauseThreadUntil' doesn't seem to work in ghci 7.0.1!
                dtms <- liftM (floor . (*1e6) . (time s -)) utcr
                -- print dtms
                threadDelay dtms
                getInput
                -- putStrLn "recurse"
                run inps outps ss
        getInput
        run (unS inputS) (unS outputS) (unS stateS)
    return (CChan.writeList2Chan ichan, Chan.readChan ochan)

-- engine2 :: Time -> [Input] -> State -> ([Output], State)
-- engine2 dt is state = ([], tick dt state)
-- 
-- mkEngine2 :: Double -> IO ([Input] -> IO (), IO [Output])
-- mkEngine2 dt = do
--     ichan <- CChan.newChan
--     ochan <- Chan.newChan
--     forkOS $ do
--         let getInputs = CChan.readChanAvailable ichan
--             putOutputs = Chan.writeChan ochan
--             run state = do
--                 is <- getInputs
--                 let (os, state') = engine2 dt is state
--                 putOutputs os
--                 dt <- liftM (floor . (*1e6) . (time state' -)) utcr
--                 threadDelay dt
--                 run state'
--         run . makeState =<< utcr
--     return (CChan.writeList2Chan ichan, Chan.readChan ochan)

mkDebugEngine :: Double -> IO ([Input] -> IO ())
mkDebugEngine dt = do
    (i, o) <- mkEngine dt
    forkIO $ forever $ do
        os <- o
        case os of
            [] -> return ()
            _ -> print os
    return i

main = do
    RL.initialise
    i <- mkDebugEngine 1e-3
    fix $ \loop -> do
        l <- RL.getLineEdited "> "
        case l of
            Nothing -> return ()
            Just "" -> loop
            Just "exit" -> return ()
            Just s -> do
                case readMaybe s of
                    Nothing -> putStrLn "Parse Error"
                    Just a -> i [a]
                loop
    RL.restore
    return ()
    where
        readMaybe s = case reads s of
                        ((a, _):_) -> return a
                        _          -> Nothing
