{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

import qualified Control.Concurrent.Process as Process
import qualified Control.Seq as Seq
import qualified Control.Exception as E
import qualified Data.List as List
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Data.Accessor
import           Data.Accessor.Template
import qualified Data.Foldable as Fold
import qualified Data.List as List
import           Data.Ord (comparing)
import qualified Data.Vector as BV
import qualified Data.Vector.Generic as V
import           Engine
import           Mescaline.Application (AppT)
import qualified Mescaline.Application as App
import qualified Mescaline.Database as DB
import qualified Mescaline.FeatureSpace.Process as FSpace
import qualified Mescaline.FeatureSpace.Model as FSpace
import qualified Mescaline.FeatureSpace.Unit as Unit
import qualified Mescaline.Pattern.Event as Event
import qualified Mescaline.Synth.Sampler.Process as Synth
import qualified Paths_mescaline_sts as Paths
import qualified Sound.OpenSoundControl.Time as Time
import           System.Console.CmdArgs.Explicit
import           System.Random.MWC

setEnv = setVal Event.attackTime 0.001 . setVal Event.releaseTime 0.001

playUnit synth time = Process.sendTo synth . Synth.PlayUnit (time + 0.2) . setEnv . Event.defaultSynth

-- playUnitSched synth time unit = do
--     -- let time' = time + Unit.duration unit
--     -- print (time, Unit.duration unit, time')
--     playUnit synth (time + 0.2) unit
--     -- Time.pauseThreadUntil time'
--     -- return time'

data Options = Options {
    dbFile_ :: FilePath
  , help_   :: Bool
  , level_ :: Int
  , cluster_ :: Int
  } deriving (Eq, Show)

$(deriveAccessors ''Options)

defaultOptions :: Options
defaultOptions = Options {
    dbFile_ = ""
  , help_   = False
  , level_ = 1
  , cluster_ = 1
  }

arguments :: Mode Options
arguments = mode "mescaline-sts" defaultOptions "Mescaline Sound Texture Synthesis" (flagArg (upd dbFile) "FILE")
    [ flagNone ["help", "h"] (setVal help True) "Show this help"
    , flagReq ["level","l"] (\v -> upd level (read v)) "LEVEL" "Clustering level"
    , flagReq ["cluster","c"] (\v -> upd cluster (read v)) "CLUSTER" "Cluster to play"
    ]
    -- , flagOpt "world" ["hello","w"] (upd "world") "WHO" "World argument"
    where upd a x v = Right $ setVal a x v

scheduler :: (Double -> Unit.Unit -> IO ()) -> Double -> [Unit.Unit] -> IO ()
scheduler f t us = do
    case us of
        [] -> return ()
        (u:us') -> do
            f t u
            print $ Unit.duration u
            let t' = t + Unit.duration u
            Time.pauseThreadUntil t'
            scheduler f t' us'

-- randScheduler :: (Double -> Unit.Unit -> IO ()) -> Double -> BV.Vector Unit.Unit -> IO ()
randScheduler f t gen us = do
    i <- uniformR (0, V.length us - 1) gen
    let u = us V.! i
    f t u
    print $ Unit.duration u
    let t' = t + Unit.duration u
    Time.pauseThreadUntil t'
    randScheduler f t' gen us

appMain :: Options -> AppT IO ()
appMain opts = do
    (synthP, fspaceP, cleanup) <- engine (opts ^. dbFile) ".*"

    -- units <- DB.withDatabase (opts ^. dbFile) $ Unit.getUnits ".*" ["es.globero.mescaline.cluster_" ++ show (opts ^. level), "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc"]
    -- let units' = filter (\u -> V.head (DB.toVector (DB.featureValue (V.head (Unit.features u)))) == fromIntegral (opts ^. cluster)) units
    units <- DB.withDatabase (opts ^. dbFile) $ Unit.getUnits ".*" []
    let units' = units
    Seq.withStrategy (Seq.seqList Seq.rseq) units' `seq` return ()
    -- liftIO $ print $ length units'

    -- liftIO $ putStr $ unlines $ map (List.intercalate "," . map show . V.toList . DB.toVector . DB.featureValue . flip (V.!) 1 . Unit.features) units'

    -- liftIO $ forever $ do
    --     time <- Time.utcr
    --     -- Fold.foldlM (playUnitSched synthP) time (List.sortBy (comparing Unit.onset) units')
    --     scheduler (playUnit synthP) time (List.sortBy (comparing Unit.onset) units')
    liftIO $ forever $ do
        time <- Time.utcr
        -- Fold.foldlM (playUnitSched synthP) time (List.sortBy (comparing Unit.onset) units')
        withSystemRandom $ \gen ->
            randScheduler (playUnit synthP) time gen (V.fromList units' :: BV.Vector Unit.Unit)

    -- tm <- mkTouches $ \vc touch -> do
    --     x <- getTouchX vc touch
    --     y <- getTouchY vc touch
    --     w <- getTouchViewWidth touch
    --     h <- getTouchViewHeight touch
    --     hPutStrLn log $ show [x, y, w, h, x/w, y/h]
    --     -- OSC.Message "/closest" [OSC.Float x, OSC.Float y] -> do
    --     --     -- io $ putStrLn $ "/closest "  ++ show x ++ " " ++ show y
    --     fspace <- Process.query fspaceP FSpace.GetModel
    --     playUnit synthP (fmap fst (FSpace.closest2D (realToFrac (x/w), realToFrac (y/h)) fspace))
    --     --     return g
    --     
    --     connect vc touch lastPoint

    liftIO cleanup

main :: IO ()
main = do
    opts <- processArgs arguments
    if opts ^. help || null (opts ^. dbFile)
        then print $ helpText HelpFormatDefault arguments
        else App.runAppT (appMain opts) =<< App.mkApp "MescalineSTS" Paths.version Paths.getBinDir Paths.getDataDir App.defaultConfigFiles
