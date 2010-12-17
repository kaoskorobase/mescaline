module Mescaline.Analysis.SonicAnnotator (
    SonicAnnotator
  , analyser
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.List as List
import           Mescaline.Analysis.Types
import           System.Exit
import           System.FilePath
import           System.Process
import           Text.Delimited as D

runSonicAnnotator args = do
    (_, hOut, hErr, h) <-
        runInteractiveProcess
            "sonic-annotator"
            args
            Nothing
            Nothing
    out <- newEmptyMVar
    err <- newEmptyMVar
    forkIO $ BS.hGetContents hOut >>= putMVar out
    forkIO $ BS.hGetContents hErr >>= putMVar err
    exit <- waitForProcess h
    outB <- readMVar out
    errB <- readMVar err
    return (exit, outB, errB)

decodeCSV x = D.decode "," (B.fromChunks [x])

forceEither :: Either String a -> IO a
forceEither = either fail return

getSegmentation file = do
    (e, csv, err) <- runSonicAnnotator [ "-w", "csv", "--csv-stdout"
                                       , "-t", "resources/analysis/segmentation.n3"
                                       ,  file ]
    -- BS.writeFile "segmentation.csv" csv
    case e of
        ExitFailure _ -> fail $ "Segmenter failed: " ++ BSC.unpack err
        ExitSuccess   -> liftM (map (!!1)) (forceEither (decodeCSV csv))

data Transform = Transform {
    transformFile   :: String
  , transformName   :: String
  , transformDegree :: Int
  } deriving (Eq, Show)

transforms :: [Transform]
transforms = [
    Transform "mfcc.n3" "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc" 13
  , Transform "spectralcentroid.n3" "http://vamp-plugins.org/rdf/plugins/vamp-example-plugins#spectralcentroid" 1
  ]

getUnitFeature :: Monad m => String -> Int -> E.Iteratee BS.ByteString m (Unit, Feature)
getUnitFeature name n = do
    E.drop 1
    u <- getUnit
    E.drop 1
    f <- getFeature (Descriptor name n)
    return (u, f)

getUnits :: Monad m => Transform -> E.Iteratee [BS.ByteString] m [(Unit, Feature)]
getUnits transform = do
    us <- go []
    return $ reverse us
    where
        go us = do
            m <- E.head
            case m of
                Nothing -> return us
                Just row -> do
                    (u, f) <- E.run_ $ E.enumList 1024 row $$
                                getUnitFeature (transformName transform)
                                               (transformDegree transform)
                    if onset u <= 0
                        then go us
                        else go ((u,f):us)

extractFeatures :: FilePath -> [BS.ByteString] -> Transform -> IO [(Unit, Feature)]
extractFeatures file segmentation transform = do
    let segs = BSC.unpack (BS.intercalate (BSC.pack ",") segmentation)
    (e, csv, err) <- runSonicAnnotator $
                        [ "-w", "csv", "--csv-stdout"
                        , "-S", "mean", "--summary-only"
                        , "--segments", segs
                        , "-t", "resources/analysis" </> transformFile transform
                        ]
                        ++ [ file ]
    -- BS.writeFile (takeFileName (transformName transform) ++ ".csv") csv
    case e of
        ExitFailure _ -> fail $ "Extractor failed: " ++ BSC.unpack err
        ExitSuccess -> do
            rows <- forceEither (decodeCSV csv)
            E.run_ (E.enumList 1024 rows $$ getUnits transform)

transposeFeatures :: [[(Unit, Feature)]] -> [(Unit, [Feature])]
transposeFeatures = map (\xs -> (fst (head xs), map snd xs)) . List.transpose

data SonicAnnotator = SonicAnnotator

analyser :: SonicAnnotator
analyser = SonicAnnotator

instance Analyser SonicAnnotator where
    analyse _ file = do
        seg <- getSegmentation file
        fs <- mapM (extractFeatures file seg) transforms
        newAnalysis file (transposeFeatures fs)
    fileExtensions = const [
        "aif", "aiff", "au", "avi", "avr", "caf", "htk", "iff", "m4a", "m4b", "m4p", "m4v", "mat", "mov", "mp3", 
        "mp4", "ogg", "paf", "pvf", "raw", "sd2", "sds", "sf", "voc", "w64", "wav", "xi" ]
