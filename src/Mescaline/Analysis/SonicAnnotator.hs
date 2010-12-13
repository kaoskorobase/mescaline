module Mescaline.Analysis.SonicAnnotator (
    SonicAnnotator
  , analyser
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import           Mescaline.Analysis
import qualified Mescaline.Data.ListReader as L
import           System.Exit
import           System.FilePath
import           System.Process
import           Text.Delimited as D
import           Sound.SC3.Lang.Collection.SequenceableCollection (clump)

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

getSegmentation file = do
    (e, x, err) <- runSonicAnnotator [ "-w", "csv", "--csv-stdout"
                                     , "-t", "resources/analysis/segmentation.n3"
                                     ,  file ]
    putStrLn (BSC.unpack err)
    case e of
        ExitFailure _ -> return $ Left "Shit!"
        ExitSuccess   -> return $ fmap (map (!!1)) (decodeCSV x)

data Transform = Transform {
    transformFile   :: String
  , transformName   :: String
  , transformDegree :: Int
}

transforms :: [Transform]
transforms = [
    Transform "mfcc.n3" "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc" 13
  , Transform "spectralcentroid.n3" "http://vamp-plugins.org/rdf/plugins/vamp-example-plugins#spectralcentroid" 1
  ]

getUnitFeature :: String -> Int -> L.ListReader BS.ByteString (Unit, Feature)
getUnitFeature name n = do
    L.tail
    u <- getUnit
    L.tail
    f <- getFeature (Descriptor name n)
    return (u, f)

execTransform t = mapM (L.execListReader (getUnitFeature (transformName t) (transformDegree t)))

getUnits :: FilePath -> [BS.ByteString] -> IO [(Unit, [Feature])]
getUnits file segmentation = do
    (e, x, err) <- runSonicAnnotator $
                        [ "-w", "csv", "--csv-stdout"
                        , "-S", "mean", "--summary-only"
                        , "--segments", BSC.unpack (BS.intercalate (BSC.pack ",") segmentation)
                        ]
                        ++ concatMap (\t -> [ "-t", "resources/analysis" </> transformFile t ]) transforms
                        ++ [ file ]
    putStrLn (BSC.unpack err)
    case e of
        ExitFailure _ -> fail "Fucking asshole"
        ExitSuccess ->
            case decodeCSV x of
                Left e -> fail e
                Right rows -> do
                    let n = length segmentation + 1
                    fs <- either fail (return . List.transpose) (zipWithM execTransform transforms (clump n rows))
                    return $ map (\us -> (fst (head us), map snd us)) fs

data SonicAnnotator = SonicAnnotator

analyser :: SonicAnnotator
analyser = SonicAnnotator

instance Analyser SonicAnnotator where
    analyse _ file = do
        seg <- getSegmentation file
        case seg of
            Left e -> fail e
            Right seg -> do
                us <- getUnits file seg
                newAnalysis file us
    fileExtensions = const [
        "aif", "aiff", "au", "avi", "avr", "caf", "htk", "iff", "m4a", "m4b", "m4p", "m4v", "mat", "mov", "mp3", 
        "mp4", "ogg", "paf", "pvf", "raw", "sd2", "sds", "sf", "voc", "w64", "wav", "xi" ]
