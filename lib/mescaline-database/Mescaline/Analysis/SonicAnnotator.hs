module Mescaline.Analysis.SonicAnnotator (
    SonicAnnotator
  , Segmentation(..)
  , analyser
  , defaultAnalyser
) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.List as List
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import           Mescaline.Analysis.Types
import qualified Sound.Analysis.Segmentation as S
import qualified Sound.Analysis.Segmentation.Statistics as S
import qualified Sound.Analysis.SonicAnnotator as SA
import           System.Exit
import           System.FilePath
import           System.Process

-- runSonicAnnotator args = do
--     (_, hOut, hErr, h) <-
--         runInteractiveProcess
--             "sonic-annotator"
--             args
--             Nothing
--             Nothing
--     out <- newEmptyMVar
--     err <- newEmptyMVar
--     forkIO $ BS.hGetContents hOut >>= putMVar out
--     forkIO $ BS.hGetContents hErr >>= putMVar err
--     exit <- waitForProcess h
--     outB <- readMVar out
--     errB <- readMVar err
--     return (exit, outB, errB)
-- 
-- decodeCSV x = D.decode "," (B.fromChunks [x])
-- 
-- forceEither :: Either String a -> IO a
-- forceEither = either fail return

-- getSegmentation file = do
--     (e, csv, err) <- runSonicAnnotator [ "-w", "csv", "--csv-stdout"
--                                        , "-t", "resources/analysis/segmentation.n3"
--                                        ,  file ]
--     -- BS.writeFile "segmentation.csv" csv
--     case e of
--         ExitFailure _ -> fail $ "Segmenter failed: " ++ BSC.unpack err
--         ExitSuccess   -> liftM (map (!!1)) (forceEither (decodeCSV csv))

data Transform = Transform {
    transformFileName :: String
  , transformName     :: String
  , transformDegree   :: Int
  } deriving (Eq, Show)

transforms :: [Transform]
transforms = [
    Transform "Coefficients" "http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc" 20
  , Transform "Linear Frequency Centroid" "http://vamp-plugins.org/rdf/plugins/vamp-example-plugins#spectralcentroid" 1
  ]

-- getUnitFeature :: Monad m => String -> Int -> E.Iteratee BS.ByteString m (Unit, Feature)
-- getUnitFeature name n = do
--     E.drop 1
--     u <- getUnit
--     E.drop 1
--     f <- getFeature (Descriptor name n)
--     return (u, f)

-- getUnits :: Monad m => Transform -> E.Iteratee [BS.ByteString] m [(Unit, Feature)]
-- getUnits transform = do
--     us <- go []
--     return $ reverse us
--     where
--         go us = do
--             m <- E.head
--             case m of
--                 Nothing -> return us
--                 Just row -> do
--                     (u, f) <- E.run_ $ E.enumList 1024 row $$
--                                 getUnitFeature (transformName transform)
--                                                (transformDegree transform)
--                     if onset u <= 0
--                         then go us
--                         else go ((u,f):us)

extractFeature :: SA.Analysis -> S.Segmentation Double a -> Transform -> [(Unit, Feature)]
extractFeature analysis segmentation transform = map conv (S.toList meanFeatures)
    where
        Just features = fmap (fmap (maybe UV.empty V.fromList)) $ Map.lookup (transformFileName transform) analysis
        segFeatures = S.segment segmentation features
        meanFeatures = fmap (V.toList.S.mean) segFeatures
        desc n = if n /= transformDegree transform
                    then error $ "descriptor/feature degree mismatch ("
                                ++ show (transformDegree transform) ++ "/" ++ show n ++ ")"
                    else Descriptor (transformName transform) (transformDegree transform)
        conv f = (Unit (S.onset f) (S.duration f), Feature (desc (length (S.label f))) (S.label f))

transposeFeatures :: [[(Unit, Feature)]] -> [(Unit, [Feature])]
transposeFeatures = map (\xs -> (fst (head xs), map snd xs)) . List.transpose

-- computeMeans :: S.Segmentation Double (Maybe (V.Vector Double)) -> S.Segmentation Double Double
-- computeMeans =

data Segmentation =
    None
  | Fixed { windowSize :: Double }
  | Onset { threshold :: Double }

data SonicAnnotator = SonicAnnotator Segmentation

analyser :: Segmentation -> SonicAnnotator
analyser = SonicAnnotator

defaultAnalyser :: SonicAnnotator
defaultAnalyser = analyser (Onset { threshold = 0.5 })

instance Analyser SonicAnnotator where
    analyse (SonicAnnotator segType) file = do
        analysis <- SA.analyse "features.n3" file
        sf <- newSoundFile file
        -- print $ S.labels $ fmap (length . fromJust) (fromJust (Map.lookup "Coefficients" analysis))
        let seg = case segType of
                    Fixed w -> S.fromEvents 0 $ map (\t -> (t, undefined)) $ take (truncate (fileDuration sf / w + 1)) $ iterate (+w) 0
                    _ -> analysis Map.! "Note Onsets"
            features = map (extractFeature analysis seg) transforms
        -- fs <- mapM (extractFeatures file seg) transforms
        -- print (length (S.toList segmentation))
        -- print (map length features)
        -- print (length (transposeFeatures features))
        -- print (map (map (length.value).snd) (transposeFeatures features))
        a <- newAnalysis file (transposeFeatures features)
        -- print (soundFile a)
        return a
    fileExtensions = const [
        "aif", "aiff", "au", "avi", "avr", "caf", "htk", "iff", "m4a", "m4b", "m4p", "m4v", "mat", "mov", "mp3", 
        "mp4", "ogg", "paf", "pvf", "raw", "sd2", "sds", "sf", "voc", "w64", "wav", "xi" ]
