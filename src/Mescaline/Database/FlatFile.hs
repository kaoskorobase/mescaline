{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Mescaline.Database.FlatFile (
    Database(..)
  , Result
  , open
  , query
  , module Mescaline.Database.Query
) where

import           Control.Monad (zipWithM)
import           Data.List (group)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Vector.Generic as V

import qualified Mescaline.Data.Unique as Unique
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Database.SoundFile as SF
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Query
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.Unit (Unit)
import qualified Sound.Analysis.Meapsoft as Meap
import           System.FilePath
import qualified System.FilePath.Find as Find
import qualified System.Random as Random

data Feature = Feature {
    f_name :: String
  , f_degree :: Int
  , f_column :: Int
} deriving (Eq, Show)

type FeatureMap = Map.Map String Feature

data Database = Database {
    features    :: FeatureMap,
    sourceFiles :: [SourceFile],
    units       :: [Unit]
} deriving (Show)

type Result = [Unit]

featureDegrees :: [(String, Int)]
featureDegrees = [
    ( "AvgChroma"         , 12  ),
    ( "AvgChromaScalar"   , 1   ),
    ( "AvgChunkPower"     , 1   ),
    ( "AvgFreqSimple"     , 1   ),
    ( "AvgMelSpec"        , 40  ),
    ( "AvgMFCC"           , 13  ),
    ( "AvgPitch"          , 1   ),
    ( "AvgSpec"           , 513 ),
    ( "AvgSpecCentroid"   , 1   ),
    ( "AvgSpecFlatness"   , 1   ),
    ( "AvgTonalCentroid"  , 6   ),
    ( "ChunkLength"       , 1   ),
    ( "ChunkStartTime"    , 1   ),
    ( "Entropy"           , 1   ),
    ( "RMSAmplitude"      , 1   ),
    ( "SpectralStability" , 1   )
    ]

mkFeatures :: [String] -> [Feature]
mkFeatures fs = zipWith3 Feature fs ds cs
    where
        ds = fromJust $ mapM (flip lookup featureDegrees) fs
        cs = scanl (+) 0 ds

meapFeatureData :: Meap.MEAP -> [Feature.Value]
meapFeatureData m = map (V.fromList.drop 2.Meap.frame_l m) [0..Meap.n_frames m - 1]

readUnits :: FilePath -> IO [((Double, Double), Feature.Value)]
readUnits path = do
    res <- Meap.read_meap path
    case res of
        Left err -> fail err
        Right m  -> let ss = Meap.segments_l m
                        vs = meapFeatureData m
                    in if length ss /= length vs
                        then fail "Ooops."
                        else return $ zip ss vs

featuresFromFile :: FilePath -> IO FeatureMap
featuresFromFile path = do
    fs <- (mkFeatures.lines) `fmap` readFile path
    return $ Map.fromList $ zip (map f_name fs) fs

zipWithIds :: [a] -> IO [(Unique.Id, a)]
zipWithIds = mapM (\a -> flip (,) a `fmap` Random.randomIO)

open :: FilePath -> IO Database
open path = do
    features <- featuresFromFile $ joinPath [path, "meap.db"]
    files <- Find.find
                Find.always
                (fmap (== ".feat_beats") Find.extension)
                path
    assocs <- zipWithIds files >>= mapM (uncurry newSourceFile)
    let sourceFiles = map fst assocs
        units       = concat $ map snd assocs
    return $ Database features sourceFiles units
    where
        newSourceFile i f = do
            let sfPath = dropExtension f
            info <- SF.getInfo sfPath
            let sf = SourceFile.SourceFile
                        i sfPath SourceFile.noHash
                        (SF.channels info)
                        (fromIntegral $ SF.samplerate info)
                        (SF.frames info)
            assocs <- readUnits f
            specs <- zipWithIds (zip (repeat sf) assocs)
            return (sf, map mkUnit specs)
        mkUnit (i, (sf, ((o, d), vs))) = Unit.unsafeCons i sf Unit.Beat o d

query :: Query -> Database -> [Unit]
query (Query q) = filter q . units
