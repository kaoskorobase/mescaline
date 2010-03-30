module Mescaline.Meap.Import (
    features
  , importDirectory
) where

import           Control.Monad
import qualified Data.Vector.Generic as V
import           Database.HDBC (IConnection)

import qualified Mescaline.Database as DB
import           Mescaline.Database.Feature (Feature)
import qualified Mescaline.Database.Feature as Feature
import           Mescaline.Database.Model ()
import           Mescaline.Database.Unit (Unit)
import qualified Mescaline.Database.Unit as Unit
import           Mescaline.Database.SourceFile (SourceFile)
import qualified Mescaline.Database.SourceFile as SourceFile
import           Mescaline.Database.Sql (SqlRow)
import qualified Mescaline.Database.Table as Table
import           Mescaline.Data.Array.Vector
import qualified Mescaline.Meap.Extractor as Extractor
import qualified Mescaline.Meap.Segmenter as Segmenter
import           Mescaline.Meap.Chain as Chain
import qualified Sound.Analysis.Meapsoft as Meap

meapFeatures :: [(String, Int)]
meapFeatures = [
    ( "AvgChroma"         , 12  )
  , ( "AvgChromaScalar"   , 1   )
  , ( "AvgChunkPower"     , 1   )
  , ( "AvgFreqSimple"     , 1   )
  , ( "AvgMelSpec"        , 40  )
  , ( "AvgMFCC"           , 13  )
  , ( "AvgPitch"          , 1   )
  , ( "AvgSpecCentroid"   , 1   )
  , ( "AvgSpecFlatness"   , 1   )
  , ( "AvgTonalCentroid"  , 6   )
  , ( "ChunkLength"       , 1   )
  , ( "ChunkStartTime"    , 1   )
  , ( "Entropy"           , 1   )
  , ( "RMSAmplitude"      , 1   )
  , ( "SpectralStability" , 1   )
  ]

meapFeaturePrefix :: String
meapFeaturePrefix = "com.meapsoft."

features :: [(String, Feature.Descriptor)]
features = map (\(n, d) -> (n, Feature.consDescriptor (meapFeaturePrefix ++ n) d)) meapFeatures

options :: Unit.Segmentation -> Chain.Options
options seg = Chain.defaultOptions {
            segmenter = Segmenter.defaultOptions {
                Segmenter.segmentation = case seg of
                                            Unit.Beat -> Segmenter.Beat
                                            Unit.Onset -> Segmenter.Onset
              , Segmenter.smoothingWindow = 0.01
            }
          , extractor = Extractor.defaultOptions {
                Extractor.windowSize = 1024,
                Extractor.hopSize = 512,
                Extractor.features  = map fst meapFeatures } }

convUnit :: SourceFile -> Unit.Segmentation -> (Double, Double) -> Unit
convUnit sf s (o, d) = Unit.cons sf s o d

convFeatureDesc :: Meap.Feature -> Feature.Descriptor
convFeatureDesc f = Feature.consDescriptor
                       (meapFeaturePrefix ++ (Meap.feature_name f))
                       (Meap.feature_degree f)

convFeature :: Feature.Descriptor -> Meap.Feature -> Unit -> [Double] -> Feature
convFeature d f u l = Feature.cons (Unit.id u) d v
    -- TODO: Make this more efficient
    where v = V.fromList . take (Meap.feature_degree f) . drop (Meap.feature_column f) $ l

insertModel :: (Table.Model a, SqlRow a, IConnection c) => c -> a -> IO a
insertModel c a = Table.insert c a >> return a

meapFrames :: Meap.MEAP -> [[Double]]
meapFrames meap = map (Meap.frame_l meap) [0..Meap.n_frames meap - 1]

insertFile :: IConnection c => [Unit.Segmentation] -> c -> FilePath -> (Chain.Options -> FilePath -> IO Meap.MEAP) -> IO ()
insertFile segs conn path getMeap = do
    -- mapM_ print (Meap.features meap)
    -- mapM_ print (meapFrames meap)
    sf <- SourceFile.newLocal path
    p <- Table.isStored conn sf
    putStrLn (path ++ " " ++ show sf ++ " " ++ show p)
    unless p $ do
        Table.insert conn sf
        mapM_ (insert sf) segs
        DB.commit conn
    where
        insert sf seg = do
            meap <- getMeap (options seg) path
            ds <- mapM (insertModel conn . convFeatureDesc) $ Meap.features meap
            us <- mapM (insertModel conn . convUnit sf seg) $ Meap.segments_l meap
            flip mapM_ (zip ds (Meap.features meap)) $ \(d, f) ->
                flip mapM_ (zip us (meapFrames meap)) $
                    insertModel conn . uncurry (convFeature d f)

importDirectory :: IConnection c => Int -> FilePath -> c -> IO ()
importDirectory np dir c = Chain.mapDirectory np (insertFile segs c) dir
    where segs = [Unit.Onset {- , Unit.Beat -}]
