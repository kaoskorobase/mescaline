module Mescaline.Meap.Import (
    features
  , lookupFeature
  , importPaths
  , audioFileExtensions
) where

import           Control.Monad
import qualified Data.Vector.Generic as V
import           Database.HDBC (IConnection)
import qualified GHC.Conc as GHC
import qualified Mescaline.Application.Logger as Log
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
import           Mescaline.Meap.Chain as Chain
import qualified Mescaline.Meap.Extractor as Extractor
import           Mescaline.Meap.Process (OutputHandler(..))
import qualified Mescaline.Meap.Segmenter as Segmenter
import           Mescaline.Util (findFiles)
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
features = map (\(n, d) -> let n' = meapFeaturePrefix ++ n in (n', Feature.consDescriptor n' d)) meapFeatures

lookupFeature :: String -> Maybe Feature.Descriptor
lookupFeature = flip lookup features

options :: Unit.Segmentation -> Chain.Options
options seg =
    Chain.defaultOptions {
        segmenter = Segmenter.defaultOptions {
            Segmenter.outputHandler = outputHandler
          , Segmenter.segmentation = case seg of
                                        Unit.Beat -> Segmenter.Beat
                                        Unit.Onset -> Segmenter.Onset
          , Segmenter.smoothingWindow = 0.01
        }
      , extractor = Extractor.defaultOptions {
            Extractor.outputHandler = outputHandler
          , Extractor.windowSize = 1024
          , Extractor.hopSize = 512
          , Extractor.features  = map fst meapFeatures } }
    where
        outputHandler = OutputHandler (Log.noticeM "Database") (Log.errorM "Database")

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

-- | Insert a single file and its analysis data into the database.
--
-- Currently, concurrent database accesses don't seem to be possible.
insertFile :: IConnection c
           => c
           -> FilePath
           -> Unit.Segmentation
           -> Meap.MEAP
           -> IO ()
insertFile conn path seg meap = do
    sf <- SourceFile.newLocal path
    Log.noticeM "Database" ("insertFile: " ++ path ++ " " ++ show sf)
    Table.insert conn sf
    ds <- mapM (insertModel conn . convFeatureDesc) $ Meap.features meap
    us <- mapM (insertModel conn . convUnit sf seg) $ Meap.segments_l meap
    flip mapM_ (zip ds (Meap.features meap)) $ \(d, f) ->
        flip mapM_ (zip us (meapFrames meap)) $
            insertModel conn . uncurry (convFeature d f)
    DB.commit conn

-- | List of audio file extensions Meap can handle at the moment.
audioFileExtensions :: [String]
audioFileExtensions = ["aif", "aiff", "wav"]

-- | Import a file or directory into the database.
importPaths :: IConnection c => Maybe Int -> [FilePath] -> c -> IO ()
importPaths np ps c = do
    files <- findFiles audioFileExtensions ps
    -- FIXME: Avoid computing the source file hash twice.
    files' <- filterM (\p -> do { sf <- SourceFile.newLocal p ; fmap not $ Table.isStored c sf }) files
    Chain.mapFiles (maybe GHC.numCapabilities id np) (options seg) files'
    >>= mapM_ (\(p, e) ->
            either (\s -> Log.errorM "Database" (p ++ ": " ++ s))
                   (insertFile c p seg)
                   e)
    where seg = Unit.Onset
