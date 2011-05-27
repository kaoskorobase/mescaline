-- | Some simple tests for the database backend.
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad
import qualified Data.Map as Map
import qualified Data.Vector.Generic as V
import           Database.Persist
import qualified Mescaline.Analysis as Analysis
import           Mescaline.Database
import           Mescaline.Database.Hash
import           System.Random
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit.Lang

-- | TODO:
--
-- * Test 'Analysis' insertion: features retain their dimensionality and values.
--
-- * Test 'Transformation': compare PCA to reference implementation.

-- | Test if the features returned by a query match the original feature set.
test_insertQueryIdentity :: Assertion
test_insertQueryIdentity = withDatabase ":memory:" $ do
    -- Create a list of descriptors with corresponding degrees
    let ns = [1, 8, 2, 20]
    ds <- mapM (\n -> fmap fst $ getDescriptor ("test_feature_" ++ show n) n) ns
    -- Create a dummy source file
    sfi <- insert $ SourceFile "/path/to/sf" noHash 2 44100 44100
    -- Create a list of dummy units
    uis <- mapM (\o -> insert $ Unit sfi (fromIntegral o) 1) [0..511]
    -- Create random features for each unit corresponding to the descriptors created above
    us <- mapM (\ui -> do { vs <- mapM (\n -> fmap V.fromList (replicateM n (liftIO randomIO))) ns
                          ; return (ui, zipWith (Feature ui) ds vs) })
               uis
    -- Insert features
    mapM_ (\(_, fs) -> mapM_ insertFeature fs) us
    -- Query features
    (_, um) <- queryFeatures (const True) ds
    -- Create mappings (UnitId, [[Double]]) for the input and the query
    let um0 = fmap (map (V.toList . featureValue)) (Map.fromList us)
        um' = fmap (\(_, fs) -> map (V.toList . featureValue) fs) um
    -- Compare input and query
    when (um0 /= um') $ liftIO $ assertFailure "insert/query"

-- | Test if an identity transform yields the concatentation of the original feature set.
test_transformIdentity :: Assertion
test_transformIdentity = withDatabase ":memory:" $ do
    -- Create a list of descriptors with corresponding degrees
    let ns = [1, 8, 20, 2]
    ds <- mapM (\n -> fmap fst $ getDescriptor ("test_feature_" ++ show n) n) ns
    -- Create transformation target descriptor
    (di, d) <- getDescriptor "test_transform_feature" (sum ns)
    -- Create a dummy source file
    sfi <- insert $ SourceFile "/path/to/sf" noHash 2 44100 44100
    -- Create a list of dummy units
    uis <- mapM (\o -> insert $ Unit sfi (fromIntegral o) 1) [0..511]
    -- Create random features for each unit corresponding to the descriptors created above
    us <- mapM (\ui -> do { vs <- mapM (\n -> fmap V.fromList (replicateM n (liftIO randomIO))) ns
                          ; return (ui, zipWith (Feature ui) ds vs) })
               uis
    -- Insert features
    mapM_ (\(_, fs) -> mapM_ insertFeature fs) us
    -- Transform features
    transformFeatureP (Transformation id) ds (Analysis.Descriptor (descriptorName d) (descriptorDegree d))
    -- Query features
    (_, um) <- queryFeatures (const True) [di]
    -- Create mappings (UnitId, [Double]) for the input and the transformation
    let um0 = fmap (concatMap (V.toList . featureValue)) (Map.fromList us)
        um' = fmap (\(_, fs) -> concatMap (V.toList . featureValue) fs) um
    -- Compare input and transformation
    when (um0 /= um') $ liftIO $ assertFailure "transform identity"

tests :: [Test]
tests = [ testCase "insert/query identity" test_insertQueryIdentity
        , testCase "transform identity" test_transformIdentity ]

main :: IO ()
main = defaultMain tests
