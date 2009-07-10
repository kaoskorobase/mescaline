import Mescaline.Meap.Chain                 (Options(..), defaultOptions, mapDirectory)
import qualified Mescaline.Meap.Extractor   as Extractor
import Mescaline.Meap.Feature               (Feature(..), Type(..))
import Mescaline.Database.Create            (createDatabase)
import System.Environment                   (getArgs)
import qualified Mescaline.Database         as DB
import Mescaline.Database.Feature           (insertSourceFile)
import Database.HDBC                        (SqlError(..), handleSql, handleSqlError)

options = defaultOptions {
            extractor = Extractor.defaultOptions {
                Extractor.features = [
                      AvgChroma
                    , AvgChromaScalar
                    , AvgChunkPower
                    , AvgFreqSimple
                    , AvgMelSpec
                    , AvgMFCC
                    , AvgSpecCentroid
                    , AvgSpecFlatness
                ]
            }
        }
        
run np dir env = mapDirectory np (insertSourceFile env) options dir

main :: IO ()
main = do
    [np, dir, db] <- getArgs
    createDatabase db
    -- mapDirectory print 4 defaultOptions dir
    handleSql (\e -> print $ seErrorMsg e) (DB.withHandle (run (read np) dir) db)
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
