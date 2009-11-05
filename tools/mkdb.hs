-- import qualified Mescaline.Database.Feature (insertSourceFile)
import Mescaline.Meap.Chain                 (Options(..), defaultOptions, mapDirectory)
import qualified Mescaline.Meap.Extractor   as Extractor
import Mescaline.Meap.Feature               (defaultFeatures)
import Mescaline.Database.Create            (createDatabase)
import System.Environment                   (getArgs)
import Database.HDBC                        -- (SqlError(..), handleSql, handleSqlError, withTransaction)

options = defaultOptions {
            extractor = Extractor.defaultOptions {
                Extractor.features = defaultFeatures
            }
        }

run np dir env = mapDirectory np (insertSourceFile env) options dir

main :: IO ()
main = do
    [np, dir, db] <- getArgs
    c <- DB.connectSqlite3 path
    handleSql (\e -> print $ seErrorMsg e) (DB.withHandle (run (read np) dir) db)
    DB.disconnect c
    -- mapDirectory print 4 defaultOptions dir
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
