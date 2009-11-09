import           Database.HDBC -- (SqlError(..), handleSql, handleSqlError, withTransaction)
import           Mescaline.Database (withDatabase)
import           Mescaline.Database.Model ()
import           Mescaline.Meap.Chain (Options(..), defaultOptions, mapDirectory)
import qualified Mescaline.Meap.Extractor as Extractor
import           Mescaline.Meap.Feature (defaultFeatures)
import           Mescaline.Meap.Import (insertFile)
import           System.Environment (getArgs)

options = defaultOptions {
            extractor = Extractor.defaultOptions {
                Extractor.features = defaultFeatures
            }
        }

doit np dir c = mapDirectory np (insertFile c) options dir

main :: IO ()
main = do
    [np, dir, db] <- getArgs
    -- c <- DB.connectSqlite3 path
    flip withDatabase db $ handleSqlError . doit (read np) dir
    -- DB.disconnect c
    -- mapDirectory print 4 defaultOptions dir
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
