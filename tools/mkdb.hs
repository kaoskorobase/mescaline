import           Database.HDBC -- (SqlError(..), handleSql, handleSqlError, withTransaction)
import           Mescaline.Database (withDatabase)
import           Mescaline.Database.Model ()
import           Mescaline.Meap.Chain (Options(..), defaultOptions, mapDirectory)
import qualified Mescaline.Meap.Extractor as Extractor
import           Mescaline.Meap.Feature (defaultFeatures)
import           Mescaline.Meap.Import (importDirectory)
import           System.Environment (getArgs)

-- doit np dir c = mapDirectory np (Import.insertFile c) Import.options dir
doit = importDirectory

sqlHandler e = putStrLn (seState e ++ ":" ++ show (seNativeError e) ++ ":" ++ seErrorMsg e)
-- sqlHandler _ = putStrLn "FUCK"

main :: IO ()
main = do
    [np, dir, db] <- getArgs
    -- c <- DB.connectSqlite3 path
    handleSqlError (flip withDatabase db (doit (read np) dir))
    -- DB.disconnect c
    -- mapDirectory print 4 defaultOptions dir
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
