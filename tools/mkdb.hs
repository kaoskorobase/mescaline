import           Database.HDBC -- (SqlError(..), handleSql, handleSqlError, withTransaction)
import qualified Mescaline.Database as DB
import           Mescaline.Database.Model ()
import           Mescaline.Meap.Import (importDirectory)
import           System.Environment (getArgs)

-- sqlHandler e = putStrLn (seState e ++ ":" ++ show (seNativeError e) ++ ":" ++ seErrorMsg e)
-- sqlHandler _ = putStrLn "FUCK"

main :: IO ()
main = do
    [np, dir, db] <- getArgs
    handleSqlError $ DB.withDatabase db $ importDirectory (read np) dir
