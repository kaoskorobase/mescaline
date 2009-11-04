import Mescaline.Database.Create (newDatabase)
import System.Environment (getArgs)
import Database.HDBC (SqlError(..), handleSql, handleSqlError)

main :: IO ()
main = do
    [db] <- getArgs
    newDatabase db
    -- mapDirectory print 4 defaultOptions dir
    -- handleSql (\e -> print $ seErrorMsg e) (DB.withHandle (run (read np) dir) db)
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
