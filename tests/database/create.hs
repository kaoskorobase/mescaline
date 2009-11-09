import qualified Mescaline.Database.SourceFile as SourceFile
import Mescaline.Database.Model
import System.Environment (getArgs)
import Database.HDBC (SqlError(..), handleSql, handleSqlError)

io c = do
    createTable c (sqlTable (undefined::SourceFile.SourceFile))
    
main :: IO ()
main = do
    [db] <- getArgs
    withDatabase io db
    -- mapDirectory print 4 defaultOptions dir
    -- handleSql (\e -> print $ seErrorMsg e) (DB.withHandle (run (read np) dir) db)
    -- handleSqlError (DB.withHandle (\e -> DB.getSourceFiles Nothing e >>= print) db)
