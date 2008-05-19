-- sample code, doesn't necessarily compile
module Main where

import Control.Monad.Trans
import Database.Sqlite.Enumerator
import Database.Enumerator
import System.Environment

import Mescaline.Database.Feature (FeatureDescriptor(..), Feature(..))

import Mescaline.Database.SourceFile (SourceFile(..))
import qualified Mescaline.Database.SourceFile as SourceFile

querySourceFileIteratee :: (Monad m) => Int -> FilePath -> String -> IterAct m [SourceFile]
querySourceFileIteratee a b c accum = result' (SourceFile a b c:accum)

queryFeatureDescIteratee :: (Monad m) => Int -> String -> IterAct m [FeatureDescriptor]
queryFeatureDescIteratee a b accum = result' (FeatureDescriptor a b:accum)

-- non-query actions.
otherActions session = do
    execDDL (sql "create table blah")
    execDML (sql "insert into blah ...")
    commit
    -- Use withTransaction to delimit a transaction.
    -- It will commit at the end, or rollback if an error occurs.
    withTransaction Serialisable $ do
        execDML (sql "update blah ...")
        execDML (sql "insert into blah ...")

main :: IO ()
main = do
    [dbPath] <- getArgs
    withSession (connect dbPath) ( do
    -- simple query, returning reversed list of rows.
    -- select sf.id, sf.path, u.id from source_file sf, unit u left join unit on u.sfid=sf.id;
    sourceFiles <- doQuery (sql "select * from source_file") querySourceFileIteratee []
    featureDescs <- doQuery (sql "select * from feature") queryFeatureDescIteratee []
    liftIO $ putStrLn $ show sourceFiles
    liftIO $ putStrLn $ show featureDescs
    --otherActions session
    )
