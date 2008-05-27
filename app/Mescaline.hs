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


fetchDetails sfid = do
  let
    iter :: (Monad m) => Int -> Int -> Float -> Float -> IterAct m [(Int, Int, Float, Float)]
    iter a b c d  acc = result $ (a, b, c, d):acc
    bindVals = [bindP (sfid::Int), bindP(2::Int)]
	--select sf.id, sf.path, u.id, u.onset_time, u.chunk_length, uf.* from source_file sf, unit u, unit_feature uf left join unit on u.sfid=sf.id left join unit_feature on uf.unit_id=u.id where sf.id = ? and uf.feature_id = ?
    query = prefetch 1000 "select sf.id, u.id, u.onset_time, u.chunk_length, uf.* from source_file sf, unit u, unit_feature uf where sf.id=u.sfid and sf.id = ? and uf.unit_id=u.id and uf.feature_id=? " bindVals
  actual <- doQuery query iter []
  liftIO (print actual)

test = do
	putStrLn "Please enter your name: "
	name <- getLine
	liftIO (print name)
	

main :: IO ()
main = do
    [dbPath] <- getArgs
    withSession (connect dbPath) ( do
    -- simple query, returning reversed list of rows.
    -- select sf.id, sf.path, u.id from source_file sf, unit u left join unit on u.sfid=sf.id;
    sourceFiles <- doQuery (sql "select * from source_file") querySourceFileIteratee []
    featureDescs <- doQuery (sql "select * from feature") queryFeatureDescIteratee [] 
    mapM_ (fetchDetails . SourceFile.id) sourceFiles
    --liftIO $ putStrLn $ show featureDescs
    --fetchDetails 5
	-- bindShortcutExample 
    )
