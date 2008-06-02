-- sample code, doesn't necessarily compile
module Main where

import Control.Monad.Trans
import Database.Sqlite.Enumerator
import Database.Enumerator
import System.Environment
import Data.Binary


import Mescaline.Database.Feature (FeatureDescriptor(..), Feature(..))

import Mescaline.Database.SourceFile (SourceFile(..))
import qualified Mescaline.Database.SourceFile as SourceFile


querySourceFileIteratee :: (Monad m) => Int -> FilePath -> String -> IterAct m [SourceFile]
querySourceFileIteratee a b c accum = result' (SourceFile a b c:accum)

queryFeatureDescIteratee :: (Monad m) => Int -> String -> IterAct m [FeatureDescriptor]
queryFeatureDescIteratee a b accum = result' (FeatureDescriptor a b:accum)


fetchDetails sfid fid = do
    let
        iter :: (Monad m) => Int -> Int -> Float -> Float -> Int -> Float -> Maybe String -> Maybe String -> IterAct m [(Int, Int, Float, Float, Int, Float, Maybe String, Maybe String)]
        iter a b c d e f g h acc = result $ (a, b, c, d, e, f, g, h):acc
        bindVals = [bindP (sfid::Int), bindP(fid::Int)]
        query = prefetch 1000 "select sf.id, u.id, u.onset_time, u.chunk_length, uf.intval, uf.realval, uf.textval, uf.arrayval from source_file sf, unit u, unit_feature uf where sf.id=u.sfid and sf.id = ? and uf.unit_id=u.id and uf.feature_id=? " bindVals
    actual <- doQuery query iter []
    liftIO (print actual)

test = do
	putStrLn "Please enter your name: "
	name <- getLine
	liftIO (print name)
	

main :: IO ()
main = do
    [dbPath] <- getArgs
    putStrLn "Please enter feature id [1..7]: "
    fid <- getLine >>= return . read
    withSession (connect dbPath) ( do
        sourceFiles <- doQuery (sql "select * from source_file") querySourceFileIteratee []
        featureDescs <- doQuery (sql "select * from feature") queryFeatureDescIteratee [] 
        mapM_ (flip fetchDetails fid . SourceFile.id) sourceFiles)
