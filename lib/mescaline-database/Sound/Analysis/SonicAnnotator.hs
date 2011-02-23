{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Sound.Analysis.SonicAnnotator (
	Exception(..)
  , Feature(..)
  , Analysis
  , analyse
) where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Char (isDigit, isSpace)
import           Data.Enumerator (($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.IO as E
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Ord
import           Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed as V
-- import           Mescaline.Analysis.Types
import qualified Sound.Analysis.Segmentation as S
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Util as SF
import           System.Exit
import           System.FilePath
import qualified System.IO as IO
import           System.Process
import           Text.XML.Enumerator.Parse hiding (parseFile)
import           Data.Text.Lazy (Text, toStrict, unpack)
import qualified Data.Text as T

data Exception =
    ParseError String
  | AnalysisError String
    deriving (Show, Typeable)

instance E.Exception Exception

data Feature = Feature {
    name :: String
  , timeStamp :: Double
  , values :: Maybe [Double]
  } deriving (Eq, Read, Show)

parseTimeStamp :: P.Parser Double
parseTimeStamp = do
    P.many (P.skip isSpace)
    x <- P.takeWhile1 (/= 'R')
    return (read (T.unpack x))

parseValue :: P.Parser Double
parseValue = do
    P.char ':'
    x <- P.takeWhile1 (\x -> isDigit x || x == '.' || x == '-' || x == 'e')
    return (read (T.unpack x))

parseVector :: P.Parser [Double]
parseVector = parseValue `P.sepBy` (P.many1 (P.char ' '))

parse :: (Monad m) => P.Parser a -> Text -> E.Iteratee a1 m a
parse p t = do
    let e = P.eitherResult $ flip P.feed T.empty $ P.parse p $ toStrict t
    case e of
        Left err -> E.throwError (ParseError err)
        Right x -> return x

parseFeature :: E.Iteratee SEvent IO (Maybe Feature)
parseFeature = tag'' "feature" $ do
    name <- force "name required" $ tag'' "name" $ liftM unpack content'
    timeStamp <- force "timestamp required" $ tag'' "timestamp" $ parse parseTimeStamp =<< content'
    values <- tag'' "values" $ do
        t <- content
        case t of
            Nothing -> return []
            Just t -> parse parseVector t
    return $ Feature name timeStamp values

parseFeatureMap :: E.Iteratee SEvent IO (Map.Map String [Feature])
parseFeatureMap = loop Map.empty
    where
        loop fs = do
            x <- parseFeature
            case x of
                Nothing -> return fs
                Just f -> loop $ Map.insertWith' (++) (name f) [f] fs

parseHandle :: Integer -> IO.Handle -> (Text -> Maybe Text) -> E.Iteratee SEvent IO a -> IO (Either E.SomeException a)
parseHandle bs h re p =
    E.run $ E.enumHandle bs h $$ E.joinI
          $ parseBytes        $$ E.joinI
          $ simplify re       $$ p

type Analysis = Map.Map String (S.Segmentation Double (Maybe [Double]))

analyse :: FilePath -> FilePath -> IO Analysis
analyse transforms file = do
    (_, hOut, hErr, h) <-
        runInteractiveProcess
            "sonic-annotator"
            [ "-t", transforms, "-w", "default", file ]
            Nothing
            Nothing
    out <- newEmptyMVar
    err <- newEmptyMVar
    forkIO $ parseHandle 4096 hOut (const Nothing) parseFeatureMap >>= putMVar out
    forkIO $ IO.hGetContents hErr >>= putMVar err
    exit <- waitForProcess h
    case exit of
        ExitSuccess -> do
            res <- readMVar out
            case res of
                Left err -> E.throwIO err
                Right x -> do
                    info <- SF.getInfo file
                    return $ flip fmap x $
                        S.fromEvents (SF.duration info)
                      . map (\f -> (timeStamp f, values f))
                      . L.sortBy (comparing timeStamp)
        ExitFailure _ -> readMVar err >>= E.throwIO . AnalysisError
