{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Exception as E
import           Data.Aeson as J
import qualified Data.Attoparsec.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Text
import           Mescaline.Analysis (analyse)
import           Mescaline.Analysis.JSON ()
import           Mescaline.Analysis.SonicAnnotator (defaultAnalyser)
import           System.Environment (getArgs)
import           System.Exit (ExitCode(..), exitWith)

mkError s = object [ "error" .= Text.pack s ]

data Params = Params { path :: String }

instance FromJSON Params where
    parseJSON (Object v) = Params <$> (v .: "path")
    parseJSON _ = empty

analyseit b = do
    case A.parse J.json b of
        A.Fail _ _ e -> return $ mkError $ "Invalid input: " ++ e
        A.Done _ value -> do
            case fromJSON value of
                J.Error e -> return $ mkError $ "Invalid input: " ++ e
                J.Success params -> E.catch (J.toJSON `fmap` analyse defaultAnalyser (path params))
                                        $ \(e :: SomeException) -> return $ mkError (show e)

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
                [] -> B.getContents >>= analyseit
                [path] -> B.readFile path >>= analyseit
                _ -> putStrLn "Usage: mescaline-analyser [FILE]" >> exitWith (ExitFailure 1)
    B.putStrLn (J.encode result)
