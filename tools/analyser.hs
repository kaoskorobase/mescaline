{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Exception as E
import           Data.Aeson as J
import qualified Data.Attoparsec as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Text
import           Mescaline.Analysis (analyse)
import           Mescaline.Analysis.JSON ()
import           Mescaline.Analysis.SonicAnnotator (analyser)

mkError s = object [ "error" .= Text.pack s ]

data Params = Params { path :: String }

instance FromJSON Params where
    fromJSON (Object v) = Params <$> (Text.unpack `fmap` (v .: "path"))
    fromJSON _ = empty

analyseit = do
    result <- A.parse J.json `fmap` BS.getContents
    case result of
        A.Fail _ _ e -> return $ mkError $ "Invalid input: " ++ e
        A.Partial _ -> return $ mkError $ "Invalid input: EOF while parsing JSON"
        A.Done _ value -> do
            case fromJSON value of
                Nothing -> return $ mkError "Invalid input: expected field \"path\""
                Just params -> E.catch (J.toJSON `fmap` analyse analyser (path params))
                                $ \(e :: SomeException) -> return $ mkError (show e)

main :: IO ()
main = B.putStrLn . J.encode =<< analyseit
