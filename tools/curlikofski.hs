{-# LANGUAGE ScopedTypeVariables #-}
import Network.Curl

import qualified Data.Aeson as A
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Mescaline.Analysis (Analysis)
import Mescaline.Analysis.JSON ()

import           System.Environment (getArgs)

curlMultiPostWithResponse :: URLString -> [CurlOption] -> [HttpPost] -> IO (CurlResponse_ [(String, String)] B.ByteString)
curlMultiPostWithResponse s os ps = initialize >>= \ h -> do
  setopt h (CurlVerbose True)
  setopt h (CurlURL s)
  setopt h (CurlHttpPost ps)
  mapM_ (setopt h) os
  perform_with_response_ h

uploadFile :: String -> String -> FilePath -> IO (Either String A.Value)
uploadFile user pwd path = withCurlDo $ do
    r <- curlMultiPostWithResponse
        "http://192.168.2.103:3000/files"
        [CurlUserPwd (user ++ ":" ++ pwd)]
        [HttpPost "upload[file]"
                  Nothing
                  (ContentFile path)
                  []
                  Nothing]
    case respCurlCode r of
        CurlOK -> case A.parse A.json (respBody r) of
                    A.Done _ value -> return $ Right value
                    A.Fail _ _ e -> return $ Left e
                    _ -> return $ Left "EOF while parsing JSON"
        c -> return $ Left (show c)

main :: IO ()
main = do
    [file] <- getArgs
    r <- uploadFile "sk@xdv.org" "blabla" file
    case r of
        Left e -> fail e
        Right json -> do
            print json
            print (A.fromJSON json :: Maybe Analysis)
                        -- Just (a :: Analysis) -> print a
                        -- Nothing -> putStrLn "Nope"
                        -- :: A-- BL.putStrLn $ A.encode json
