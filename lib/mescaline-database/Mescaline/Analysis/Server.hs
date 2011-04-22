{-# LANGUAGE ScopedTypeVariables #-}
module Mescaline.Analysis.Server
	( Analyser
	, analyser
	) where

import qualified Data.Aeson as A
import qualified Data.Attoparsec as P
import qualified Data.ByteString as B
import qualified Mescaline.Analysis.Types as Analysis
import 			 Mescaline.Analysis.JSON ()
import 			 Network.Curl

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
        "http://api.puesnada.es/api/files"
        [CurlUserPwd (user ++ ":" ++ pwd)]
        [HttpPost "upload[file]"
                  Nothing
                  (ContentFile path)
                  []
                  Nothing]
    case respCurlCode r of
        CurlOK -> case P.parse A.json (respBody r) of
                    P.Done _ value -> return $ Right value
                    P.Fail _ _ e -> return $ Left e
                    _ -> return $ Left "EOF while parsing JSON"
        c -> return $ Left (show c)

data Analyser = Analyser String String

analyser :: String -> String -> Analyser
analyser = Analyser

instance Analysis.Analyser Analyser where
	analyse (Analyser user pass) file = do
	    r <- uploadFile user pass file
	    case r of
	        Left e -> fail e
	        Right json -> do
	            case A.fromJSON json of
					A.Error e -> fail e
					A.Success a -> return a
	fileExtensions _ = [
	    "aif", "aiff", "au", "avi", "avr", "caf", "htk", "iff", "m4a", "m4b", "m4p", "m4v", "mat", "mov", "mp3", 
	    "mp4", "ogg", "paf", "pvf", "raw", "sd2", "sds", "sf", "voc", "w64", "wav", "xi" ]
	
