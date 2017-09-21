import Data.List
import Language.Haskell.Interpreter
import qualified Mescaline.Pattern as P
import qualified System.FSNotify as FSN
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Reactive.Banana hiding (interpret)
import Reactive.Banana.Frameworks
import Sound.OSC
import System.FilePath

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

compilePattern :: String -> IO (Either InterpreterError (P.P Int))
compilePattern code =
  runInterpreter $ do
    setImportsQ [("Prelude", Nothing), ("Mescaline.Pattern", Nothing)]
    interpret code (undefined :: P.P Int)

compileFile event = do
  let path = case event of
                FSN.Added p _ -> Just p
                FSN.Modified p _ -> Just p
                FSN.Removed _ _ -> Nothing
  case path of
    Nothing -> return ()
    Just path -> do
      code <- readFile path
      r <- compilePattern code
      case r of
        Left err -> putStrLn $ errorString err
        Right p -> print $ P.unP p

sourceFiles = [show x ++ ".hs" | x <- [1..4]]
isSourceFile (FSN.Added path _) = takeFileName path `elem` sourceFiles
isSourceFile _ = False

watchDir :: FSN.WatchManager -> FilePath -> FSN.ActionPredicate -> MomentIO (Event FSN.Event)
watchDir mgr dir predicate = fromAddHandler $ AddHandler (FSN.watchDir mgr dir predicate)
 
serverLoop :: (MonadIO m, RecvOSC m) => (Message -> IO ()) -> m ()
serverLoop snk = do
  m <- recvMessage
  case m of
    Nothing -> serverLoop snk
    Just msg -> do
      liftIO $ snk msg
      case messageAddress msg of
        "/quit" -> return ()
        _ -> serverLoop snk

main :: IO ()
main = do
  (oscMsgSource, oscMsgSink) <- newAddHandler

  FSN.withManager $ \mgr -> do
    network <- compile $ do
      fsEvents <- watchDir mgr "patterns" isSourceFile
      oscMsg <- fromAddHandler oscMsgSource
      reactimate $ print <$> oscMsg
      reactimate $ compileFile <$> fsEvents
    actuate network

    withTransport (udpServer "127.0.0.1" 3000) $
      serverLoop oscMsgSink

