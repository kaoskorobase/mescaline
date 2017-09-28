import           Control.Concurrent (threadDelay)
import           Control.Lens
import           Control.Monad (forever)
import           Data.Default (def)
import           Data.List
import           Language.Haskell.Interpreter as Interp
import qualified Mescaline.Pattern as P
import qualified Mescaline.Pattern.Event as E
import qualified Mescaline.Pattern.Player as Player
import qualified System.FSNotify as FSN
import           Reactive.Banana hiding (interpret)
import           Reactive.Banana.Frameworks
import           Sound.OSC
import qualified Sound.OSC.Transport.FD as FD
import           System.FilePath

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

compilePattern :: String -> IO (Either InterpreterError (P.Pattern P.Event))
compilePattern code =
  runInterpreter $ do
    Interp.set [ languageExtensions := [ OverloadedLists, OverloadedStrings ] ]
    setImportsQ [("Prelude", Nothing), ("Mescaline.Pattern", Nothing)]
    interpret code (undefined :: P.Pattern P.Event)

compileFile player event = do
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
        Right p -> Player.assign player 0 def (Just p)

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

dirty dirt time evt =
  let msg = do {
    snd <- E.fromField =<< evt ^. E.fields . at "sound" ;
    return $ Message "/play2" [string "sound", string snd] }
  in maybe (return ()) (FD.sendMessage dirt) msg
 
main :: IO ()
main = do
  (oscMsgSource, oscMsgSink) <- newAddHandler

  player <- Player.start

  FD.withTransport (openUDP "127.0.0.1" 57120) $ \dirt -> do
  -- localhost 57120 /play2 ss sound sd:6
    FSN.withManager $ \mgr -> do
      network <- compile $ do
        fsEvents <- watchDir mgr "patterns" isSourceFile
        oscMsg <- fromAddHandler oscMsgSource
        playerEvents <- Player.events player
        reactimate $ print <$> oscMsg
        reactimate $ compileFile player <$> fsEvents
        reactimate $ uncurry (dirty dirt) <$> playerEvents
      actuate network

      putStrLn "Starting the fabulous Mescaline pattern machine on 127.0.0.1:3000"

      withTransport (udpServer "127.0.0.1" 3000) $
        serverLoop oscMsgSink

