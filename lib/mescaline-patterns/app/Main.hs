import           Control.Exception (displayException)
import           Control.Lens
import           Data.Default (def)
import           Mescaline.Clock (Time(..))
import qualified Mescaline.Pattern.Event as E
import qualified Mescaline.Pattern.Interpreter as Interp
import qualified Mescaline.Pattern.Player as Player
import           Mescaline.Quant (Quant(..))
import qualified System.FSNotify as FSN
import           Reactive.Banana hiding (interpret)
import           Reactive.Banana.Frameworks
import           Sound.OSC (Bundle(..), Message(..), RecvOSC, string)
import qualified Sound.OSC as OSC
import qualified Sound.OSC.Transport.FD as FD
import           System.FilePath
import           Text.Read (readMaybe)

fsEventExistingPath :: FSN.Event -> Maybe FilePath
fsEventExistingPath event =
  case event of
    FSN.Added p _ -> Just p
    FSN.Modified p _ -> Just p
    FSN.Removed _ _ -> Nothing

compileFile :: Player.Process -> FilePath -> IO ()
compileFile player path = do
  case readMaybe (dropExtension . takeFileName $ path) of
    Nothing -> return ()
    Just slot -> do
      code <- readFile path
      r <- Interp.compilePattern code
      case r of
        Left err -> putStrLn $ displayException err
        Right p -> do
          putStrLn $ "New pattern for slot " ++ show slot
          Player.setSlot player slot def { quant = 4 } (Just p)

sourceFiles :: [FilePath]
sourceFiles = [show x ++ ".hs" | x <- [1..4::Int]]

isSourceFile :: FSN.Event -> Bool
isSourceFile (FSN.Added path _) = takeFileName path `elem` sourceFiles
isSourceFile _ = False

watchDir :: FSN.WatchManager -> FilePath -> FSN.ActionPredicate -> MomentIO (Event FSN.Event)
watchDir mgr dir predicate = fromAddHandler $ AddHandler (FSN.watchDir mgr dir predicate)
 
serverLoop :: (MonadIO m, RecvOSC m) => (Message -> IO ()) -> m ()
serverLoop snk = do
  m <- OSC.recvMessage
  case m of
    Nothing -> serverLoop snk
    Just msg -> do
      liftIO $ snk msg
      case messageAddress msg of
        "/quit" -> return ()
        _ -> serverLoop snk

dirty :: FD.Transport t => t -> Mescaline.Clock.Time -> E.Event -> IO ()
dirty dirt time evt =
  let msg = do {
    sound <- evt ^. E.field "sound" ;
    return $ Bundle (realToFrac . seconds $ time) [Message "/play2" [string "sound", string sound]] }
  in maybe (return ()) (FD.sendBundle dirt) msg
 
main :: IO ()
main = do
  (oscMsgSource, oscMsgSink) <- newAddHandler

  player <- Player.start

  FD.withTransport (OSC.openUDP "127.0.0.1" 57120) $ \dirt -> do
  -- localhost 57120 /play2 ss sound sd:6
    FSN.withManager $ \mgr -> do
      network <- compile $ do
        fsEvents <- watchDir mgr "patterns" isSourceFile
        oscMsg <- fromAddHandler oscMsgSource
        playerEvents <- Player.events player
        reactimate $ print <$> oscMsg
        reactimate $ compileFile player <$> filterJust (fsEventExistingPath <$> fsEvents)
        reactimate $ uncurry (dirty dirt) <$> playerEvents
      actuate network

      putStrLn "Starting the fabulous Mescaline pattern machine on 127.0.0.1:3000"

      OSC.withTransport (OSC.udpServer "127.0.0.1" 3000) $
        serverLoop oscMsgSink

