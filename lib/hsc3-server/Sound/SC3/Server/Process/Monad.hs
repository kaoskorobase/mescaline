{-# LANGUAGE ScopedTypeVariables #-}
module Sound.SC3.Server.Process.Monad (
    withSynth
  , withInternal
) where

import           Sound.SC3.Server.Process (ServerOptions, OutputHandler, RTOptions)
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.Connection as Conn
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Internal as Process
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import qualified Sound.OpenSoundControl as OSC

withSynth :: OSC.Transport t => (ServerOptions -> RTOptions -> IO t) -> ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynth openTransport serverOptions rtOptions outputHandler action =
    Process.withSynth
        openTransport
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action

withInternal :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withInternal serverOptions rtOptions outputHandler action =
    Process.withInternal
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action
