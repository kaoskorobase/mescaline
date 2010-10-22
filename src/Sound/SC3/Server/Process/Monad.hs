{-# LANGUAGE ScopedTypeVariables #-}
module Sound.SC3.Server.Process.Monad (
    withSynthUDP
) where

import           Sound.SC3.Server.Process (ServerOptions, OutputHandler, RTOptions)
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.Connection as Conn
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import qualified Sound.OpenSoundControl as OSC

withSynthUDP :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynthUDP serverOptions rtOptions outputHandler action =
    Process.withSynth
        serverOptions
        rtOptions
        outputHandler
        $ \(t :: OSC.UDP) ->
            Conn.new (State.new serverOptions) t >>= Server.runServer action
