module Mescaline.Sampler.Keyboard where

import           Control.Concurrent.Chan
import		 Control.Monad.Trans (liftIO)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Graphics.UI.Gtk.Gdk.EventM

-- import           Graphics.UI.Gtk (Event)
-- import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.UI.Gtk.Gdk.Keys   as G
-- import qualified Graphics.UI.Gtk            as G

mkKeyMap :: [(String, EventM EKey Bool)] -> EventM EKey Bool
mkKeyMap keys = do
        m <- flip Map.lookup keyMap `fmap` eventKeyVal
        case m of
            Nothing -> return False
            Just a  -> a
    where keyMap = Map.fromList (map (\(kn, m) -> (G.keyFromName kn, m)) keys)

suppressRepeatedKeys :: EventM EKey Bool -> IO (EventM EKey Bool)
suppressRepeatedKeys a = do
    prevEventRef <- newIORef Nothing
    return $ do
        prevEvent <- liftIO $ readIORef prevEventRef
        case prevEvent of
            Nothing -> do
                val  <- eventKeyVal
                mods <- eventModifierAll
                write prevEventRef (val, mods)
                a
            Just (val, mods) -> do
                val'  <- eventKeyVal
                mods' <- eventModifierAll
                if val == val' && mods == mods'
                    then return False
                    else do
                        write prevEventRef (val', mods')
                        a
    where
        write r x = liftIO $ writeIORef r $ Just x
 
