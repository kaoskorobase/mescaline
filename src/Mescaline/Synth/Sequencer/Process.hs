module Mescaline.Synth.Sequencer.Process (
    Sequencer
  , Input(..)
  , Output(..)
  , new
) where

import           Control.Concurrent.MVar
import           Control.Concurrent.Process
import           Control.Monad
import           Control.Monad.Fix (fix)
import           Control.Monad.Reader
import           Data.Accessor
import           Mescaline (Time)
import qualified Mescaline.Synth.Sequencer.Model as Model
import qualified Sound.OpenSoundControl.Time as Time

data TransportState = Start | Pause | Reset

data Input a =
    ToggleField !Int !Int !a
  | ClearAll
  | Transport   !TransportState
  | QueryModel (MVar (Model.Sequencer a))

data Output a =
    Changed Time (Model.Sequencer a)

type Sequencer a = Handle (Input a) (Output a)

new :: Model.Sequencer a -> IO (Sequencer a)
new s0 = do
    t0 <- liftIO Time.utcr
    spawn $ loop s0 t0
    where
        loop s t = do
            x <- poll
            s' <- case x of
                    Nothing -> return s
                    Just x -> case x of
                        ToggleField r c a ->
                            return $ Model.toggle r c a s
                        ClearAll ->
                            return $ Model.deleteAll s
                        Transport _ ->
                            return s
                        QueryModel mvar -> do
                            liftIO $ putMVar mvar s
                            return s
            let s'' = Model.step (undefined::Model.Score) s'
                t'  = t + getVal Model.tick s''
            notify $ Changed t s''
            liftIO $ Time.pauseThreadUntil t'
            loop s'' t'
