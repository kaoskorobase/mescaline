module Mescaline.Synth.Database.Process (
    Input(..)
  , Output(..)
  , Handle
  , new
) where

import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Monad
import           Data.Maybe
import qualified Mescaline.Database.Feature as Feature
import qualified Mescaline.Analysis.Meap as Meap
import qualified Mescaline.Synth.Database.Model as Model

data Input =
    Load !FilePath !String
  | Import [FilePath]

data Output = Changed FilePath String

type Handle = Process.Handle Input Output

data State = State {
    path :: !FilePath
  , pattern :: !String
  }

new :: IO Handle
new = spawn $ loop $ State "" "%"
    where
        loop state = do
            x <- recv
            (state', changed) <-
                case x of
                    Load path pattern -> do
                        return (State path pattern, True)
                    Import paths -> do
                        io $ do
                            Model.importPaths (path state) paths
                            Model.transformFeature (path state)
                                Model.PCA
                                (Feature.consDescriptor "es.globero.mescaline.spectral" 2)
                                [fromJust (Meap.lookupFeature "com.meapsoft.AvgMFCC")]
                        return (state, True)
            when changed $ notify $ Changed (path state') (pattern state')
            loop state'
