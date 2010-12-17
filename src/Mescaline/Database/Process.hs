module Mescaline.Database.Process (
    Input(..)
  , Output(..)
  , Handle
  , new
) where

import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Monad
import qualified Mescaline.Analysis as Analysis
import qualified Mescaline.Database as DB

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
                            Analysis.importPathsDefault Nothing (path state) paths
                            DB.withDatabase (path state) $
                                DB.transformFeature
                                    (DB.PCA 2)
                                    "es.globero.mescaline.spectral"
                                    ["http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc"]
                        return (state, True)
            when changed $ notify $ Changed (path state') (pattern state')
            loop state'
