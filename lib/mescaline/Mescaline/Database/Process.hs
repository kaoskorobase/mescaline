{-# LANGUAGE CPP #-}
module Mescaline.Database.Process (
    Input(..)
  , Output(..)
  , Handle
  , new
) where

import           Control.Concurrent.Process hiding (Handle)
import qualified Control.Concurrent.Process as Process
import           Control.Monad
#if USE_ANALYSIS
import qualified Mescaline.Analysis as Analysis
#endif
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
#if USE_ANALYSIS
                        io $ do
                            Analysis.importPathsDefault Nothing (path state) paths
                            DB.withDatabase (path state) $
                                DB.transformFeature
                                    (DB.PCA 2)
                                    "es.globero.mescaline.spectral"
                                    ["http://vamp-plugins.org/rdf/plugins/qm-vamp-plugins#qm-mfcc"]
                        return (state, True)
#else
						return (state, False)
#endif -- USE_ANALYSIS
            when changed $ notify $ Changed (path state') (pattern state')
            loop state'
