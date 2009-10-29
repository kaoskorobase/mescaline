module Mescaline.Synth.Pattern where

import qualified Sound.SC3.Lang.Pattern as P

-- Segment set combinators
-- * select segments from sound file based on time, features
-- * construct linear sequences (for time based manipulation) or sets (for feature based manipulation)

data SoundFile = SoundFile {
    sf_path :: FilePath,
    sf_info :: ()
}

data Unit = Unit {
    u_file      :: SoundFile,
    u_onset     :: Double,
    u_dur       :: Double,
    u_features  :: [(String, Int)],
    u_values    :: [[Double]]
}

type PUnit = P.P Unit

p_file :: String -> PUnit
p_file = undefined
