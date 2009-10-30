module Mescaline.Database.SoundFile (
    getInfo
  , module Sound.File.Sndfile
) where

import Sound.File.Sndfile
import Sound.File.Sndfile as SF

getInfo :: FilePath -> IO SF.Info
getInfo path = do
    h <- SF.openFile path SF.ReadMode SF.defaultInfo
    SF.hClose h
    return (SF.hInfo h)
