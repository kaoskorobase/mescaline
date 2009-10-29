module Sound.SC3.Server.Command.Completion where

import           Sound.OpenSoundControl
import qualified Data.ByteString.Lazy as B

-- Encode an OSC packet as an OSC blob.
encode_osc_blob :: OSC -> Datum
encode_osc_blob = Blob . B.unpack . encodeOSC

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc' :: OSC -> Int -> Int -> Int -> OSC
b_alloc' osc nid frames channels = Message "/b_alloc" [Int nid, Int frames, Int channels, encode_osc_blob osc]

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read' :: OSC -> Int -> String -> Int -> Int -> Int -> Int -> OSC
b_read' osc nid p f n f' z = Message "/b_read" [Int nid, String p, Int f, Int n, Int f', Int z, encode_osc_blob osc]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead' :: OSC -> Int -> String -> Int -> Int -> OSC
b_allocRead' osc nid p f n = Message "/b_allocRead" [Int nid, String p, Int f, Int n, encode_osc_blob osc]
