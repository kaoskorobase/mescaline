import Sound.SC3
import qualified Data.ByteString.Lazy as B
import Sound.OpenSoundControl (Datum(..), OSC(..), Time(..), encodeOSC, utcr)

blobFromOSC :: OSC -> Datum
blobFromOSC = Blob . B.unpack . encodeOSC

d_load' :: String -> OSC -> OSC
d_load' p c = Message "/d_load" [String p, blobFromOSC c]

complMess _ = s_new "default" 1001 AddToTail 0 [("freq", 440)]
complBund = flip Bundle [s_new "default" 1001 AddToTail 0 [("freq", 440)],
                         s_new "default" 1002 AddToTail 0 [("freq", 550)],
                         s_new "default" 1003 AddToTail 0 [("freq", 660)]]
compl = complBund

main = do
    t <- (UTCr . (+5)) `fmap` utcr
    withSC3 $ \fd -> do
        send fd $ d_load'
                    "/Users/sk/scwork/synthdefs/default.scsyndef"
                    (compl t)
