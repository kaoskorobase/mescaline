pattern $ \db ->
    let us     = query (fileNameMatch "*Track 03*") db
        us1    = concat $ replicate 2 $ take 64 $ drop 4 $ reverse us
        us2    = take 16 $ drop 8 us
        p      = (flip pseq 1 . fmap pevent) (cycle $ us1 ++ us2)
        setEnv = setVal (synth.>attackTime) 0.001 . setVal (synth.>releaseTime) 0.05
    in fmap setEnv p
