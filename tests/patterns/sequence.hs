pattern $ \db ->
    pseq (fmap pevent (query (fileNameMatch "*") db)) 1
