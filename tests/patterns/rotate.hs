pattern $ \db ->
    -- let f = "*Rub A Dub*" in
    let f = "*100*" in
        pseq (map pevent $ concat $ map (rotate 3) $ clump 4 $ query (fileNameMatch f) db) 100
