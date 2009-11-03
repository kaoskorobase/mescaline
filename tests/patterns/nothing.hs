pattern $ \db -> (pseq (map pevent (query nothing db)) 1)
