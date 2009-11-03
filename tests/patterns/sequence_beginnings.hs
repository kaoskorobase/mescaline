pattern $ \db ->
    let us = query everything db
        g a b = Unit.sourceFile a == Unit.sourceFile b
        us' = map (head.tail) $ groupBy g us
    in ptrace (pseq (map pevent us') 1)
