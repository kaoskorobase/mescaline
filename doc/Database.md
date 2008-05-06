# Database

### Tables

### SourceFile

    *id       integer
    path      varchar(256)
    hash      varchar(40)

### Unit

    *id           integer
    sfid          integer
    start_seconds float8
    end_seconds   float8

### Feature

    *id           integer
    name          varchar(32)
    ...

### UnitFeature

    *uid          integer
    fid           integer
    intval        int4
    realval       float8
    textval       text
    arrayval      float8 array[1024]

## SQLite specifica

* [sqlite][]
* [sqlite FAQ][sqlite-faq]

[sqlite]: http://www.sqlite.org/
[sqlite-faq]: http://www.sqlite.org/faq.html
