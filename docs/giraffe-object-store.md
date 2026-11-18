# Giraffe object storage

Giraffe stores canonical EDN object bytes in `ROOT/objects.sqlite`, table
`objects(sha TEXT PRIMARY KEY, body BLOB NOT NULL)`. The SHA and run index
formats are unchanged. Writers use WAL, a 30-second SQLite busy timeout,
insert-or-ignore, and one transaction per run's object DAG. Independent JVMs
can write safely. SQLite may also maintain `-wal` and `-shm` sidecars.

Readers use the database when present. Only an absent database permits legacy
`objects/<prefix>/<sha>.edn` reads: a missing row cannot fall back to stale files.
Python connections use `mode=ro`. The turn-frame reader supports the same database
layout for dispatch payloads, but dispatch payload storage is separate and its
writer is unchanged by this migration.

## Migration

From the futon3c repository, inspect first:

```sh
clojure -M -m futon3c.giraffe.object-migration "$HOME/storage/giraffe"
```

Then explicitly copy, verify and remove legacy objects:

```sh
clojure -M -m futon3c.giraffe.object-migration "$HOME/storage/giraffe" --write
```

Stop legacy file writers before migrating. Deploy/reload the SQLite writer only
after migration; it refuses a first write over an unmigrated file store. Existing
SQLite writers can coexist with reruns (subject to the busy timeout).

The dry run reports object count, bytes and digest mismatches without writing.
Any source digest mismatch refuses migration. A new database is built in a private
staging directory and published only after every copied row verifies. An existing
database is extended transactionally; corrupt existing rows refuse the copy.
Before deleting any files, the migration runs `verify-run` on up to three sealed
CAS-indexed runs, then rechecks every source digest. Failure retains the files;
a rerun verifies them again before deletion. Indexes and manifests are never
rewritten. A store containing no indexed runs reports an empty sample list.
An interrupted failed staging copy may leave a `.objects-pack-*` directory;
that directory is never authoritative. Do not remove an active migration's staging
files. Completed migration removes its staging directory and empty legacy directories.

No live migration is performed by tests or by loading these namespaces.
