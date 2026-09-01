# C254 — atomic invoke-ledger snapshot replacement

Date: 2026-08-31

## Repair

The invoke-jobs ledger remains the same single EDN map. Its commit protocol is
now:

1. acquire one process-wide writer lock covering the in-memory transition and
   durable commit;
2. render into a uniquely named temporary sibling in the ledger directory;
3. write all bytes and `FileChannel.force(true)` the temporary file;
4. atomically rename it over the authority with `ATOMIC_MOVE` and
   `REPLACE_EXISTING`;
5. open and force the parent directory so the rename itself reaches stable
   storage;
6. return success only after those steps complete.

Historical statement (superseded by C263): if any step failed, lifecycle
mutation helpers restored the prior in-memory ledger and active index before
propagating the exception.

2026-09-01 amendment (C263): failure before rename remains
`:committed? false` and rolls memory back. Once atomic rename succeeds, the new
snapshot is authoritative; failure to force the parent directory is reported
as `{:committed? true :durability :unconfirmed}` and propagated to the caller,
but memory remains equal to the new disk snapshot. Rolling back after rename
created a false memory/disk split.

The same lock covers stream-event atom changes. Although those events remain
intentionally non-durable until finalization, they can no longer interleave
with a failed durable mutation's rollback or let two durable snapshots reach
disk out of atom order.

## Loud loading

An existing ledger is read as exactly one EDN form. Empty input, truncated or
malformed EDN, a non-map root, trailing forms, missing required schema keys,
wrong required value types, an unsupported version, or an empty `:jobs` map throw
`invoke-jobs ledger is unreadable; refusing empty fallback`. Only an actually
absent file receives the fresh default ledger. Corruption can no longer be
coerced to an apparently valid empty history.

The fresh default is held in memory but is not written as an empty authority;
the first real job mutation creates the file. Thus “no file yet” remains
distinguishable from “an existing file claims an empty history.”

## Falsifiers

`futon3c.transport.invoke-ledger-atomicity-test` uses isolated temporary
directories and covers:

- a fault after the new temporary file is forced but before rename: the old
  ledger remains byte-readable and no temporary file is leaked;
- a truncated authority: load throws rather than returning the default;
- a replacement failure: the mutation caller receives the exception and the
  in-memory ledger rolls back.

Canonical focused invocations:

```sh
clojure -M:test -n futon3c.transport.invoke-ledger-atomicity-test
clojure -M:test -n futon3c.transport.active-invoke-index-test
clj-kondo --lint src/futon3c/transport/http.clj \
  test/futon3c/transport/invoke_ledger_atomicity_test.clj
emacs --batch -Q -l ../futon4/dev/check-parens.el \
  src/futon3c/transport/http.clj \
  test/futon3c/transport/invoke_ledger_atomicity_test.clj
```

Results: atomicity 3 tests / 6 assertions, index 1 test / 4 assertions;
all exit 0. clj-kondo and check-parens exit 0.

## Explicitly skipped

- No repository-wide or full suite gate; C241 owns that run.
- No live-ledger fault injection or rewrite during verification.
- No backend migration, format/version change, pruning, compaction, or async
  publication.

The 134.6 MB full-map cost remains. This delivery repairs atomicity and error
semantics, not scaling.
