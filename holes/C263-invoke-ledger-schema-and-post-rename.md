# C263 — invoke-ledger schema boundary and post-rename semantics

Date: 2026-09-01

## Result

The C254 repair had moved two boundaries without closing them:

1. EDN syntax was validated, but schema-incomplete maps were merged with a
   default and could become an apparently fresh empty ledger.
2. A failure after atomic rename rolled memory back even though disk already
   held the new authoritative snapshot.

Existing ledger files now require the complete version-1 shape: integer
`:version` and `:next-seq`, vector `:job-order`, map `:trace->job`, and nonempty
map `:jobs`. Missing keys, wrong types, unsupported versions, and empty job
history fail loudly through the existing unreadable-ledger boundary. The
authority is not rewritten. A genuinely absent file still creates a fresh
in-memory default, but that empty default is not persisted until the first job
mutation. Absence and an existing empty claim are therefore distinct.

Persistence errors now carry commit phase:

- before rename: `{:committed? false :durability :not-committed}`; mutation
  memory and the active index roll back;
- after rename: `{:committed? true :durability :unconfirmed}`; the caller is
  informed by an exception, while memory and the active index retain the new
  disk-authoritative state.

The latter is a durability warning, not a claim that the write did not happen.
No format, backend, pruning, or live ledger changed.

## Controls and focused verification

Canonical invocations:

```sh
clojure -M:test -n futon3c.transport.invoke-ledger-atomicity-test
clojure -M:test -n futon3c.transport.active-invoke-index-test
clj-kondo --lint src/futon3c/transport/http.clj \
  test/futon3c/transport/invoke_ledger_atomicity_test.clj
emacs --batch -Q -l ../futon4/dev/check-parens.el \
  src/futon3c/transport/http.clj \
  test/futon3c/transport/invoke_ledger_atomicity_test.clj
```

Results:

- atomicity: 6 tests / 21 assertions, exit 0;
- active index: 1 test / 4 assertions, exit 0;
- clj-kondo: 0 errors / 0 warnings, exit 0;
- check-parens: exit 0.

The schema control covers `{}`, `{:version 1}`, `{:jobs {}}`, and a complete
but empty version-1 shape; each fails and remains byte-for-byte unchanged. The
post-rename control injects a failure immediately after rename, observes the
committed/unconfirmed exception, and proves in-memory ledger equals disk.

Per packet scope, no full suite, workspace gate, live-ledger load, or live
ledger write was run.
