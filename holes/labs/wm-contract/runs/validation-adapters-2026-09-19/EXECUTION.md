# Validation incident adapters — slice 2 execution

Final source commit: `b85ac9ce87c6c5e0fb1b0ad915ecb5243c1fa8b6`

## Pins

- adapter: `4a9699d8166443de86ae51214a73745f570aa9992df96daf00509122bff93733`
- tests: `ae5e8f57a77631125401bb02695c3ab325e784855b170ff26848484806de647b`
- live Agency endpoint shape: `GET /api/alpha/invoke/jobs?limit=N` returns
  `{:ok true :count N :jobs [...]}` (`transport/http.clj:6222-6232`).
- retained trip shape was read from the actual Futon2 trip ledger: schema 1
  records carry `:trip/recorded-at`, `:trip/wire-id`, and `:trip/witness`.

## Attempt trail

The first focused run against the complete live trip directory was stopped
with exit 130 before assertions. Discovery: 293 reports occupy 6.3 GB, and the
initial reader parsed every historical report before applying cursor state.
Source/harness commit `b85ac9ce` changed the reader to exclude already-seen
canonical paths before parsing. Its live-pin control copies the byte-exact
smallest real retained report into an isolated temporary directory.

## Static gates after the repair

```text
$ clj-kondo --lint src/futon3c/test_registry/validation_adapters.clj test/futon3c/test_registry/validation_adapters_test.clj
linting took 16ms, errors: 0, warnings: 0

$ emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- --no-defaults src/futon3c/test_registry/validation_adapters.clj test/futon3c/test_registry/validation_adapters_test.clj
OK

$ git diff --check -- src/futon3c/test_registry/validation_adapters.clj test/futon3c/test_registry/validation_adapters_test.clj
[exit 0, no output]
```

## Focused fresh-JVM run after the source/harness repair

```text
$ clojure -X:test :nses '[futon3c.test-registry.validation-adapters-test]'
WARNING: Unknown module: org.apache.arrow.memory.core specified to --add-opens

Running tests in #{"test"}

Testing futon3c.test-registry.validation-adapters-test

Ran 2 tests containing 11 assertions.
0 failures, 0 errors.
[exit 0]
```

The test read the live Agency API and a byte-exact real trip report. It used
temporary queue/cursor ledgers, confirmed both default subject forms, and
confirmed unchanged second sweeps enqueue zero incidents.

## Read-only-source CLI sweep into temporary ledgers

```text
$ FUTON3C_REVALIDATION_QUEUE=<temp>/queue.ednlog \
  FUTON3C_VALIDATION_CURSOR=<temp>/cursor.ednlog \
  FUTON3C_AGENCY_BASE=http://127.0.0.1:7070 \
  clojure -M -m futon3c.test-registry.validation-adapters sweep agency
enqueued 46 skipped 0
```

The sweep read the real Agency endpoint but appended only to isolated `/tmp`
ledgers. No serving-JVM reload, HTTP mutation, route change, or live hook was
performed. Pre-existing unrelated worktree entries were not staged.
