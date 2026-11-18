# Catalogue checks over query results (ramo)

This package evaluates the finite, ground facts in the existing shared fixture
`../../test/resources/arxana/design-catalogue-checks.json`. It does not copy that
fixture, query a store, create records, define a view, or change an extension.
XTQL is the query authority; these are evaluator agreement tests over its proposed
result shapes, not a claim that the query projection has already been implemented.

Install locally from the lockfile:

```sh
cd js/arxana-design-catalogue-ramo
npm ci --ignore-scripts
npm test
```

From the futon3c root, the equivalent test commands used for packet 17 are:

```sh
node --test js/arxana-design-catalogue-ramo/test.js
/home/joe/.local/lib/code-server-4.145.0-linux-amd64/lib/node --test js/arxana-design-catalogue-ramo/test.js
```

`ramo` is pinned to **0.1.2**, with its tarball integrity in package-lock.json.
There are no other runtime dependencies. node_modules is ignored. The code uses
Node's built-in test runner. `check(case)` returns `{result, violations}` using
JSON enum strings and objects, with duplicate violations removed. Tests compare
violation sets without imposing the traversal order of any engine.

## Evaluator differences

* Ramo 0.1.2 exports `eq`, `conde`, `Rel`, `run`, `succeedo` and `failo`, but no
  negation-as-failure operation. The internal `groundNot` only receives closed
  goals built from concrete input rows. It runs a separate query for one witness:
  a witness refutes absence; no witness means the finite relation was exhausted.
  It never negates an outer unbound logic variable. This matches the JVM checks'
  ground `nafc membero` uses, not arbitrary nonground negation.
* Enumeration is finite: citations, rules (including ordered pairs of rules),
  and links come from input arrays. The outer `run()` is exhaustive; only the
  existential absence subquery uses a one-result limit. No timeout or result cap
  returns a partial answer as a pass. Errors propagate. Large finite snapshots
  can still be expensive (duplicate-name checking is quadratic); no background
  scheduling, browser execution or timeout integration is claimed here.
* Ramo represents variables with symbol-tagged objects. It converts JSON arrays
  to its list representation and back, preserving `[]` versus `null`. Unlike
  Reazon it needs no tagged-array adapter. JSON object keys are structurally
  unified; object key order is irrelevant. Fixture results use keyword names as
  strings. The shared null/empty-array case pins that distinction.
* Agreement is **per shared fixture**, not a general equivalence theorem for
  JavaScript values, Clojure values and Emacs Lisp values. The Emacs and JVM
  implementations are unchanged by this package.

The test suite includes a ramo-only uncovered feature. It must return a violation
naming that feature, and the contrary `pass` assertion must fail. A separate
false-pass plant was also run and exited 1; its output is in the packet report.
