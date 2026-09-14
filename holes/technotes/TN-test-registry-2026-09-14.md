# Test registry: warrants through the evidence boundary

Joe's authority is futon2/holes/labs/wm-contract/NOTE-test-registry.md (rulings
2026-09-14). The registry stores intent, result and review records through
`futon3c.evidence.boundary/append!` and the existing HTTP EvidenceBackend.
It does not create a second ledger. Log files and exported receipts are artifacts;
the evidence API is the record authority.

Each record carries canonical EDN in the evidence body, its SHA-256, and the
previous evidence ID and SHA-256. This uses RUN4's `c-fold-config/sha256` byte
hashing and pinned-packet discipline. As with WORK-REMAINING row 3's construction
records, validator outcomes and input bindings live inside the saved record.
JSON transports the pinned EDN string without changing keyword or number types.
Each run starts a chain; its result follows its durable intent, and review/sample
runs extend that chain. Independent runs can coexist without a mutable global
head. Federation uses the same EvidenceBackend protocol, origin labels and
content-addressed records; this implementation does not claim a cross-server
global total order or authenticated author signatures.

## Author

From canonical futon3c:

```
clojure -M -m futon3c.test-registry run holes/technotes/test-registry-2026-09-14/author.edn
```

The EDN configuration declares repository root, code and test paths, exact argv,
author, artifact directory and Agency URL. The code/test SHA fields hash sorted
path-to-file-SHA manifests, so uncommitted bytes are identified, rather than
misrepresented by HEAD alone. HEAD is retained separately. The writer validates
one explicit namespace, captures inputs/environment, appends an intent, runs the
command with stdout/stderr captured to a fresh log, checks inputs/environment
again, then appends the result. An interrupted run leaves an intent without a
result; a failed append yields no warrant. A failed or unparseable test run is
recorded with typed absent counts, not invented zeros.

The first execution adapter supports Clojure's Cognitect test runner. Other
languages need a fingerprint/result-parser adapter and currently return a typed
unsupported-runner refusal. Repository paths and the evidence backend are not
hardcoded. Direct test commands remain available when no warrant can be made.

## Environment and bounded execution

The fingerprint hashes the resolved test classpath: JAR bytes and every file in
each directory, plus CLI configuration, launcher, JVM executable/modules and
hashed relevant environment-variable values. It checks both before and after
execution. It does not prove the absence of transient edits or authenticate the
runner; it is a warrant under the declared test inputs and trusted author.
External services, clocks and nondeterministic fixtures still require reviewer
judgment and appropriate test lanes.

A directory exceeding the declared 256 MiB fingerprint budget is typed-refused,
as are symlinks. No files are silently excluded to obtain an environment match.
The normal futon3c `:test` classpath includes sibling repository roots, including
the 87 GB futon1b checkout and mutable runtime data; the first commissioning
attempt failed during fingerprinting with `:symlink-scope-unsupported`, before
any intent or test execution. Its report is `/tmp/clojure-607686495590884412.edn`.
This is an environment limitation, not a passing registered run.

The explicit `:test-pure` alias supplies a smaller dependency closure for pure
Clojure tests. It uses the same pinned Cognitect runner and slow-test exclusion.
It is not substituted automatically for `:test`; tests requiring stack libraries
must use their actual dependencies and obtain a bounded classpath structurally.
The demonstration uses the existing pure flight pretty-printer namespace.

## Reviewer

Use `check` or `review` with an EDN file containing `:entry-id`, `:repo-root` and
`:changed-paths` (the reviewer-declared diff scope). `check` verifies the complete
chain, intent/result binding, timestamps, exact current manifests, diff coverage,
resolved environment, log hash and parsed result envelope. Missing data produces
`:test-registry/refusal` with `:warrant? false`. It neither forbids direct tests
nor silently promotes a claim to a warrant. The diff scope is a reviewer input;
the registry does not independently infer the intent of a multi-author diff.

`review` additionally requires a different `:reviewer`, a substantive `:adequacy`
note, explicit `:lane` and `:first-run?`. Routine review runs exactly one declared
`:spot-test` using `-v`; its counts must confirm one executed test. `:pre-push`,
`:invariant`, or first-ever new-test review requires the whole declared namespace
command. A warrant failure requests a full rerun or operator inquiry with a typed
`:rerun-required` outcome. No invocation expands to the unbounded entire suite.
Full change validation spanning namespaces means one registered full-scope command
per relevant namespace, respecting the workspace's test policy.

Adequacy and lane selection are reviewer judgments recorded as such, not inferred
from test success. This demonstration does not exempt the new registry integrity
tests from full focused validation. It illustrates routine review on an unchanged,
existing renderer. Cost fields distinguish execution time from record-check and
fingerprint/registration overhead; a three-test example may save no wall time.

## Validation and commissioning

Registry controls: 13 tests / 57 assertions green using
`clojure -M:test -n futon3c.test-registry-test`. These gates are executed directly;
the broad stack classpath currently has no registry warrant. Pure renderer
registration and independent review receipts are recorded below after commissioning.
All changed Clojure/EDN passes clj-kondo 0/0 and the workspace paren gate. No shared server restart or live-load is
needed: the CLI uses the existing evidence API.

### Independent refusal and correction

Zai-7's first independent review refused before selective execution. Its diagnostic
isolated `LC_ALL`: the author shell set it to `C.UTF-8`, the reviewer shell left it
unset. The initial hypothesis that classpath resolution used the verifier's alias
was disproved: dependencies and toolchain matched. The empty mismatch details
were a real implementation defect.

The corrected check includes expected/observed differing fields and a next action.
Locale remains a test input, not a waived comparison. The demo explicitly declares
`:test-environment {"LC_ALL" "C.UTF-8" "LANG" "C.UTF-8" "TZ" "UTC"}`; both the test
process and fingerprint probes apply these recorded values. Other environmental
differences still refuse. Only these nonsecret locale keys support declared values.

JVM metadata now comes from a subprocess resolved with the recorded test alias.
A dedicated last alias sets a metadata-only main form, and its resolved classpath
must equal the test classpath. This preserves the test's JVM options without
inheriting the verifier JVM's properties. An earlier `-A ... -e` probe incorrectly
inherited the test runner main options and failed loading namespaces before any
test assertions ran (`/tmp/clojure-17780542405487325694.edn`). It is not counted as
validation; the corrected probe explicitly replaces the main entrypoint.
