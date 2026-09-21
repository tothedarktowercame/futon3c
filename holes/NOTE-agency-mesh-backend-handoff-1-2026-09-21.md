# Agency mesh backend — handoff 1 validation, 2026-09-21

Author: codex-17; reviewer/owner: claude-5. Implements only handoff 1 of
`NOTE-agency-accounting-gaps-2026-09-21.md`. Baseline is
`cfa2e00c80dcd0122cceb5732ea31da7cdfcc1f2`.

## Change and reload

Mesh operations resolve omitted backends from the existing
`futon3c.dev/!evidence-store` atom, without loading dev or creating another
store authority. HTTP creation paths with configured stores pass those stores
explicitly; the mesh endpoint passes its configured store to the reader.
`create-invoke-job!` validates the backend before job-state mutation.
Nil with no configured backend, raw atoms and AtomBackend are refused with
`ExceptionInfo` data `:error/code :mesh/non-durable-evidence-store`.

The only production namespaces requiring reload are
`futon3c.social.coordination-ledger` and `futon3c.transport.http`, in that order.
No bootstrap reconfiguration is needed: resolution reads dev's existing var at
call time. **No reload, eval, service restart or shared-JVM write was performed.**
The real-backend test ran a temporary Futon1b server/store on an ephemeral
loopback port in the isolated test JVM.

Existing unit tests explicitly opt into volatile storage through
`mesh-test-fixtures/with-store`. Its root rebinding covers raw executor
callbacks, which do not carry thread-local dynamic bindings. Production's
`*test-evidence-store*` default is nil. Existing test assertions were not changed.
The new integration test installs a production-shaped dev configuration var
without loading dev services, uses the actual resolver, creates a job through
HTTP's common job creator without an explicit store, reconstructs the real
Futon1bBackend client, and reads the edge through both mesh and evidence HTTP
readers. The unused default atom remains empty.

## Validation

Final focused command:

```sh
clojure -M:test:test-all -n futon3c.social.mesh-backend-test
```

**5 tests / 28 assertions, zero failures or errors**, including the `^:slow`
real-backend test. Earlier intermediate integration failures were test-fixture
errors (invalid ingress configuration and JSON string versus EDN keyword
comparison); both were corrected before this final run.

Every other namespace below ran separately using `clojure -M:test -n <ns>`.
The full suite was never invoked. The table reports the final focused result
for mesh-backend-test and the namespace runs for the existing tests.

| Namespace | Tests | Assertions | Failures | Errors |
|---|---:|---:|---:|---:|
| `futon3c.social.coordination-ledger-test` | 5 | 21 | 0 | 0 |
| `futon3c.social.mesh-backend-test` | 5 | 28 | 0 | 0 |
| `futon3c.cross-repo-test` | 8 | 64 | 0 | 0 |
| `futon3c.transport.http-test` | 128 | 654 | 43 | 4 |
| `futon3c.transport.auto-bellback-test` | 35 | 119 | 20 | 0 |
| `futon3c.transport.invoke-ingress-integration-test` | 13 | 85 | 0 | 0 |
| `futon3c.transport.delivery-protocol-conformance-test` | 7 | 258 | 0 | 0 |
| `futon3c.transport.job-timeout-test` | 23 | 70 | 5 | 0 |
| `futon3c.transport.warrant-handoff-test` | 6 | 24 | 0 | 0 |
| `futon3c.transport.execution-evidence-test` | 16 | 36 | 1 | 0 |
| `futon3c.agency.r9-genesis-test` | 1 | 31 | 0 | 0 |
| `futon3c.agency.inbox-test` | 2 | 27 | 3 | 1 |
| `futon3c.runtime.agents-test` | 8 | 28 | 0 | 0 |
| `futon3c.apm.incident-regression-fixtures-test` | 7 | 50 | 0 | 0 |
| `futon3c.social.whistles-test` | 12 | 39 | 0 | 0 |
| `futon3c.social.dispatch-test` | 30 | 55 | 0 | 0 |
| `futon3c.social.dispatch-integration-test` | 8 | 36 | 0 | 0 |
| `futon3c.social.dispatch-realbackend-test` | 7 | 42 | 0 | 0 |
| `futon3c.social.pipeline-test` | 8 | 28 | 0 | 0 |

The five failing regression namespaces are **not reported as passing**. Their
failing test cases were also run against the committed source and test files
in an isolated classpath overlay (no working-tree replacement and no shared
JVM loading). Baseline comparison results are below. These failures are outside
this handoff; no guards, assertions or invariants were disabled to clear them.

clj-kondo: zero errors; the production changes and new tests have zero warnings.
The full changed-file lint reports 13 warnings in four existing test files;
`git show cfa2e00c:<path>` piped to clj-kondo with the same filename reproduces
all 13 (cross-repo 2, dispatch-integration 8, dispatch-realbackend 1, pipeline 2).
The existing http.clj redundant-boolean info diagnostic also remains unchanged.
`futon4/dev/check-parens.el` passed over all 22 changed Clojure files.
`git diff --check` passed.

### Committed-source comparisons

Each row selects only the failing cases with repeated `-v namespace/case`
arguments, still one namespace per invocation. The overlay contains the
baseline `coordination_ledger.clj`, `http.clj` and the baseline test files and
precedes the normal test classpath. This avoids changing the shared checkout.

| Namespace | Selected cases | Baseline failures/errors | Changed failures/errors |
|---|---:|---:|---:|
| `futon3c.transport.http-test` | 9 | 43 / 4 | 43 / 4 |
| `futon3c.transport.auto-bellback-test` | 7 | 20 / 0 | 20 / 0 |
| `futon3c.transport.job-timeout-test` | 3 | 5 / 0 | 5 / 0 |
| `futon3c.transport.execution-evidence-test` | 1 | 1 / 0 | 1 / 0 |
| `futon3c.agency.inbox-test` | 2 | 3 / 1 | 3 / 1 |

Failure identities and per-case failure/error counts match exactly, not just
aggregate totals. The first full HTTP baseline run suffered extra cascading
failures from its test ledger; the isolated-case comparison above reproduces
the changed run's exact failure set without that contamination. Examples:
portfolio/health expectations, added surface-event timestamps, delivery receipt
expectations, missing commission archive fixture, typed-bell fixtures expecting
an unregistered recipient to pass, and timeout expectations.

Raw local logs: `/tmp/c17-mesh-final.log`,
`/tmp/c17-futon3c.<namespace-suffix>.log`, and
`/tmp/c17-baseline-futon3c.<namespace-suffix>.log`.
These scratch logs are not a durable test-registry warrant; the table and
reproduction details are committed for review.

## Boundaries for handoffs 2–11

No edge schema/fields changed. No historical volatile edges were migrated.
The explicit backend resolver and reader arity are reusable by later work.
Schema/idempotency, lifecycle outboxes, park durability, wake events, provider
usage linkage and complete paginated graph queries remain unimplemented.
In particular, the existing HTTP best-effort edge-publication catch/ignored
failure receipt remains: this fixes backend selection, not crash-safe atomic
job-plus-edge publication. Handoffs 3–4 still need to close that gap.
The scheduled-dispatch (R10) emitter's separate fallback is unchanged; it is not
a mesh-edge writer. Later work must not mistake this change for a general
repair of all evidence producers or a complete append-only lifecycle journal.
