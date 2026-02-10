# QA: M-forum-refactor (Evidence Landscape)

Date: 2026-02-10

Repo: `futon3c` @ `079e8d3`

This is a read-only verification against `holes/missions/M-forum-refactor.md` exit conditions.

## Test Run Record

Command:
```bash
cd /home/joe/code/futon3c && clojure -X:test
```

Result (recorded):
```
Ran 157 tests containing 397 assertions.
0 failures, 0 errors.
```

## Exit Conditions Verification (9)

Source list: `holes/missions/M-forum-refactor.md:384` (`## Exit Conditions`).  

### 1. Evidence shapes validate all entry/query/artifact-ref types

Status: Met

Evidence:
- Shapes defined: `src/futon3c/social/shapes.clj:266` (ClaimType) through `src/futon3c/social/shapes.clj:316` (EvidenceQuery).
- Shape tests cover valid + invalid:
  - ArtifactRef valid/invalid: `test/futon3c/social/shapes_test.clj:410`, `test/futon3c/social/shapes_test.clj:416`
  - EvidenceEntry valid/invalid: `test/futon3c/social/shapes_test.clj:422`, `test/futon3c/social/shapes_test.clj:434`
  - EvidenceQuery valid: `test/futon3c/social/shapes_test.clj:445`

Notes:
- EvidenceQuery currently lacks an explicit invalid test case in `shapes_test.clj` (flagged in the consistency check).

### 2. Evidence store passes invariant tests (append-only, reply-chain integrity, fork integrity)

Status: Met (for implemented invariants)

Evidence:
- Reply-chain integrity (reject missing `in-reply-to`): `test/futon3c/evidence/store_test.clj:39`
- Fork integrity (reject missing `fork-of`): `test/futon3c/evidence/store_test.clj:52`
- Append-only behavior for durable evidence:
  - Store API provides append + query, no overwrite/update API: `src/futon3c/evidence/store.clj:111` (append*) and `src/futon3c/evidence/store.clj:139` (append!)
  - Ephemeral compaction removes ephemeral only and preserves durable entries: `test/futon3c/evidence/store_test.clj:151`

### 3. Thread projections pass proof-tree invariant tests (all 7 + fork relaxation)

Status: Met (via integration invariant test)

Evidence:
- All 7 invariants are asserted in `proof-tree-invariants-hold`: `test/futon3c/evidence/integration_test.clj:421` through `test/futon3c/evidence/integration_test.clj:479`.
- Fork scenario and fork navigation exercised: `test/futon3c/evidence/integration_test.clj:154` through `test/futon3c/evidence/integration_test.clj:203`.

Notes:
- The invariants are currently validated in tests, not enforced as hard rejections in the projection code. This is acceptable for “projection as view” but should be tracked if projections become inputs to decisions.

### 4. Conjectures trackable through lifecycle (open → confirmed/refuted)

Status: Met

Evidence:
- Open → confirmed: `test/futon3c/evidence/integration_test.clj:209` through `test/futon3c/evidence/integration_test.clj:245`
- Refuted path: `test/futon3c/evidence/integration_test.clj:247` through `test/futon3c/evidence/integration_test.clj:266`

### 5. Ephemeral entries excluded from queries by default, compactable

Status: Met

Evidence:
- Excluded by default: `test/futon3c/evidence/store_test.clj:84`
- Included when opted-in: `test/futon3c/evidence/store_test.clj:94`
- Compactable + preserves durable record: `test/futon3c/evidence/integration_test.clj:272` through `test/futon3c/evidence/integration_test.clj:317`

### 6. Front page query works (recent activity across subjects)

Status: Met

Evidence:
- Cross-subject recent activity + excludes ephemeral: `test/futon3c/evidence/integration_test.clj:127` through `test/futon3c/evidence/integration_test.clj:148`
- Ordering newest-first: `test/futon3c/evidence/store_test.clj:137`

### 7. Pattern mining works on closed threads

Status: Met

Evidence:
- Pattern mining integration test: `test/futon3c/evidence/integration_test.clj:323` through `test/futon3c/evidence/integration_test.clj:367`

### 8. All new tests pass, existing tests unaffected

Status: Met

Evidence:
- Entire suite passes: see “Test Run Record” above.

### 9. `clojure -X:test` passes cleanly

Status: Met

Evidence:
- Entire suite passes: see “Test Run Record” above.

## Findings / Gaps (Non-blocking)

1. Shape test coverage gaps for enum-only shapes:
   - `ClaimType`, `ArtifactRefType`, `EvidenceType` appear in the registry (`src/futon3c/social/shapes.clj:356`) but do not have direct valid+invalid tests in `test/futon3c/social/shapes_test.clj`. They are indirectly exercised via `EvidenceEntry` tests.
2. `EvidenceQuery` has a valid test but no explicit invalid test (`test/futon3c/social/shapes_test.clj:445`).

## Shape/Implementation Consistency Check (Registry-wide)

Source of truth: `src/futon3c/social/shapes.clj:356` (shape registry map).

Legend:
- Factory: function or helper in `test/futon3c/social/test_fixtures.clj`
- Valid/Invalid tests: `test/futon3c/social/shapes_test.clj`
- Component shape validation: code calls `shapes/valid?` or `shapes/validate` at the boundary (or is validated as a sub-shape of a larger validated record)

| Shape | Factory | Valid test | Invalid test | Component shape validation |
|---|---|---|---|---|
| TypedAgentId | `make-agent-id`, `make-transport-id` | `typed-agent-id-valid` (`test/futon3c/social/shapes_test.clj:26`) | `typed-agent-id-invalid` (`test/futon3c/social/shapes_test.clj:32`) | Used/validated at many boundaries (presence/auth/dispatch/etc.) |
| AgentConnection | `make-connection` | `agent-connection-valid` (`test/futon3c/social/shapes_test.clj:42`) | `agent-connection-invalid` (`test/futon3c/social/shapes_test.clj:56`) | `presence/verify` validates input (`src/futon3c/social/presence.clj`) |
| PresenceRecord | `make-presence` | `presence-record-valid` (`test/futon3c/social/shapes_test.clj:70`) | `presence-record-invalid` (`test/futon3c/social/shapes_test.clj:79`) | `presence/verify` validates output (`src/futon3c/social/presence.clj`) |
| AgentIdentity | `make-identity` | `agent-identity-valid` (`test/futon3c/social/shapes_test.clj:91`) | `agent-identity-invalid` (`test/futon3c/social/shapes_test.clj:99`) | `authenticate/resolve-identity` validates input+output (`src/futon3c/social/authenticate.clj`) |
| ClassifiedMessage | `make-classified-message` | `classified-message-valid` (`test/futon3c/social/shapes_test.clj:111`) | `classified-message-invalid` (`test/futon3c/social/shapes_test.clj:126`) | `mode/classify` validates output (`src/futon3c/social/mode.clj`) |
| DispatchReceipt | `make-dispatch-receipt` | `dispatch-receipt-valid` (`test/futon3c/social/shapes_test.clj:139`) | `dispatch-receipt-invalid` (`test/futon3c/social/shapes_test.clj:153`) | `dispatch/dispatch` validates input+output (`src/futon3c/social/dispatch.clj`) |
| CoordinationOutcome | `make-coordination-outcome` | `coordination-outcome-valid` (`test/futon3c/social/shapes_test.clj:164`) | `coordination-outcome-invalid` (`test/futon3c/social/shapes_test.clj:173`) | `validate/validate-outcome` validates output (`src/futon3c/social/validate.clj`) |
| SessionRecord | `make-session-record` | `session-record-valid` (`test/futon3c/social/shapes_test.clj:186`) | `session-record-invalid` (`test/futon3c/social/shapes_test.clj:194`) | `persist/*` validates outputs (`src/futon3c/social/persist.clj`) |
| PatternLibrary | `mock-patterns` | `pattern-library-valid` (`test/futon3c/social/shapes_test.clj:206`) | Missing | `mode/classify` + `validate/validate-outcome` validate patterns (`src/futon3c/social/mode.clj`, `src/futon3c/social/validate.clj`) |
| AgentRegistryShape | `mock-registry` | `agent-registry-shape-valid` (`test/futon3c/social/shapes_test.clj:215`) | Missing | `presence/verify`, `authenticate/resolve-identity`, `dispatch/dispatch` validate registry snapshots |
| SocialError | `make-social-error` | `social-error-valid` (`test/futon3c/social/shapes_test.clj:226`) | `social-error-invalid` (`test/futon3c/social/shapes_test.clj:240`) | All components return typed errors |
| ModeTransition | `make-mode-transition` | `mode-transition-valid` (see `test/futon3c/social/shapes_test.clj:254`) | `mode-transition-invalid` (see `test/futon3c/social/shapes_test.clj:275`) | `mode/validate-transition` validates output (`src/futon3c/social/mode.clj`) |
| PeripheralSpec | `make-peripheral-spec` | `peripheral-spec-valid` (see `test/futon3c/social/shapes_test.clj:292`) | `peripheral-spec-invalid` (see `test/futon3c/social/shapes_test.clj:310`) | `peripheral/load-peripherals` validates every spec (`src/futon3c/social/peripheral.clj`) |
| HopRequest | `make-hop-request` | `hop-request-valid` (see `test/futon3c/social/shapes_test.clj:332`) | `hop-request-invalid` (see `test/futon3c/social/shapes_test.clj:344`) | `peripheral/validate-hop` validates input (`src/futon3c/social/peripheral.clj`) |
| HopResult | `make-hop-result` | `hop-result-valid` (see `test/futon3c/social/shapes_test.clj:359`) | `hop-result-invalid` (see `test/futon3c/social/shapes_test.clj:375`) | `peripheral/validate-hop` validates output |
| ArtifactRef | `make-artifact-ref` | `artifact-ref-valid` (`test/futon3c/social/shapes_test.clj:410`) | `artifact-ref-invalid` (`test/futon3c/social/shapes_test.clj:416`) | Evidence store/threads/validate use it as subject |
| EvidenceEntry | `make-evidence-entry` | `evidence-entry-valid` (`test/futon3c/social/shapes_test.clj:422`) | `evidence-entry-invalid` (`test/futon3c/social/shapes_test.clj:434`) | `evidence.store/append!` validates entry (`src/futon3c/evidence/store.clj`) |
| EvidenceQuery | `make-evidence-query` | `evidence-query-valid` (`test/futon3c/social/shapes_test.clj:445`) | Missing | `evidence.store/query*` validates query (`src/futon3c/evidence/store.clj`) |
| ClaimType | Indirect (via EvidenceEntry factory) | Indirect (via EvidenceEntry tests) | Missing | Validated as sub-shape of EvidenceEntry |
| ArtifactRefType | Indirect (via ArtifactRef factory) | Indirect (via ArtifactRef tests) | Missing | Validated as sub-shape of ArtifactRef |
| EvidenceType | Indirect (via EvidenceEntry factory) | Indirect (via EvidenceEntry tests) | Missing | Validated as sub-shape of EvidenceEntry |

Summary flags:
- Missing invalid tests: `PatternLibrary`, `AgentRegistryShape`, `EvidenceQuery` (and direct tests for enum-only shapes).

