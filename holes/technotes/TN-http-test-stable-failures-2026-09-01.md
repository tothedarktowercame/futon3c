# Stable `futon3c.transport.http-test` failures, 2026-09-01

## Scope and reproduction

This is a read-only diagnosis of the failures left after the invoke-ledger test
isolation in `01d5ba2a`. A full isolated run of
`clojure -M:test -n futon3c.transport.http-test` produced:

```text
Ran 124 tests containing 631 assertions.
38 failures, 3 errors.
```

The request calls this an eight-var set, but its list and the test output both
contain **seven** distinct vars. No eighth var was inferred.

| Var | Category | Immediate cause | What a fix would have to change |
| --- | --- | --- | --- |
| `portfolio-step-returns-recommendation` | **A. SHARED LIVE STATE** | The endpoint returns 500, `portfolio-step-failed`, with `Cannot invoke "Object.toString()" because "s" is null`; all later response/evidence assertions consequently fail (`test/futon3c/transport/http_test.clj:2927-2945`). A direct scratch invocation gives `NullPointerException` at `clojure.string/lower-case`, called by `compute-coverage` at `src/futon3c/mission_control_backend.clj:1350`. That function lower-cases `:mission/id` values, one of which is nil. `build-portfolio-review` constructs those missions by scanning the current configured repository roots and reading current devmaps (`src/futon3c/mission_control_backend.clj:1543-1555`). The shared resource is the live Futon checkout's mission/devmap files, also consumed by the running system; the test does not replace that scanner with a fixture. | Give the test a hermetic portfolio-review/inventory fixture at the scanning seam. Separately, production could validate malformed inventory entries, but changing that is not necessary to remove shared-state coupling from this test. |
| `aif-stack-live-rolls-forward-s6-agenda` | **A. SHARED LIVE STATE** | Its setup calls the same failing portfolio-step endpoint (`test/futon3c/transport/http_test.clj:2952-2957`) and ignores that response. Therefore no step evidence is written: the agenda remains `unattempted`, witness/effect/successor fields are nil, and the three `(pos? nil)` checks error (`test/futon3c/transport/http_test.clj:2964-2996`). The shared resource is again the current checkout's mission/devmap inventory, reached through `build-portfolio-review` (`src/futon3c/mission_control_backend.clj:1543-1555`); the live stack projection additionally consumes the resulting evidence/state. | Supply the portfolio review, stack inputs, and evidence state hermetically, and assert that the prerequisite step succeeds before testing roll-forward. |
| `agent-compact-cold-path-queues-literal-control-and-returns-outcome` | **B. STALE FIXTURE** | The sole mismatch at `test/futon3c/transport/http_test.clj:1232` is the actual response's additional `:compact-witness nil`. `cold-compact-result` now includes `:compact-witness` in its response map (`src/futon3c/transport/http.clj:6101-6114`), introduced by `90ea01e4d` (`compact: treat registry :invoking as busy; witness fallback for pre-reload invoke-fn closures`). | Update the expected cold-path response contract to include the normalized witness value (nil for this fixture). |
| `health-includes-evidence-count` | **B. STALE FIXTURE** | The two assertions at `test/futon3c/transport/http_test.clj:2329-2330` expect `:evidence`; `/health` no longer supplies it. Commit `760210674` deliberately removed the unbounded evidence-store total count from the liveness endpoint; the current handler documents that decision at `src/futon3c/transport/http.clj:2097-2142`. | Remove the evidence-count expectation from this health test. If an evidence total is still a product requirement, expose and test a bounded/maintained metric outside the liveness path. |
| `maybe-route-surface-writes-strips-and-relays-minibuffer-directives` | **D. TEST BUG** | The exact-vector assertion at `test/futon3c/transport/http_test.clj:1457` omits `:server-sent-at-ms`; both actual payloads correctly contain it. The router intentionally attaches that dynamic delivery timestamp at `src/futon3c/transport/http.clj:3899-3903`. Blame shows the timestamp predates the test, so this is not a later contract invalidating an older fixture. | Compare stable payload fields and assert the timestamp separately as an integer/in-range value. |
| `war-machine-serves-cached-snapshot` | **B. STALE FIXTURE** | The fixture gives the authoritative decision `{"action" "abstain"}` but expects the head of `ranked-actions` to become recommendation `M-live` and expects a non-blocking boundary (`test/futon3c/transport/http_test.clj:3012-3025,3048-3055`). Commit `284da598d` (`unify live WM decision and presentation`) deliberately changed `live-recommendation/project` to present only an actionable, strategic-memory-influenced authoritative decision, and explicitly forbids re-ranking candidates to manufacture a winner (`src/futon3c/aif/live_recommendation.clj:2-6,36-101`). Thus this abstain fixture now correctly projects no recommendation. | Change the fixture/expectations to the current authoritative-decision schema, depending on whether this test is meant to cover abstention or recommendation issuance. |
| `whistle-timeout-returns-pollable-supervised-job` | **C. REAL REGRESSION** | **Production violates the test's stated guarantee.** The response correctly becomes 504/overrun, but the polled job becomes `failed` rather than `done`, losing `late whistle result` (`test/futon3c/transport/http_test.clj:481-482`). `handle-whistle` uses the caller's 20 ms response wait as `run-invoke-job!`'s `:timeout-ms` (`src/futon3c/transport/http.clj:5421-5434`). That same value is passed through the supervisor into `reg/invoke-agent!` (`src/futon3c/transport/http.clj:4257-4263`), whose local-invoke branch stops waiting after 20 ms and returns a detached timeout error (`src/futon3c/agency/registry.clj:1122-1144`). The outer supervisor therefore receives a failure instead of retaining the actual invocation future whose result arrives at 120 ms. This test registers its own local agent and server, so no live resource explains the failure. | Separate the synchronous HTTP response deadline from the supervised turn's inner invocation lifetime, so the response can return 504 while the same authoritative work continues and later finalizes the ledger job. |

## Counts and conclusion

- **A. Shared live state: 2 of 7 vars** (`portfolio-step-returns-recommendation`,
  `aif-stack-live-rolls-forward-s6-agenda`). Most failures are concentrated in
  these two vars, but most vars are not category A.
- B. Stale fixture: 3 of 7.
- C. Real regression: 1 of 7, the whistle supervision path.
- D. Test bug: 1 of 7.
- E. Undetermined: 0 of 7.

The headline is therefore not that most failing vars share production state.
Two do, and they account for most failure/error blocks because the AIF stack
test continues after its shared-state-dependent prerequisite has failed. The
most important independent finding is the whistle regression: the response
deadline is still being used as an inner invocation deadline, defeating the
late-result supervision contract.
