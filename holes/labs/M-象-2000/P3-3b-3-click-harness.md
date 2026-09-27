# P3-3b-3 — commissioned click evidence harness

Implementation `f9e52ae4`. The ordinary route has no direct evidence/act write,
but the R10 commissioned branch does write one existing evidence receipt via
its adapter. Therefore this packet stamps that receipt; it invents no new write.

## Write inventory (futon3c source)

| Path | Write | Site |
|---|---|---|
| HTTP entry | Delegates; no direct evidence or hyperedge append | transport/http.clj:8946–9041 |
| Ordinary click | Local JSONL budget consumption, forced before worker starts | wm/ordinary_click_budget.clj:50–96; callback http.clj:9012 |
| RUN4 admission | Local reservation.edn then click-result.edn, atomic writes | wm/run4_attempt_admission.clj:141,170,174,200 |
| Commissioned R10 admission | Local single-use reservation and state transitions | wm/r10_commission.clj:82,86,126,145,167 |
| Commissioned R10 completion | One coordination evidence entry, tags scheduled-dispatch/R10; commission-linked dispatch receipt | wm/r10_click_adapter.clj:32–61 → social/coordination_ledger.clj:125–171 |
| Runner lifecycle | Local phase JSONL and click-run-binding EDN | wm/runner_service.clj:237–251,294–414 |
| Runner terminal projections | Local terminal/historical projection artifacts | wm/run4_terminal_projection.clj:134–146; wm/run4_historical_projection.clj:84 |

No minted hyperedge is written by these click admission/receipt paths. The async
worker calls the full-loop runner (`runner_service.clj:485–501`), whose later
agent dispatches, entity groundings and run artifacts are downstream execution,
not additional writes made by this HTTP admission handler. Their context propagation
remains a separate producer packet. This change does not stamp those acts by
inference from a click happening nearby.

## Execution identity and change

`runner_service.clj:535` mints `wm-click-<random UUID>` for each accepted click;
it is returned as :click-id. The adapter already copies that into :dispatch/id
and :click/id in the receipt. It now adds :dispatch/harness with that same id,
kind war-machine, basis producer-context. The coordination ledger copies this
explicit producer value onto :evidence/harness only when supplied. Other ledger
users gain no inferred/default harness. No admission, cast or identity rules changed.

Origin is not rewritten. In the actual commissioned path the ledger currently
supplies no origin; the existing boundary stamps unknown. An operator issuer
is not converted into origin operator by this patch. The test pins this existing
unknown origin and unknown authorization. The user-facing requirement that an
already operator-stamped act stay operator is respected by leaving origin logic
untouched; this route did not previously produce such a stamp.

## Verification

New route test runs the actual HTTP handler, adapter, real on-disk commission
reservation machinery, coordination ledger and AtomBackend. Authority lookup
and runner execution are replaced; the latter returns two distinct click ids
without starting workers. Both resulting evidence records carry the exact
returned id. Reusing the consumed commission exercises actual admission refusal:
no additional runner call, evidence entry or harness stamp. Its existing HTTP
status is 500 (the first test expected 409, then was corrected after inspection);
this packet does not change error mapping. Temp files are removed in finally.

Only click-related tests ran, one namespace at a time:
- click-harness-http-test: 1 test / 13 assertions.
- r10-click-adapter-test: 6 tests / 32 assertions.
- flight-click-http-test: 3 tests / 7 assertions.

All 10 tests / 52 assertions passed; clj-kondo and check-parens passed the three
changed Clojure files. Code changed, so prior warrants cannot cover this revision.
Logs /tmp/p3-3b3-*.log.

Reloaded social.coordination-ledger and wm.r10-click-adapter through proof-eval,
using require :reload. Resource confirmed canonical
file:/home/joe/code/futon3c/src/futon3c/wm/r10_click_adapter.clj.
The unchanged HTTP route uses requiring-resolve for the adapter; the adapter uses
the ledger Var, so no retained handler or namespace reload is needed there.
After reload GET :7070/api/alpha/agents returned HTTP 200 with parsed JSON
(ok/count/agents). No live click, grant consumption or evidence probe was started.
