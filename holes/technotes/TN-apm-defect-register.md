# TN-apm-defect-register — every known defect, and what stops it recurring

claude-clink-1, 2026-08-27. Compiled at Joe's request. This is the evidence
side: what went wrong, what is in place now, and — stated strictly — whether
anything makes recurrence impossible.

**"Guaranteed from DATE" means one thing here:** a mechanism exists such that
the defect cannot recur without that mechanism itself failing or being removed.
A fix at one call site is not a guarantee if a second call site can reintroduce
it. A test is not a guarantee. A convention is not a guarantee. Where there is
no such mechanism the entry says **none**, and that is an acceptable answer.

Two failure classes matter most (Joe, 2026-08-27): **frames not advancing** and
**data being lost**. Defects are grouped by which class they produce.

## Class 1 — data lost

| id | defect | status | guarantee |
|---|---|---|---|
| D1 | Terminal repair overwrote the discarded attempt in place; `used-ids`, response and disclosure destroyed | `b642640a` appends the discarded terminal to `:superseded-terminals` before the `dissoc` | **partial** — covers the terminal-repair path only |
| D2 | Other overwrite paths unaudited: solver rounds, typed-contract migration, transport retry, supervisor/claim recovery, manual repair/resume, any attempt-numbered write | not audited | **none** |
| D3 | The attempt live slot is one mutable file; a repair rewrite makes two reads at different times look like one observation | unchanged | **none** |
| D4 | Recovered evidence exists only under gitignored `data/*` plus an Agency job ledger that expires | two files copied by hand today | **none** |
| D5 | A student attempt discarded for an apparatus-caused breach consumed the frame's repair budget | restored by hand for f48 | **none** |

## Class 2 — frames not advancing

| id | defect | status | guarantee |
|---|---|---|---|
| A1 | No liveness check anywhere in the machine. `grep` for stall/watchdog/heartbeat/no-progress across `src/futon3c/apm/` returns nothing | the only stall detection is a shell loop in a session monitor, outside the machine, advisory | **none** |
| A2 | f49 minted 13:39, dead 13:41 on its first tick (`live-job-state-invalid`), nothing dispatched, no alarm | unchanged | **none** |
| A3 | `live-regulator/stop!` (1-arity) cancels the runner but leaves the registry entry enabled; recovery re-arms it | 2-arity `durable-coordinator/stop!` does disable durably; the misleading API remains | **none** |
| A4 | A `stop!` that has returned does not mean no tick is running; an in-flight tick kept working and minted f49 | unchanged | **none** |
| A5 | Neither the runner table nor the active-job list reveals an in-flight tick — both were empty while work landed on disk | unchanged | **none** |
| A6 | Durable state read `:regulator/status :running` with an empty runner table | unchanged | **none** |
| A7 | `live-regulator-tick-threw` — exception escaping the tick, 13 occurrences, 23 Aug–27 Aug | outer containment only, no classification | **none** |
| A8 | `live-supervisor-launch-audit-failed` — 13 occurrences | unchanged | **none** |

## Class 3 — measurement contaminated (causes of the above)

| id | defect | status | guarantee |
|---|---|---|---|
| M1 | Same-problem holdout not enforced on the search channel; f46 (2 of 2 withheld ids served and used) and f48 (1 of 4) | `d3cf69df` carries the holdout on the job authority and filters `search!` | **partial** — a newly added channel is not covered |
| M2 | The holdout's domain is shelf-derived, so a same-problem memory never on a shelf is not withheld at all (f47's category) | unchanged | **none** |
| M3 | `authority-fields` is a hand-maintained allow-list decoupled from the request and validator schemas — the mechanism of M1 | unchanged | **none** |
| M4 | Invariants are validated at terminal collection, after the action they constrain | unchanged for reads | **none** |
| M5 | Apparatus-caused breach indistinguishable from student fault; disclosure made discard more likely, not less | unchanged | **none** |
| M6 | A Lean invariant may quantify over one carrier where the system has several (`same_problem_holdout_uses_depositor_truth` over `shelf`) with no mechanism to detect it | unchanged | **none** |
| M7 | Loaded JVM namespaces may diverge from the committed source | unchanged | **none** |
| M8 | Lean emitter ↔ generated contract ↔ Clojure validator ↔ loaded namespace have no enforced coupling | unchanged | **none** |
| M9 | `frame-void/prepare` refuses any frame that is not active, so a closed frame cannot be voided (f46) | unchanged | **none** |
| M10 | 25 distinct live failure codes, 86 occurrences, largely uncharacterised for retryability, committed effects and recovery | unchanged | **none** |

## Class 4 — coordination (added 2026-08-27 14:40)

| id | defect | status | guarantee |
|---|---|---|---|
| C1 | A completed job whose caller is a pull-only (`delivery-mode :inbox`) seat records `status: "delivered"` with `note: "bell-job-ready"` and a poll URL as its destination. No auto-bellback job is created and nothing is written to the seat's inbox. The caller learns nothing unless it polls. | observed on both codex-3 jobs (`…2379-903d5af6`, `…2380-a5e8d1dd`), caller correctly recorded as `claude-clink-1` in each; jobs from other seats in the same period produced real `auto-bellback-*` jobs and inbox files | **none** |

| C2 | A park placed by a pull-only CLI seat wakes a headless `claude -p --resume` fork that this terminal never sees. The fork reads the park payload as instructions and **acts on it with the parking agent's identity** — on 2026-08-27 it dispatched a review of the holdout fix to codex-18 as `caller: claude-clink-1`, duplicating work already committed as `d3cf69df` and consuming a seat reserved for other work. The parking session learns nothing. | the CLI-fork hazard is documented in `README-park.md`; the packet-level consequence — shadow dispatch under the parker's identity — was not | **none** |
| C3 | A job can be functionally successful and policy-failed under one `state` field: a probe returned the correct result and was delivered, yet reports `state: failed`, `terminal-code: no-execution-evidence`. A reviewer gating on `state` discards good work | observed 2026-08-27 on `…2384-1be867e4` | **none** |
| C4 | A job can be dispatched carrying another agent's identity as `caller` without that agent having sent it. Two instances today, both under `caller: claude-clink-1`: `…2376` to codex-18 (a duplicate holdout review, cause: park fork — C2) and `…2405` to codex-2 at 15:35:19, 18 seconds after my own bell to the same seat and not sent by me. Content in both cases was legitimate, but job provenance under a given identity cannot be trusted for audit or for seat accounting | not diagnosed; the job ledger is a rolling window, so earlier instances may have aged out and the count of 2 is a floor | **none** |
| G1 | `frame-cycle-handlers` guard `:frame-cycle-input-receipt-set-mismatch` fires only when `(:receipt/input-receipt-ids receipt)` is non-nil. `bank/build-receipt` has never set that key (unchanged since 2026-08-22), so for bank receipts `declared-inputs` is always nil and **the guard has never fired**. `bank-handler-rejects-a-different-frames-verify-receipt` asserts a protection that does not exist: a bank receipt naming another frame's verify receipt is accepted | found by the 2026-08-27 sweep; not today's regression | **none** |

**C4 resolved 2026-08-28.** Joe: codex-12 is working topology problems with
codex-18, and those seats carry his own work. The jobs appearing under
`caller: claude-clink-1` are legitimate work whose caller attribution is
inherited or defaulted — not something dispatching under a stolen identity. The
practical consequence stands and is now a seat-reservation matter rather than a
defect: job provenance under a given identity cannot be used to decide who asked
for what, so an audit must corroborate from elsewhere. This most likely also
explains the 2026-08-27 21:55 ticks on the disabled `jit-all-open-v2`
coordinator: the overnight commits were JIT tick-claim and deadline work, which
exercises that coordinator.
| A9 | On finding no live watchdog, `durable-coordinator` **halts the coordinator** instead of arming one. Both sites — the tick-time check and `start-entry!` — go straight to `stop!` with `:durable-coordinator-running-unwatched`. The obvious remedy, arm a watchdog and proceed, is absent. On 2026-08-28 this turned a stale-JVM condition into a self-inflicted outage: the campaign started, found no watchdog, disabled itself, and appeared unstartable. Joe: "the extremely obvious next step would be to start a watchdog process" | not fixed | **none** |
| A10 | F△ arms its own watchdog on its own coordinator, so it never traverses the production start path. F△ passing proves *a* frame can run; it does not prove *this campaign* can start. A9 sat undetected through eight green F△ runs for exactly this reason | not fixed | **none** |

C1 is the same shape as A3, A5 and A6: a status field asserting an action the
machine did not perform. Both affected jobs were dispatched with
`--mode work`; two samples is not enough to blame the mode, but the delivery
record shows the mechanism regardless of what selects it.

## Summary as of 2026-08-27 13:30

Twenty-six defects: two partial, twenty-four with no guarantee. (Count corrected 2026-08-27 after codex-3 noted it was stale — C2 and C3 were added without updating it.)

**This is not a statement that the machine guarantees nothing.** The register
lists what broke, and what broke is by construction what was not guaranteed —
so its emptiness of guarantees is close to tautological. Real guarantees exist
outside it: `futon3c.apm.generated-contract-test` (18 tests, 98 assertions,
green 2026-08-27) round-trips the Lean emitter's own output through the Clojure
validator and runs ~15 mutation tests that must reject a changed policy. Drift
between the model and the specification cannot pass silently. That perimeter is
the contract's content; every defect here landed outside it, which is the
finding, not an absence of assurance.

Relatedly, the 86 recorded failures are an upper bound on defects, not a count
of them: several codes (`student-candidate-validation-failed`,
`live-learning-request-invalid`, `promotion-publication-accounting-invalid`)
read as the apparatus correctly refusing invalid input — guarantees firing, not
failing. Settling that split needs the taxonomy in TN-codex3-apm-repair-plan §1.

Evidence: `TN-opus-f48-critical-findings.md`, `TN-opus-f47-observation.md`,
`TN-codex3-apm-repair-plan.md`, and the interleaved model/failure timeline at
https://claude.ai/code/artifact/b01d0577-1557-4b1f-9322-526222a79f75
