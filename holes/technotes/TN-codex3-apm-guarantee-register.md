# TN-codex3-apm-guarantee-register — dated guarantees for the APM machine

Codex-3, 2026-08-27. Plan and register only; no mechanisms described as
planned below were implemented in this round.

This applies the definition in `TN-apm-defect-register.md` strictly:
**guaranteed from a date** means a mechanism exists such that the defect cannot
recur without the mechanism failing or being removed. Tests, review,
conventions, and post-damage checks do not count as prevention. `detected`
means the event cannot remain absent from the machine's durable account under
the stated mechanism; its latency is explicit.

Dates marked `(planned)` are limited to mechanisms that are credible to build
and validate in one day. Other dates are sequencing targets, not promises of
implementation capacity. `Unscheduled` means design or dependency work must be
settled before an honest date can be assigned.

## Guarantee register

| id | guarantee | guaranteed from | mechanism | what would break it |
|---|---|---|---|---|
| D1 | partial | 2026-08-27 | `b642640a` appends the rejected terminal, ticket, collection, and findings to `:superseded-terminals` before the generic live-job terminal-repair path removes the live collection. | A redispatch/repair path outside `live_job_driver` overwrites the slot; the state persist fails after successor side effects; later compaction or state replacement drops `:superseded-terminals`. |
| D2 | none | unscheduled | No common append-before-successor boundary covers solver rounds, transport retry, supervisor/claim recovery, manual resume, typed migration, and attempt-numbered writes. Target: all successor creation consumes an append-only predecessor-disposition capability. | Until that boundary exists, any unaudited writer can replace evidence. Afterward, a writer constructed without the capability or a faulty event store would break it. |
| D3 | prevented | 2026-09-03 | Target: make an append-only attempt/event stream authoritative and make `live/student-attempt-N.edn` a projection carrying its source event id and projection time. Two observations can then be compared by event id rather than mistaken for one immutable record. | A reader treats the projection as authority, event ids are reused, or an alternate writer changes the projection without appending an event. |
| D4 | prevented | 2026-09-10 | Target: store terminal/submission/receipt/disposition events in a backed-up append-only authority with explicit retention; permit ledger expiry only after a verified durable reference exists. | Retention deletes the final referenced blob, backup/integrity verification fails, or a write path emits an unreferenced external blob. |
| D5 | none | unscheduled | Target: evidence-derived fault attribution and separate apparatus retry accounting; an apparatus-caused or indeterminate invalidation cannot decrement the student attempt budget. The dependency on M5 and unresolved attempt-budget policy make a date premature. | Classification lacks prompt/access evidence, budget mutation bypasses the disposition transition, or policy deliberately reclassifies apparatus retries as student attempts. |
| A1 | detected | 2026-08-27 (planned) | Add a machine-owned semantic-progress watchdog with durable halt authority. It watches the frame progress cursor `(frame-id, phase, attempt-ordinal, obligation/status, active-job-id, last-committed-event-id)`, not scheduler tick count; terminal regulator failure halts immediately, ready/non-awaiting lack of progress halts after 5 minutes, and awaiting jobs use their declared deadline plus a 2-minute reconciliation grace. Detection latency is immediate for a failed tick, at most 5 minutes for an internal stall, or deadline + 2 minutes for an external job. | The campaign starts without the watchdog, progress fields are updated without a real committed transition, a job has no declared deadline, the watchdog shares the failed scheduler/executor, or halt cannot durably disable recovery. |
| A2 | detected | 2026-08-27 (planned) | The A1 watchdog treats a first-tick `:failed` regulator state or a frame with no dispatched obligation and no semantic transition as a stop condition; it durably disables the campaign and records the failure. Detection latency: one watchdog poll, targeted at no more than 10 seconds for terminal failure. | Failure-state persistence fails, the independent watchdog does not run, frame state falsely records progress, or durable disable is bypassed by recovery. |
| A3 | prevented | 2026-09-03 | Target: remove/publicly hide process-local one-arity stop; the only operator stop transition durably disables first, prevents new tick claims, then drains. Existing two-arity stop is the correct intent update but coexistence with the misleading API prevents a total guarantee. | Any caller retains access to runner-only cancellation as “stop”, registry writes are not atomic/durable, or recovery starts disabled entries. |
| A4 | prevented | 2026-09-10 | Target: stop establishes `draining`, rejects new claims, waits for the claimed tick to finish or returns a named in-flight tick, and returns `stopped` only with a durable quiescence witness. | Tick effects occur without a durable claim, cancellation is reported as completion, lease ownership is ambiguous, or an effect commits after the quiescence witness. |
| A5 | prevented | 2026-09-10 | Target: every tick durably claims an epoch/tick id before effects and clears it only after durable completion; the sole status API reports that claim and reconciliation state. | Work executes before/without the claim, claim persistence and effect persistence reorder, stale leases are presented as live without reconciliation, or callers use runner/job tables as status. |
| A6 | prevented | 2026-09-10 | Target: status is derived from desired state, runner epoch, durable tick claim, and regulator state and returns `inconsistent` rather than the bare stale `:running` value. Recovery must reconcile before starting. | A status consumer reads the regulator file directly, reconciliation trusts stale state, or runtime identity/epoch is not bound to the durable claim. |
| A7 | detected | 2026-08-27 | `live-regulator/tick!` catches an escaped `Throwable`, records class/message as `:live-regulator-tick-threw`, transitions the regulator to `:failed`, and persists it during that tick. Detection is same-tick if persistence succeeds; this does not diagnose, prevent, alert on, or safely retry the cause. | The process dies before failure persistence, `persist-fn` fails, an exception occurs outside the wrapped callback, fatal/cancellation semantics evade the catch, or later state overwrite removes the failure. |
| A8 | detected | 2026-08-27 | `live-supervisor/tick!` converts a failed launch audit to `:live-supervisor-launch-audit-failed`; the regulator records the result and fails in the same tick. This guarantees a durable failure result, not advancement or an operator alarm. | Audit throws instead of returning (then A7 records only the outer class), regulator persistence fails, another launch path skips `launch-audit-fn`, or later state replacement erases the result. |
| M1 | partial | 2026-08-27 | `d3cf69df` carries the shelf-derived withheld ids in typed job authority and filters them in the known search adapter before returning results; shelf and cascade also filter their known candidates. | A new/alternate serving path skips filtering; authority propagation drops the fields; memory is store-only and therefore absent from the shelf-derived set; or content reaches the prompt through an unrecorded cache/embed path. |
| M2 | prevented | 2026-09-03 | Target: a mandatory pre-serve decision compares the current problem to immutable depositor/problem truth on every candidate, independent of shelf membership; missing provenance fails closed. | A memory lacks trusted subject metadata, a serving adapter bypasses the gate, metadata can be rewritten after decision, or prompt construction can fetch content by another route. |
| M3 | prevented | 2026-09-03 | Target: generate request authority, persisted authority, access capability, receipts, and validator schemas from one contract; serving adapters receive the policy capability rather than selecting fields manually. | A handwritten authority path remains, generated artifacts are stale yet admitted, an adapter reads undeclared fields, or runtime loads a different generated contract. |
| M4 | partial | 2026-09-03 | Target for memory reads: the M2 gate runs before ids/content cross the role boundary; terminal validation remains independent detection. Existing launch/provider checks already protect some other actions, so this row cannot honestly promise all invariants without the effect-entry inventory. | An effectful entry point is unregistered, information is embedded before the gate, the gate checks ids but not content, or an invariant can only be evaluated after the effect and no later irreversible action is blocked. |
| M5 | prevented | 2026-09-10 | Target: derive `student-fault`, `apparatus-caused`, `mixed`, or `indeterminate` from append-only access/prompt/authority/timing evidence before disposition and budget mutation; disclosure is recorded separately. | Required evidence is absent, apparatus can serve content without a receipt, classification accepts self-report as causation, or disposition/budget callers bypass the classifier. |
| M6 | detected | 2026-09-17 | Target: generate a live-carrier/model-carrier coverage matrix and reject any serving adapter without a model carrier and refinement obligation; generalise holdout from `shelf` membership to `Served job carrier memory`. Detection is synchronous at build/admission for registered source changes. | Dynamic/unregistered carriers exist, generation inputs are incomplete, CI/admission does not reject drift, or a live adapter satisfies the registry nominally while bypassing the modelled gate. |
| M7 | partial | 2026-08-27 | `scripts/proof-eval.sh` refuses `load-file` outside the running JVM classpath, preventing the known worktree-namespace replacement route. Repository policy also names the canonical JVM, but policy alone is not a guarantee. | `require :reload`, `load-file` from a dirty canonical checkout, another REPL/eval path, classpath resource drift, or direct runtime mutation can still desynchronise loaded definitions from the declared commit. |
| M8 | prevented | 2026-09-10 | Target: frame admission certificate binds Lean-emitter schema, generated Clojure contract, semantic positive/negative fixtures, loaded namespace/resource digests, and runtime identity; mint rejects a stale/mismatched certificate. | A mint path skips admission, digest scope omits an executable dependency, runtime reload does not invalidate the certificate, or both sides share the same erroneous fixture interpretation. |
| M9 | none | unscheduled | No mechanism permits an already closed historical frame to receive an append-only invalidation disposition while preserving its original closure. Target requires ledger/disposition semantics, not bypassing `frame-void/prepare`. | Until designed, closure makes retroactive void impossible. Afterward, mutation replaces closure instead of appending invalidation, references are not migrated, or downstream metrics ignore the later disposition. |
| M10 | detected | 2026-09-17 | Target: every error code is registered with owning boundary, committed-effects schema, retry class, durable evidence, recovery transition, and campaign consequence; unknown codes fail closed at the outer boundary. Detection/classification is synchronous at the owning boundary; this guarantees classification/containment, not absence of failures. | Code throws/returns outside the typed boundary, a new code is emitted without registry enforcement, committed effects are reported inaccurately, or recovery executes without consulting the classification. |

## Assessment of the two proposed guards

### 1. Liveness watchdog

The proposed watchdog is the right family of mechanism, but the proposed tuple
is not the right progress predicate. Including `tick` means a scheduler that
wakes every two seconds satisfies the condition forever while the frame remains
stuck in one phase. Conversely, a single fixed `N` over phase and attempt would
halt legitimate long-running role jobs.

Use three clocks:

1. **Scheduler health:** heartbeat/lease while a tick is executing. A tick
   should normally finish far below the two-second regulator period; a claim
   older than 30 seconds is inconsistent and stops new work pending
   reconciliation.
2. **Internal semantic progress:** while the supervisor is `ready` and not
   waiting on an external job, one of phase, attempt ordinal, obligation
   status, active job id, or committed event id must change within **5
   minutes**. At a two-second period this permits roughly 150 unsuccessful
   observations—ample tolerance for ordinary filesystem/scheduling jitter,
   while bounding an unattended internal stall to a small fraction of a role
   turn.
3. **External wait:** an awaiting job is not stalled merely because phase is
   unchanged. Each dispatch must declare a deadline derived from the role/job
   class. The watchdog stops at **deadline + 2 minutes** if terminal state
   cannot be reconciled. There should be no global `N` for solver and human
   jobs of different expected duration.

A regulator `:failed`, invalid state, failed launch audit, or impossible
transition should bypass the timers and halt immediately. “Halt” must mean
durably disable the coordinator and block new tick claims; a process-local
cancel recreates A3/A4. The watchdog must be scheduled independently of the
runner it watches, or a dead scheduler also kills its observer.

This can honestly be dated **2026-08-27 (planned)** only as a **detection and
campaign-containment guarantee**, not as prevention of stalls and not as
truthful quiescence. The one-day slice can read existing durable state, maintain
an independent progress record, durably disable on the conditions above, and
emit a reason. It must fail closed when a dispatch has no deadline. The durable
tick-claim/quiescence work for A4–A6 is larger and remains dated later.

### 2. Write guard and resolvability scan

The proposal combines two useful ideas, but “no write to a populated record may
reduce it” is the wrong invariant if reduction means byte length, map-key count,
or structural containment. Legitimate projections change state, remove
ephemeral fields, compact derived data, and replace values. Such a generic
guard would either reject valid transitions or acquire exceptions until it no
longer guarantees anything.

The right prevention rule is narrower and semantic:

- authoritative attempts, terminals, receipts, and dispositions are
  **append-only, identity-addressed events**;
- successor announcement/activation requires a durable predecessor disposition
  reference;
- mutable live files are explicitly typed projections and can be replaced, but
  every projection names the event from which it was derived;
- retention cannot remove a referenced event until a verified archival
  transition moves that reference to another durable authority.

The resolvability scan is the right complementary **detection** mechanism. Walk
durable ledgers/events, resolve every attempt/job/receipt/blob reference, verify
content digests and readable schemas, and durably halt on the first missing or
corrupt target. Run it at admission, after every repair/retry/compaction
transaction, and periodically. Periodic-only scanning has a latency equal to
its interval; recommend **60 seconds** while a campaign is enabled, because the
dataset is local and small, plus synchronous scans at destructive lifecycle
boundaries. It detects loss; it does not restore the missing bytes.

An append-only wrapper for the single generic terminal-repair path plus a
resolvability scanner is plausible on **2026-08-27 (planned)**, but it would
only extend D1's partial guarantee and add prompt detection. A total guarantee
for D2–D4 cannot honestly receive that date: every writer and retention path
must be brought behind the event boundary, projections migrated, replay tested,
and durable storage/backup authority chosen. The register therefore dates the
projection/event split to 2026-09-03 and durable retention to 2026-09-10, while
leaving the currently unaudited whole class unscheduled until its inventory is
complete.

## What can be said on each date

- **Now, 2026-08-27:** D1 and M1 are partial; M7 blocks one known reload route;
  A7/A8 durably detect their returned/caught failure in the same tick, subject
  to persistence. No data-loss class and no frame-advancement class is totally
  prevented.
- **One-day planned slice:** stalled/failed frames become detected and the
  campaign is durably stopped within the stated latency. This is the first
  honest “will not continue unnoticed” statement, not “will never stall.”
- **2026-09-03 target:** store-independent holdout enforcement, generated
  authority propagation, semantic event identity, and a single durable stop
  entry point close the most direct recurrence routes.
- **2026-09-10 target:** append-only retained evidence, fault attribution,
  runtime-bound admission, and claimed/drained tick lifecycle support stronger
  prevention and truthful status.
- **2026-09-17 target:** generated live/model carrier coverage and complete
  error-boundary registration support formal and operational coverage claims.

None of those future statements becomes a guarantee merely because this file
assigns a date. The register must be updated from `none`/`partial` to the actual
guarantee only after the mechanism is merged, loaded where applicable, and its
failure boundary is verified.
