# TN-apm-unattended-progress-contract — surviving a 6–8 hour unattended window

claude-7, 2026-09-03. **Design only.** Nothing here is implemented; no runtime
state was altered in producing it. All figures are measured from
`data/apm-campaigns/jit-all-open-v2/` at the time of writing, not remembered.

Requested by Joe via codex-7: make the APM problem loop survive recurring 6–8
hour nights and an upcoming 6–8 hour flight without an operator.

## 0. Verdict first

**The two-hour FRAME budget cannot be met by the current protocol, and adopting
it as a hard deadline would be actively harmful to campaign yield.**

The median fault-free path through the existing phase ladder is **126 minutes**
— the sum of per-phase medians, with zero retries, zero stalls and zero
substrate faults. The median frame that actually *closed* took **172 minutes**;
only **2 of 19** closed frames finished inside 120 minutes, and the fastest
frame ever closed took **101 minutes**.

A 120-minute deadline therefore parks ~95% of frames. It would deliver a
campaign that advances the queue briskly and closes almost nothing: **~0.2
closed problems per 8-hour night**, against ~1.0 at a 240-minute budget.

The budget is not the binding constraint. **The student/guide ladder is.** It
accounts for 61% of the median frame. Raising or lowering timeouts cannot fix
that; only changing the ladder's depth can, and that changes what the
experiment measures, so it is Joe's call and not an operational one
(`TN-apm-watcher.md`: *"Do not silently change parameters (for example the
student turn budget) or anything else that changes what the experiment
measures."*).

**Recommendation:** adopt everything in this note *except* the 120-minute
number, and set the unattended frame budget to **240 minutes**. Ship the
park-and-advance disposition, the bounded retry ladder, and the readiness gate.
Treat "make a frame fit in two hours" as a separate, authorised, structural
change to the ladder — §11.

## 1. Measured baseline

### 1.1 Queue remainder (authoritative, not remembered)

Derived read-only by replicating `futon3c.apm.open-problem-queue/derive-queue`
against the corpus revision pinned in the active frame, rather than trusting any
cached count. `queue-state.edn` carries `:next-index 39`; its
`:apm-problem-queue` key is empty, so the queue is *derived*, and a remainder
can only be computed against a stated revision.

| corpus revision | `status.json` files | selected (open, non-topology, non-excluded) | remainder at `:next-index 39` |
|---|---|---|---|
| `9fa428f7a7bb` — pinned by active frame **f84** | 475 | **123** | **84** |
| `c82457269673` — current `master` | 475 | 103 | 64 |

Exclusions at the pinned revision: 267 `:not-open`, 79 `:topology`, 4
`:defective`, 2 `:construction-blocked`.

> **The measured remainder is 84.** The remembered figure of ~150 is wrong by a
> factor of ~1.8. Note also that the two rows differ by 20: 22 problems became
> `:not-open` between the pin and current master. **Any throughput projection
> must state which revision it counts against**, and a re-pin mid-campaign
> silently changes the denominator.

### 1.2 Frame wall-clock, all 39 frames (f46–f84)

Measured as last-event minus first-event in each frame's `ledger.edn`.

```
n=39   min 23   p25 91   MEDIAN 139   p75 269   p90 781   max 1317   mean 285   (minutes)
≤120 min: 14/39 (36%)      >240 min: 11      >600 min: 7
```

Closed frames only (`:frame/closed` present), n=19:

```
min 101   p25 127   MEDIAN 172   p75 326   max 1317
≤120 min: 2/19
```

Ledger event census: `:obligation/claimed` 406, `:frame/advanced` 303,
`:frame/opened` 39, `:frame/closed` 19, `:frame/stopped` 6.
**19 closed / 39 opened = 49% close rate**, matching `:parked` 14 in
`queue-state.edn`.

Inter-frame gaps: median **2 min**, max 142, total idle 433 min across the
campaign. **Dispatch latency is not the problem.** The time is inside frames.

### 1.3 Per-phase cost — where the 126 minutes goes

| phase | n | median | p75 | p90 |
|---|---:|---:|---:|---:|
| `:solve` | 39 | 19.3 | 46.1 | **100.1** |
| `:student-attempt-1` | 38 | 19.5 | 27.3 | 41.3 |
| `:student-attempt-2` | 26 | 19.4 | 26.1 | 57.5 |
| `:student-attempt-3` | 20 | 17.3 | 30.2 | 42.1 |
| `:guide-intervention-1` | 30 | 13.3 | 18.8 | 63.6 |
| `:guide-intervention-2` | 22 | 7.6 | 10.5 | 13.9 |
| `:promote-solver` | 37 | 10.9 | 19.2 | **125.3** |
| `:scribe-reduce` | 19 | 11.1 | 20.5 | 29.7 |
| `:preflight` | 39 | 2.3 | 3.1 | 10.7 |
| `:verify` | 39 | 2.2 | 2.4 | 3.3 |
| `:close-frame` | 19 | 2.7 | 8.3 | 16.8 |
| **sum of medians** | | **126.0** | | |

Two facts drive the whole design:

1. **The learning ladder is 61% of a frame.** `student-attempt-{1,2,3}` +
   `guide-intervention-{1,2}` = 77.1 min of the 126-min median path.
2. **`:promote-solver` has a pathological tail** — median 10.9 but p90 **125.3**.
   That is a promotion/store latency defect, not model work, and unlike the
   ladder it can be fixed without touching the experiment.

## 2. The unattended-progress contract

### 2.1 What is guaranteed

For an unattended window of length **W**, with the readiness gate of §8 passed
at **T₀**:

- **G1 (Advance).** No single frame consumes more than `FRAME_BUDGET` of
  wall-clock. At `FRAME_BUDGET`, the frame reaches a terminal disposition —
  `:closed`, `:parked`, or `:stopped` — and the queue cursor advances.
- **G2 (Evidence).** Every terminal disposition, including a deadline park,
  carries the exact receipts, residuals, job ids, phase cursor and reason that
  were true at the moment it was taken. No disposition is ever synthesised from
  absence.
- **G3 (Detection).** Loss of semantic progress is detected within
  `internal-progress-max-ms` (currently 5 min) for internal stalls, or
  `deadline + external-deadline-grace-ms` (2 min) for external jobs. This is
  already guaranteed by `futon3c.apm.semantic-progress-watchdog`; this note does
  not weaken it.
- **G4 (Bounded fault handling).** Known-transient substrate faults are retried
  under a finite budget with backoff. Retries cannot loop unboundedly and cannot
  consume more than `RETRY_BUDGET` of the frame's own budget.
- **G5 (Liveness).** While campaign-fatal conditions are absent, the campaign
  makes at least one queue advance per `FRAME_BUDGET + DISPATCH_GRACE`.

### 2.2 What is *not* guaranteed, and must not be claimed

- **Not guaranteed: that any problem closes.** G1 guarantees advance, not yield.
  At a 120-min budget the honest expected yield is ~0.2 closes per night.
- **Not guaranteed: progress past a campaign-fatal fault.** §5 lists conditions
  where stopping is correct. An unattended window can legitimately end early.
- **Not guaranteed: that a parked frame was hopeless.** A park at deadline is a
  statement about the clock, not about the mathematics. It must never be
  recorded as a mathematical verdict.

### 2.3 The prohibition this design must not violate

`TN-apm-watcher.md` is explicit: *"**void a frame**, ever … Void-and-advance was
explicitly rejected."*

**Park-and-advance is not void-and-advance**, and the distinction is the crux of
this note:

| | void-and-advance (rejected) | park-and-advance (proposed) |
|---|---|---|
| frame record | erased/ignored | retained, terminal, complete |
| receipts | discarded | recorded exactly as observed |
| problem | consumed | remains **selectable** |
| claim made | "this frame didn't happen" | "this frame ran out of clock at phase P" |

The second row of that table is already true of the existing park: per
`open_problem_queue.clj`, *"the park lives in queue-state.edn, which this
selection never reads"* — a parked problem is re-admitted by the next
derivation. Park-and-advance therefore loses no work; it defers it.

## 3. FRAME budget: start, deadline, heartbeat, terminal outcomes

Distinct from per-turn and per-job timeouts (§7).

- **Start.** `T_frame_start` := the `:event/at` of the frame's `:frame/opened`
  event. Not mint time, not dispatch time — the ledger event, so the budget is
  reconstructible from durable evidence alone.
- **Deadline.** `T_deadline` := `T_frame_start + FRAME_BUDGET`. Persisted into
  the frame manifest at open, so it survives coordinator restart. A restarted
  coordinator must **not** recompute it from its own start time.
- **Heartbeat.** `:frame/heartbeat` — `{:at, :cursor, :budget/deadline,
  :budget/elapsed-ms, :progress/last-change-at, :retry/count}`, written every
  `default-period-ms` (10s) to the frame directory. The cursor is exactly
  `semantic-progress-watchdog/cursor-keys`:
  `[:frame-id :phase :attempt-ordinal :obligation/status :active-job-id
  :last-committed-event-id]`. **The heartbeat is a witness, not a guarantee** —
  §4 turns it into one.
- **Terminal outcomes**, exactly three, all append-only:
  - `:frame/closed` — normal completion; unchanged semantics.
  - `:frame/stopped` — campaign-fatal (§5). Queue does **not** advance.
  - `:frame/parked` — frame-local fault or deadline breach (§4). Queue advances.
    `:park/reason` ∈ `{:frame-deadline-exceeded, :retry-budget-exhausted,
    :frame-local-fault}` plus the originating watchdog `:code`.

**Invariant F1.** Every frame has exactly one terminal event.
**Invariant F2.** `T_deadline` is written once and never rewritten.
**Invariant F3.** A `:frame/parked` event is only valid if it carries a cursor
whose `:last-committed-event-id` exists in that frame's ledger.

## 4. Evidence-preserving bounded park-and-advance

On a frame-local fault or deadline breach, in strict order:

1. **Quiesce.** Stop claiming new obligations for the frame. Do not cancel an
   in-flight external job; record it.
2. **Reconcile before judging.** Run the authenticated-submission reconciliation
   of §6 *first*. **f83 is the precedent: it sat blocked ~19 hours with a valid
   typed completion behind a wrapper still marked `:running`.** Parking f83
   without reconciling would have discarded a real result. Reconciliation is
   mandatory and precedes every park.
3. **If reconciliation yields a completion**, apply it, advance the phase,
   and re-evaluate the budget. A frame rescued this way is not parked.
4. **Otherwise snapshot**, into the frame directory:
   - every certificate/receipt id already minted;
   - residual Lean state and `sorry` counts as of the last committed event;
   - the active job id, its endpoint, its declared deadline and last status;
   - the full progress cursor and `:progress/last-change-at`;
   - the retry ledger (§6);
   - `:park/reason` and the watchdog `:code`.
5. **Append `:frame/parked`.** Update `:parked` in `queue-state.edn`.
6. **Advance** `:next-index`, dispatch the next frame.

**Invariant P1.** A park never writes a `:frame/result` that asserts a
mathematical outcome. Deadline parks are `:frame/result :parked`, never
`:partial` and never `:closed`.
**Invariant P2.** A park never mutates or deletes a prior receipt, deposit,
terminal or verdict.
**Invariant P3.** A park never decrements a student attempt budget. An
apparatus-caused stop is not a student failure — this is guarantee **D5** in
`TN-codex3-apm-guarantee-register.md`, still open, and park-and-advance must not
make it worse.
**Invariant P4.** Unknown failures park. They never close. *No unknown becomes a
success.*

## 5. What must remain campaign-fatal

Park-and-advance is wrong where continuing would violate authority or
coherence. These stop the campaign (`:frame/stopped`, no advance):

- **Authority/identity faults** — `:failed-launch-audit`, unverifiable agent
  seat, a role acting without a valid capability. Advancing would produce
  frames whose provenance cannot be trusted.
- **Ledger/store integrity faults** — `:impossible-transition`,
  `:invalid-state`, expected-version conflict, digest mismatch, or a failed
  durable append. If the ledger cannot be trusted, neither can the park record,
  so park-and-advance is *unsound* here by construction.
- **Corpus re-pin mid-campaign** — the derived queue changing revision under a
  live `:next-index` (§1.1). The cursor would index a different queue.
- **Validation-path unavailability** — Lean toolchain or verify path down.
  Continuing manufactures unverified frames.
- **Repeated same-signature failure** — the existing
  `:consecutive-frame-failures` mechanism. Currently
  `{:classification :role-terminal-unrecoverable, :count 1, :last-frame-id f82}`.
  Proposed threshold: **3 consecutive parks sharing a signature** ⇒ stop. A
  systematic apparatus fault must not silently park the entire remaining 84.
- **Disk/store exhaustion** — below the §8 floor.

**Invariant C1.** Campaign-fatal conditions are evaluated *before* the park
path. A fatal condition can never be downgraded to a park.

## 6. Semantic-progress monitoring and known-transient faults

### 6.1 What must change (durable, not process liveness)

The progress cursor is already the right object and already exists. Progress
means a change in `[:frame-id :phase :attempt-ordinal :obligation/status
:active-job-id :last-committed-event-id]` **backed by a committed ledger
append**. A heartbeat tick, a scheduler tick count, or a process being alive are
**not** progress. `TN-apm-watcher.md` records `:regulator/status :running`
reading `:running` for nine minutes while ticks did not advance; the coordinator
currently shows `:regulator/ticks 4720`.

Existing thresholds in `semantic_progress_watchdog.clj`, to be retained
unchanged: `scheduler-claim-max-ms` 30s, `internal-progress-max-ms` 5 min,
`external-deadline-grace-ms` 2 min, `default-period-ms` 10s.

### 6.2 Distinguishing the five stall shapes

| observed | discriminator | disposition |
|---|---|---|
| legitimate active work | `:active-job-id` set, job polls `running`, within declared deadline | wait |
| **stranded wrapper** (f83) | wrapper `:running` **but** an authenticated typed submission exists for the job | **reconcile** (§6.3), then continue |
| **stopped coordinator** (f84) | `:regulator/ticks` static across ≥2 watchdog periods | restart regulator (bounded, §6.4); fatal if it will not start |
| **blocked promotion scan** | phase `:promote-solver`, cursor static, no active job | frame-local ⇒ park at deadline; also see the p90 125.3 defect in §1.3 |
| **queued turn** | job accepted, not started, no seat available | wait against declared deadline; missing deadline ⇒ `:external-job-deadline-missing` ⇒ fatal |

### 6.3 Reconcile-before-retry (mandatory)

Before *any* retry or park, for each `:active-job-id`: query the typed
submission store; if an authenticated completion exists, apply it and reconcile
the wrapper. **Never redispatch a job that already has an authenticated
submission** — that is how duplicate work and conflicting terminals are minted.
This is the f83 repair generalised into a precondition.

### 6.4 Bounded retry for known-transient substrate faults

Retryable, and *only* these: Codex responses-endpoint **404/5xx/timeout**
(f84's cause), connection reset, seat-unavailable, and transient store
contention.

```
attempt 1: immediate       attempt 2: +60s        attempt 3: +240s
RETRY_BUDGET   = 3 attempts OR 30 minutes, whichever first
```

- Every attempt appends to a durable `:retry/ledger` — timestamp, fault class,
  endpoint, job id, outcome. Retries are **evidence**, not silent.
- Backoff is capped; there is no unbounded loop.
- Retry time is charged against the frame budget. It cannot extend `T_deadline`.
- Budget exhausted ⇒ `:park/reason :retry-budget-exhausted`.
- A fault **not** on the retryable list is never retried — it parks or stops.

**Invariant R1.** Retry count is monotone and durable; a coordinator restart
resumes the count, never resets it.
**Invariant R2.** Reconciliation (§6.3) runs before every retry attempt.

## 7. Separating the four clocks

These are independent and must not be collapsed:

| clock | scope | value | on breach |
|---|---|---|---|
| **turn timeout** | one model turn | existing, unchanged | job-level retry |
| **job deadline** | one dispatched job | declared per job; **absence is fatal** (`:external-job-deadline-missing`) | reconcile, then retry ladder |
| **RETRY_BUDGET** | one frame's faults | 3 attempts / 30 min | park |
| **FRAME_BUDGET** | whole frame, all phases | **240 min** (see §0/§11) | park |
| **campaign liveness** | whole window | ≥1 advance per `FRAME_BUDGET + DISPATCH_GRACE` (median gap is 2 min; set `DISPATCH_GRACE` = 15 min) | alert; fatal only per §5 |

**Invariant S1.** `RETRY_BUDGET` ⊂ `FRAME_BUDGET`. Retries never extend a frame.
**Invariant S2.** No job deadline may exceed the frame's remaining budget; a job
dispatched with `T_now + job_deadline > T_deadline` is clamped at dispatch.
**Invariant S3.** One frame cannot consume the window. With a 240-min budget an
8-hour window admits ≥2 frames even in the worst case.

## 8. Unattended readiness gate

Run before sleep or flight. **Any FAIL ⇒ do not go unattended.** Read-only; no
state mutated.

| # | check | pass condition |
|---|---|---|
| 1 | coordinator | `:regulator/status :running` **and** `:regulator/ticks` advances across two 10s samples |
| 2 | scheduler | a tick claim completes within `scheduler-claim-max-ms` (30s) |
| 3 | queue | `derive-queue` returns `:ok true`; remainder > 0; revision equals the active frame's pin (§1.1) |
| 4 | watchdog | `semantic-progress-watchdog` reports `:status :watching`, not `:not-running`/`:halted` |
| 5 | alert delivery | a synthetic alert is delivered **and acknowledged end to end** — an undelivered alert is the f84 failure mode |
| 6 | disk | free space ≥ 20× the median frame's artifact footprint |
| 7 | store | append + read-back of a probe event; expected-version conflict detection verified |
| 8 | agent seats | every role — proctor, solver, guide, student, scribe — answers a liveness probe; **and the Codex responses endpoint returns 200 for a trivial request** (the exact f84 fault) |
| 9 | JVM/code state | loaded namespace shas match the `:conditions` registry in `queue-state.edn`; **no `:loaded? false` condition is required by the coming frames** (C-7, C-8, C-10 are currently `:loaded? false`) |
| 10 | budget sanity | `FRAME_BUDGET × expected frames ≤ W`; projected closes stated honestly (§10) |

Check 9 deserves emphasis: the campaign is running with three registered
conditions marked `:loaded? false`. Going unattended without resolving whether
they matter for the next frames means the frames are not the experiment anyone
thinks they are.

## 9. Fault-injection acceptance plan

Each is falsifiable, run against a synthetic campaign
(cf. `TN-apm-csquare-synthetic-campaign.md`), never against a live one.

| # | injected fault | required observable outcome |
|---|---|---|
| A1 | stranded running job **with** typed submission | reconciled and advanced; **no** park, **no** redispatch; wrapper reconciled ≤ 1 watchdog period |
| A2 | responses endpoint 404 | retry ladder 0s/60s/240s; recovery on any attempt ⇒ frame continues; retry ledger has 1 entry per attempt |
| A3 | responses endpoint timeout, permanent | budget exhausts at 3 attempts/30 min ⇒ `:park/reason :retry-budget-exhausted`; queue advances; campaign alive |
| A4 | missing submission (job gone, no receipt) | park with residuals; **no** invented receipt; `:frame/result :parked` |
| A5 | stuck job, heartbeat still ticking | cursor static ⇒ stall detected in ≤5 min; process liveness does **not** suppress detection |
| A6 | no semantic ledger movement, all subsystems "healthy" | `:internal-semantic-progress-stalled` in ≤5 min |
| A7 | coordinator stopped mid-frame | detected in ≤2 watchdog periods; bounded restart; **f84's 6h12 becomes ≤5 min** |
| A8 | promotion/store latency 130 min at `:promote-solver` | frame parks at `T_deadline` with promotion residuals intact; no partial promotion committed |
| A9 | queued turn, no seat | waits against declared job deadline; missing deadline ⇒ fatal, not an indefinite wait |
| A10 | deadline park then next-frame dispatch | `:frame/parked` appended, `:next-index` advances, next frame opens within `DISPATCH_GRACE`, parked problem still selectable by `derive-queue` |
| A11 | ledger append fails during park | campaign **stops**; no half-written park (§5 integrity) |
| A12 | 3 consecutive same-signature parks | campaign stops with the signature recorded |

**A10 and A11 are the pair that matter most.** A10 proves park-and-advance
preserves the problem; A11 proves it cannot forge a record when the store is
untrustworthy.

## 10. Operational observability — one status view

A single read-only view, no interpretation required:

```
campaign jit-all-open-v2      window 22:30–06:30   unattended: HOLDS
frame    f85  b97A05          phase :student-attempt-2  (7 of 11)
elapsed  096m / 240m budget   deadline 02:14Z
progress last change 3m ago   cursor …/:student-attempt-2/2/:claimed/job-8814/evt-2291
job      job-8814 codex/responses   running 4m   job deadline 02:05Z
retries  0 / 3        retry window unused
queue    39 done · 84 remaining @ 9fa428f7a7bb
rate     172m median/frame → projected 2.8 frames, ~1.0 closes this window
guarantee HOLDS   (watchdog :watching, alerts ack'd 21:12Z, disk 412G)
```

`unattended: HOLDS` is false the moment any §8 check fails, the watchdog is not
`:watching`, or alert delivery is unacknowledged. **It must degrade to `DOES NOT
HOLD` rather than go stale** — a status line that cannot say "no" is the f84
failure repeated in a nicer font.

### Projected throughput

Empirical model over the 39 observed frames: a frame closes iff it closed
historically *and* fits the budget; otherwise it parks at the budget. Gap = 2
min (measured median). Window = 480 min.

| FRAME_BUDGET | close rate | park rate | min/frame | frames/8h | **closes/8h** |
|---:|---:|---:|---:|---:|---:|
| 120 | 5% | 95% | 106 | 4.5 | **0.2** |
| 150 | 21% | 79% | 122 | 3.9 | 0.8 |
| 180 | 26% | 74% | 134 | 3.6 | 0.9 |
| **240** | **31%** | **69%** | **153** | **3.1** | **1.0** |
| 300 | 36% | 64% | 168 | 2.9 | 1.0 |
| 360 | 38% | 62% | 181 | 2.6 | 1.0 |

Closes/8h saturates at ~1.0 from 240 min onward; below 240 it falls off a
cliff. **240 minutes is the knee.** At that rate the measured remainder of 84
problems is ~84 nights of unattended running — which is the real finding here,
and an argument for §11 rather than for any timeout value.

## 11. If two hours is genuinely required

Then these structural costs must change. Timeouts cannot do it; 126 > 120
before any fault.

1. **Shorten the ladder (necessary).** Dropping `student-attempt-3` and
   `guide-intervention-2` removes 24.9 min of median, giving a ~101-min median
   path that fits 120 with headroom. **This changes what the experiment
   measures** and is explicitly forbidden to the watcher without authorisation.
   It is a scientific decision for Joe, not an ops lever.
2. **Fix the `:promote-solver` tail (pure defect, no authorisation needed).**
   Median 10.9, p90 125.3. A single promotion scan can exceed the entire frame
   budget on its own. This should be fixed regardless of what budget is chosen.
3. **Bound `:solve` (p90 100.1).** Solver rounds need their own job deadline
   inside the frame budget, or one solve can eat a two-hour frame alone.
4. **Parallelism (structural alternative).** Nothing above changes per-frame
   latency; running 2–3 frames concurrently multiplies closes/8h without
   touching the ladder. This is a much larger change — the campaign is
   sequential by construction, `:next-index` is a scalar cursor, and shared JVM
   state is a live hazard (§8 check 9). Out of scope here; flagged as the only
   route to a step change in yield.

## 12. Migration and rollback

Sequenced so each stage is independently reversible:

- **M0 — observe only.** Ship the heartbeat, the retry ledger and the §10 status
  view. Write records; change no control flow. *Rollback: stop writing.*
- **M1 — readiness gate.** §8 as a read-only preflight. *Rollback: skip it.*
- **M2 — reconcile-before-retry.** §6.3 as a precondition. Strictly reduces
  duplicate dispatch. *Rollback: revert precondition.*
- **M3 — bounded retry.** §6.4 for the listed transient classes only.
  *Rollback: retry budget 0, i.e. current behaviour.*
- **M4 — frame budget, alerting only.** Compute `T_deadline`, alert on breach,
  **do not park**. Run ≥5 frames; compare predicted vs actual parks against
  §1.2. *Rollback: stop alerting.*
- **M5 — park-and-advance.** Enable only after M4's predictions match. *Rollback:
  disable the park disposition; frames revert to blocking, which is today's
  behaviour.*

**Rollback triggers (any one):** a park is minted without complete receipts; a
frame closes on unknown failure; a parked problem is not re-admitted by
`derive-queue`; duplicate dispatch against an authenticated submission; a
campaign-fatal condition downgraded to a park; close rate falls below the §10
model for the chosen budget across ≥5 frames.

**Non-negotiable across every stage.** The proposal must not weaken validation,
invent receipts, erase frames, or turn unknown failures into successes. Each has
an invariant above — P2, P4, the void/park distinction in §2.3, and P1 — and
each has an acceptance test in §9.

## 13. Invariant summary

- **F1–F3** one terminal event; immutable deadline; parks cite a real event id.
- **P1–P4** parks assert clock not mathematics; never mutate prior evidence;
  never charge a student; unknown parks, never closes.
- **C1** fatal is evaluated before park and never downgraded.
- **R1–R2** retries are monotone, durable, and always preceded by reconciliation.
- **S1–S3** retry ⊂ frame; jobs clamped to frame; no frame eats the window.
