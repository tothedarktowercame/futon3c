# Excursion: E-repl-continuations — an agent parks its own REPL turn until its dependencies return, then self-resumes

**Date:** 2026-06-30
**Status:** IDENTIFY ratified (Joe, 2026-06-30) → **MAP in progress.** (Lifecycle: `futon4/holes/mission-lifecycle.md`; MAP = survey existing work, facts not design. Joe flagged this explicitly: *"be sure to include a formal MAP stage … this is the kind of thing where we want to be careful"* — much of the execution substrate already exists.)
**Repo:** futon3c — the turn/execution path: `transport/http.clj` (`invoke-executor`, `handle-bell`, autobell-back, async-accept), `agency/turn_queue.clj` (durable per-agent queue + drainer-v2), `agents/tickle*` (self-trigger), `agency/clock_lineage.clj` (durable per-agent state precedent).
**Spawned from:** a REPL design turn (Joe, 2026-06-30) loosely connected to **C-cascade-real** — the cascade is a DAG of cross-agent dependencies; if each agent could park-until-deps-ready and self-resume, the cascade flows autonomously instead of Joe hand-cranking each step. Sibling of [[E-crossed-bells]] (makes bell *threads* visible) — this makes a turn's *continuation* durable.

## HEAD (one line)
At turn-end an agent registers a **continuation** — *"park me until {the bellbacks / jobs I'm waiting on} return (± a timer), then re-invoke me with those results in hand"* — so a multi-agent operation is one resumable turn, not a string of amnesiac externally-triggered turns. Self-resume = autobell-back pointed back at yourself, gated on a **join**.

## 1. IDENTIFY — the gap (shape-first)

autobell-back already gives us the edge *"recipient completes → bell routes to caller, re-invoking caller"* (verified live this session: claude-4's co-verify bellbacks chained my turns without Joe cranking them). The proposal turns that edge **reflexive**. Three pieces are the candidate delta — **but IDENTIFY's first duty is to check how much already exists, because Joe's recollection is that claude-4 already built the JVM-backed execution path** (*"confirm `invoke-executor` is `defonce` so a reload won't orphan the thread pool"*). MAP (§2) confirms: **it did.** So the gap is narrower than "build a continuation engine."

The candidate-missing pieces, to be confirmed/refuted in MAP:
1. **The join.** Dispatch 3 bells → today each bellback may wake the caller *separately* and *partially*; "resume once, when all 3 are back" is `parallel()`-barrier semantics at REPL-turn granularity. *(Open: does drainer-v2 already coalesce? §2.C-Q1.)*
2. **Self-targeting without an external bell.** *"Wake me when this bg job exits"* / *"wake me in 10 min to run the campaign producer."* Joe's note: this *"was supposed to have been solved"* by the JVM-backed path — **so MAP must locate the existing entry point, not assume one is missing.** *(§2.C-Q2.)*
3. **A durable continuation payload.** The kangaroo pouch is LRU-evicted between turns, so a resumed turn reconstructs intent from the summary. A parked continuation should *carry* what it was waiting for across the gap. *(Open: does the durable queue already persist enough? §2.C-Q3.)*

**The tension (why care):** the execution substrate is present but *invisible* — *"it clearly hasn't become highly visible as a path yet"* (Joe). An undiscovered capability gets **reinvented** (I-4 violation) or **left unused**. So this excursion's value may be mostly **surfacing + ergonomics + the thin missing join/payload**, not new engine code.

**IDENTIFY exit:** the gap is named — *an agent cannot ergonomically park its own REPL turn on a join of expected results and self-resume, even though the durable-execution substrate to do so largely exists but is unsurfaced.* ✓ Ratified (Joe, 2026-06-30) → MAP.

## 2. MAP — what already exists (evidence, file:line) + the open questions

> MAP's object: **survey the substrate before designing.** Facts with pointers, so DERIVE pins only the real delta. (Audit of the futon3c turn/execution path, 2026-06-30.)

### A. The execution substrate that EXISTS (claude-4 / M-agency-hardening, Car-3)

| piece | where | what it gives us |
|---|---|---|
| **`invoke-executor`** | `transport/http.clj:122` — `(defonce ^ExecutorService invoke-executor …)`, 8 lanes (bumped 4→8 2026-06-12) | the JVM-backed execution pool; `(.submit invoke-executor …)` runs an invoke off the request thread, parented to the long-lived JVM (survives pouch teardown). **This is the path Joe referenced.** |
| **durable per-agent turn queue** | `agency/turn_queue.clj` — `accept!`, `accept-and-drain!`, persisted to `/tmp/futon3c-durable-turn-queue.edn`, OOM-resume-safe, flag default-ON since 2026-06-14 | a FIFO of pending turns per agent that **survives teardown**; persists routing metadata (from/to/surface/msg-id/seq/accepted-at). |
| **drainer-v2** | `turn_queue.clj:294+` — dedicated per-agent **daemon drainer thread** | each agent drains on its own thread (no shared invoke-lane park); a bell can enqueue and return without holding a lane. Validated by `drainer-model-test`. |
| **`:tickle` self-trigger** | `agents/tickle_logic.clj` (+ runtime `agents.tickle`); `registry.clj:673` *"conductor/tickle + invoke-executor"* | a synthetic self-triggering agent (today: watchdog/escalation/page facts) — the existing *"fire a turn with no human inbound"* mechanism. |
| **autobell-back** | `transport/http.clj` `handle-bell` + completion-bell routing; the `--from` caller edge | completion→reinvoke across agents. **The reflexive case is the same wire pointed at self.** Verified live this session. |
| **durable per-agent STATE** | `agency/clock_lineage.clj` (this session) | precedent for persisting per-agent state to substrate-2, edge-triggered, survives teardown — the natural shape for an `agent/parked-on` continuation record. |
| **durable helper-work** | `scripts/bg.py` / `agency.bg-process` (JVM-parented); `ScheduledExecutorService` daemons (`watcher/multi`, `watcher/scope_reingest`) | the "wake me when a *job* exits" half — bg jobs already outlive turns and are tailable. |

### B. The reframe MAP forces
The **engine is built.** What is *not* obviously present is: (i) a single ergonomic **agent-facing entry** to say "park me on these deps and resume," (ii) **join** semantics (resume-once-on-all), (iii) a **continuation payload** carried across the gap, (iv) a **dependency condition** (park-on-bellbacks/jobs) rather than plain FIFO/timer. The likely deliverable is **wiring + surfacing + a thin join**, layered on `turn_queue` + `invoke-executor` + the `--from` edge — *not* a new executor.

### C. MAP questions — ANSWERED with evidence (2026-06-30)
- **Q1 — join vs wake-per-entry → WAKE-PER-ENTRY (no join).** `drain!` (`turn_queue.clj:259-280`) loops `pop-next!` and runs entries strictly one-at-a-time FIFO, each with its own `process-fn`; `mark-terminal!` (`:241-247`) delivers each entry's result to its OWN waiter promise (keyed by entry id). No coalescing, no "wait for N present." **→ the join is genuinely NEW.**
- **Q2 — self-enqueue entry point → EXISTS (primitive), missing only the affordance.** The enqueue primitive is **`turn-queue/accept-async!`**, used by auto-bellback (`http.clj:553`), the bell handler (`:3071`), and the REPL path (`:3246`). An agent can self-enqueue TODAY via **self-bell** (POST `/api/alpha/bell` with `agent-id` = itself) — no `agent-id == caller` guard found — which enqueues + runs on its drainer-v2 thread. **Bonus find: `FUTON3C_REPL_THROUGH_QUEUE` (`http.clj:3156,3239`) already routes a REPL turn THROUGH the durable queue + drainer-v2** — the REPL turn can already *be* a durable queue entry. *(One live-confirm owed before relying on self-bell: that no loop-guard rejects `agent-id == caller`.)*
- **Q3 — payload sufficiency → PARTIAL.** The entry persists `:prompt` + routing meta (`normalized-entry :134-152`, `accept! :154-202`) and survives teardown — but the **`:process-fn` closure + `:waiter` live in RAM-only `defonce` atoms** (`!processors`/`!waiters`, dissoc'd at `:166`). So a continuation that survives an OOM-resume must encode intent in the **persisted prompt/entry, not a closure.**
- **Q4 — dependency condition → ABSENT.** `pop-next!` (`:218-227`) is plain FIFO `first`; a queued entry is immediately drainable. Nothing releases on "{bell-ids} complete." **→ the condition-gated release is the genuine NEW piece.**
- **Q5 — tickle runtime → ADJACENT only.** `agents/tickle.clj` is a **watchdog/pager** (`scan-activity` → `detect-stalls` → `page-agent!` via `ring-test-bell!`/IRC → `escalate!`), not a continuation timer. It proves a periodic conductor loop that *fires bells at agents* exists — but it nudges *stalled* agents; it carries no parked continuation.

### MAP EXIT VERDICT — what exists vs what's missing
**The engine exists** (claude-4 / M-agency-hardening, Car-3): a durable per-agent FIFO turn queue (OOM-resume-safe), per-agent serialized execution via **drainer-v2** daemon threads on the **`invoke-executor`** pool, the **`accept-async!`** enqueue primitive, **REPL-through-queue** plumbing, self-enqueue via self-bell, durable **prompt** persistence, and autobell-back completion→reinvoke. **The delta is small and well-bounded:** a **condition-gated, join-aware release** layered on `accept-async!`/drainer-v2 — (i) park-until-`{bell/job-ids}`-complete (Q4, new), (ii) join-on-all before one resume (Q1, new), (iii) continuation encoded in the **durable prompt** not a closure (Q3), (iv) a thin agent-facing "park" affordance (Q2), released **edge-triggered** off completion (off the Arxana Clock; Q5's periodic loop is a fallback-timer precedent only), plus a **runaway backstop**. **No new executor, no new queue, no clock load.** → MAP closed; ready for DERIVE.

## 3. DERIVE — the design (2026-06-30, on MAP-confirmed substrate)

**Builds on, does not rebuild:** `accept-async!` enqueue + drainer-v2 serialized execution on `invoke-executor`; **self-bell** as the self-enqueue affordance (live-confirmed 2026-06-30: a `--from claude-1 --to claude-1` bell was `accepted`/`queued`, caller==agent-id, no loop-guard, serialized behind the live turn); durable `:prompt` persistence; autobell-back completion→reinvoke; clock-lineage durable-per-agent-state.

### 3.1 The continuation record — `agent/parked-on` (durable, clock-lineage-style)
Written to substrate-2 as a hyperedge so it survives pouch teardown (Q3: intent lives in **durable data, not a closure**):
```
agent/parked-on  endpoints [agent:<id>]
  props {:session <sid>  :parked-at-ms <t>
         :awaiting #{<bell/job-id> …}     ; the dispatched deps this turn is waiting on
         :arrived  {<id> <result-summary>}; filled as each completes (the join accumulator)
         :timer-due-ms <t?>               ; optional wake time (no-dep "resume me in N")
         :payload  <prompt fragment>      ; what to say to the resumed agent — the continuation
         :budget   {:resumes-left N :max-depth D :deadline-ms T}   ; the backstop (3.4)
         :released? false}                ; single-fire guard (retract-on-release, clock-lineage pattern)
```

### 3.2 The agent-facing affordance (Q2 — the missing surface, not mechanism)
A thin `POST /api/alpha/park` (or `turn-queue/park!`): `{:agent :awaiting [ids] :timer-ms :payload :budget}`. The agent ends a turn with *"park me on these deps, resume with results"* instead of hand-rolling a self-bell. Internally: write the `parked-on` record + **immediately reconcile** against the invoke-jobs ledger for any awaited id already terminal (closes the lost-wakeup race — a dep that finished between dispatch and park).

### 3.3 The edge-triggered JOIN release (Q1 + Q4 — the genuine new logic)
Hook the **same completion path that already drives autobell-back** (`mark-terminal!` / `run-finalizer!` / `enqueue-auto-bellback!`). On any bell/job reaching terminal state, a sibling of `enqueue-auto-bellback!` — keyed on **awaited-id membership** instead of caller — runs:
1. for each `parked-on` whose `:awaiting` contains this id: move it into `:arrived`, drop from `:awaiting`;
2. **when `:awaiting` becomes ∅ (the JOIN):** atomically set `:released? true` (single-fire), decrement budget, and enqueue **one** resume via `accept-async!` with prompt = `:payload` + the joined `:arrived` results.

**Edge-triggered off completion ⇒ zero Arxana Clock load.** This is autobell-back generalized: autobell-back is the n=1, caller-keyed case; `parked-on` is n≥1, awaited-id-keyed, with a durable payload + budget. (We do **not** refactor the working autobell-back; `parked-on` sits alongside.)

### 3.4 The runaway backstop (hard constraint, in from day one)
`:budget {:resumes-left N :max-depth D :deadline-ms T}` on every record. Each resume decrements `:resumes-left` and the chain's `:max-depth`; at zero, or past `:deadline-ms`, the release is **suppressed** and the record terminates as `:budget-exhausted` (optionally one final notify). Plus: a per-agent cap on concurrent `parked-on`, and the `:released?` single-fire guard so a doubly-delivered completion can't double-resume. *Without this a self-resume chain burns tokens forever — the `/loop` clamp lesson.*

### 3.5 The optional timer arm (the only scheduler touch)
For no-dep "resume me in N min" (e.g. the C-cascade-real campaign producer): **one shared** `ScheduledExecutorService` tick (the `watcher/multi` / `scope_reingest` daemon pattern, **not** cyder, **not** per-agent) sweeps `parked-on` for due `:timer-due-ms` and releases them through the same 3.3 path. One coalesced timer, not a clock.

## 4. ARGUE — central decision
**IF** an agent must park its REPL turn on a join of dispatched deps and self-resume durably,
**HOWEVER** the execution substrate (durable queue, drainer-v2, self-enqueue, autobell-back) already exists and MAP showed only the *condition-gated join* + *durable payload* are missing,
**THEN** layer a durable `agent/parked-on` record + an edge-triggered join release on the existing completion path (sibling of `enqueue-auto-bellback!`), released off bellback/job *arrival*, with a per-record resume-budget,
**BECAUSE** reusing the proven substrate (I-4) keeps the build to the genuine delta, edge-triggering keeps it off the Arxana Clock (Joe's constraint), and the budget is the non-negotiable ceiling self-resume requires.

### 4.1 Hook site → `finalize-invoke-job!` (resolved by evidence, 2026-06-30)
**IF** the join release must catch every awaited dep exactly once,
**HOWEVER** there are three terminal entry paths (sync invoke, async bell, queue-submit),
**THEN** hook the release inside **`finalize-invoke-job!` (`http.clj:738`), beside the `auto-bellback-request` computation** — NOT in `turn_queue/mark-terminal!`,
**BECAUSE** `finalize-invoke-job!` is the **single chokepoint** all six terminal call-sites route through (`:2725,2758,2794,2812,3078,3094`), and it is *already* where autobell-back fires — so the join inherits autobell-back's proven, live-verified coverage. `mark-terminal!` is queue-internal and would miss the sync direct-invoke path.

### 4.2 Result payload → reuse the ledger's `:result-summary` (resolved)
**IF** the resumed prompt must carry each dep's result without blowing the context,
**HOWEVER** raw results can be whole transcripts,
**THEN** the `:arrived` accumulator stores the job's existing **`:result-summary`** (already computed by `summarize-result-text` inside `finalize-invoke-job!`),
**BECAUSE** the size budget is *already solved* at the chokepoint — no new summarization, and the resumed agent gets bounded summaries by construction.

### 4.3 `park!` surface → HTTP endpoint over an in-process primitive (resolved)
**IF** the parker is an agent on an ephemeral pouch,
**HOWEVER** an in-process `turn-queue/park!` is only reachable from inside the JVM,
**THEN** expose **`POST /api/alpha/park`** as the affordance, delegating to a thin in-process `park!` (write record + reconcile),
**BECAUSE** agents already self-enqueue over HTTP (`/api/alpha/bell`); park must be reachable from any surface the same way — endpoint = surface, primitive = mechanism, exactly mirroring `handle-bell → accept-async!`.

**ARGUE closed** — the design (§3) rests on a single proven chokepoint, reuses the existing summary + enqueue primitives, and adds only the durable `parked-on` record, the dep-keyed release branch, the `park!` endpoint, and the budget. → ready for VERIFY/INSTANTIATE.

## 5. VERIFY — the five hard cases walked against the code (2026-06-30)

Grounding facts confirmed by reading: `update-invoke-jobs-ledger!` **persists on every update** (`swap!` → `persist-invoke-jobs-ledger!`, no lazy window); a terminal job stays in `[:jobs job-id]` with `:state` + `:result-summary`; `recover-inflight-jobs` marks every `queued`/`running` job **terminal** (`failed/worker-lost-on-restart`) on boot; **I-0 ⇒ exactly one durable ledger** in the one JVM, so a bell to *any* agent finalizes in the *same* persisted ledger.

| case | verdict | why / refinement forced |
|---|---|---|
| **1. lost-wakeup** (dep finishes before `park!`) | **HOLDS** | ledger persists each update + retains terminal jobs by id; `park!`'s reconcile reads `[:jobs id]`, folds an already-terminal dep's `:result-summary` into `:arrived` — if all deps already terminal, release fires immediately. No code gap. |
| **2. double-delivery** (release fires twice for one id) | **HOLDS w/ R1** | needs the decrement + `:released?` flip to be ONE atomic swap — mirror the existing *compute-inside-swap, enqueue-after* pattern (the `bellback-request` atom). **R1: an in-RAM transactional `parked-on` index** (durable substrate-2 record as backing, written after). Single winner by construction. |
| **3. budget exhaustion** | **HOLDS w/ R2** | **R2: release AND budget-exhausted must RETRACT the record** (clock-lineage end-valid-time) + drop it from the index — not merely mark it — else a zombie keeps catching late completions. |
| **4. teardown mid-park** | **HOLDS w/ R3** | durable record survives (substrate-2, verified this session); ledger survives + `recover-inflight-jobs` gives EVERY awaited id a terminal state on boot (incl. `failed`); I-0 means cross-agent deps are in that same ledger. **R3: a boot rehydrate+reconcile hook** (sibling of `recover-inflight-jobs`) — load non-released `parked-on`, fold now-terminal awaited ids into `:arrived` (treat `failed` as *arrived-with-failure*, not hang), release any join completed during downtime. |
| **5. dep never completes** | **HOLDS w/ R4** | primary liveness is the invoke **`timeout-ms`** → finalize-as-failed → arrival (so most "hangs" self-resolve). **R4: the §3.5 timer sweep is REQUIRED, not optional** — it enforces `:deadline-ms` as the residual backstop (force-terminate past-deadline records). Reclassify §3.5: *deadline sweep (required) + timer-resume (optional)*. |

**VERIFY verdict: the design is sound on all five**, conditional on folding **R1–R4** into INSTANTIATE. Two of them (R1 atomic-single-fire, R3 boot-reconcile) are the load-bearing ones; both reuse existing proven patterns (the bellback-request swap; `recover-inflight-jobs`). Notably **I-0 is what makes cross-agent joins tractable** — one JVM, one durable ledger, so every awaited dep (whoever it was belled to) reconciles uniformly. Nothing here breaks the design; it sharpens it.

### Build checklist (for INSTANTIATE / a scoped handoff)
1. `agent/parked-on` substrate-2 record (§3.1) + **R1 in-RAM index** mirror.
2. `park!` in-process primitive + `POST /api/alpha/park` (§3.2/§4.3); reconcile-on-park vs the ledger (case 1).
3. dep-keyed release branch in **`finalize-invoke-job!`** beside `auto-bellback-request` (§4.1); atomic single-fire (R1); reuse `:result-summary` (§4.2).
4. **R2** retract-on-release/exhaustion; **R3** boot rehydrate+reconcile hook; **R4** required deadline sweep + optional timer-resume on one shared `ScheduledExecutorService` (§3.5).
5. budget (§3.4) on every record; per-agent concurrent-`parked-on` cap.
6. tests: the five VERIFY cases as unit/integration cases (model-test style, like `drainer-model-test`) BEFORE live :7071.

## 6. INSTANTIATE — Car 1 landed: the testable core (2026-06-30, claude-1 direct — Codex on vacation)

**Design refinement at INSTANTIATE:** the continuation store persists **disk-backed edn** (`spit`/`slurp`, the `invoke-jobs-ledger` / `turn_queue` pattern), **NOT substrate-2** — so it survives pouch teardown with **zero :7071 write** (stays inside the sanctioned envelope) and matches the sibling durable stores. A substrate-2 hyperedge projection for WebArxana visibility is deferred to a later, optional layer; disk is the source of truth.

**Landed — `agency/parked_on.clj` + `agency/parked_on_test.clj`:**
- `park!` (§3.2) with reconcile-on-park (case 1); `note-completion!` — the dep-keyed JOIN release with atomic single-fire via `swap-vals!` + dep-index consumption (R1, case 2); release/budget-exhaust both **retract** the record (R2, case 3); `rehydrate!` boot-reconcile (R3, case 4); `sweep-deadlines!` (R4, case 5) for the required deadline backstop + due timer parks; per-record `:budget` (§3.4).
- **Dependency-injected** (`resume!` / `ledger-lookup` / `now-ms`) so the five cases are unit-testable with no live JVM and no :7071.
- **Gates cleared:** clj-kondo 0/0 · check-parens OK · **7 tests / 18 assertions, 0 fail** — each of VERIFY cases 1–5 is an executable test, plus the core 2-dep join and the degenerate empty-join.

**Car 2 — hot-path wiring (remaining, done carefully via Drawbridge, no JVM restart):**
1. dep-keyed release hook in **`finalize-invoke-job!`** beside `auto-bellback-request` (§4.1) → calls `parked-on/note-completion!` with the job-id + the already-computed `:result-summary` (§4.2); exposed as a dynamic-var hook mirroring `*enqueue-auto-bellback!*` so the http.clj edit is one line + a require.
2. real `resume!` = assemble `:payload` + joined `:arrived` summaries → `turn-queue/accept-async!` (self-enqueue, live-confirmed).
3. `POST /api/alpha/park` thin wrapper over `park!` (§4.3).
4. boot call to `rehydrate!` (sibling of `recover-inflight-jobs`) + one shared `ScheduledExecutorService` daemon ticking `sweep-deadlines!` (NOT cyder).
5. live verify: park claude-1 on a real bell → completion fires the resume.

## Cross-links
- [[E-crossed-bells]] — sibling; transactional transport + reply-routes (the substrate this rides). Confirms durable queue + drainer-v2 made transport transactional.
- [[M-points-de-fuite]] — the live-session render; an autonomously-resuming agent is a cleaner phase-portrait source.
- **C-cascade-real** — the motivating consumer: cascade DAG flows autonomously if agents park-until-deps-ready.
- **M-agency-hardening** — owns the durable queue / drainer-v2 / OOM-resume work this builds on.
- `agency/clock_lineage.clj` — durable-per-agent-state precedent for the `parked-on` record.
