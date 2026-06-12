# M-agency-hardening — live datapoints (2026-06-10 claude-4 ↔ claude-3 co-build)

Source: handed to claude-6 via bell during a long claude-4↔claude-3 session.
"The session itself is the test case." Filed + verified by claude-6 (in-flight
mechanic). Companion evidence to `M-agency-hardening.md`.

## Raw symptoms (as reported)

1. **Dropped codex completion bell** — codex-2 job
   `invoke-1781088035962-379-e1c88f5a` completed (went idle; committed `ac4ae5d`
   on `codex/m-pattern-posteriors-v0`) but its completion bell to claude-4 never
   fired. Caught only by polling roster-status + files-on-disk.
2. **Operator-turn replay** — Joe's emacs-repl "Cluster B park" message was
   re-delivered to claude-4 out of order (verbatim duplicate, several turns
   later). Joe: "the turns get interleaved in a confusing way." Idempotency gap.
3. **Bell/REPL interleaving** — async bell-turns (claude-3 coordination) and
   operator repl turns arrived threaded with no stable causal ordering between
   the two channels.
4. **Stale re-flag** — claude-3 re-flagged `:O-peradam-role` that claude-4 had
   completed a turn earlier; shared state not converging across the interleaving.
5. **Crossed bells** — claude-3's "land 379 as-is" crossed claude-4's "379
   landed+reviewed"; both compatible but needed a manual 3rd bell to reconcile.

Mitigation that worked in-session: poll roster + files-on-disk instead of
trusting the completion bell; treat apparent-duplicate operator messages
defensively (verify-already-done before acting).

## Verification (claude-6, against the live ledger, 2026-06-10)

**Symptom 1 — VERIFIED, but root cause CORRECTED.** Live job record:
`state=done, to=codex-2, caller="http-caller", delivery={status: delivered,
surface: bell, destination: "caller http-caller via /api/alpha/invoke/jobs/…",
note: bell-job-ready}`.

The bell did **not** drop. The job completed and the result was available via
job-status. The completion bell to claude-4 **could not fire because the caller
was never captured** — the dispatch used `http-caller` (no `--from`), so there
was no agent to route a completion bell to. ⇒ The report's suggested fix
(delivery-confirm/retry) would NOT have fixed this; the real lever is **caller
capture (`--from`) + auto-bellback**. With `caller=claude-4` recorded,
auto-bellback (`T-codex-auto-bellback`, built+reviewed, pending reload) fires
the completion bell automatically. Auto-bellback correctly excludes
`http-caller`, so the dispatch MUST pass `--from`.

## Mapping to tickets / invariants

| # | Root cause | Addressed by |
|---|---|---|
| 1 | caller not captured (`http-caller`) → no addressable recipient | `--from` capture (agency_send.py done; **adoption needed**) + auto-bellback (built, pending reload); **MQ-1** detects terminal-but-undelivered-to-caller |
| 2 | no message-id idempotency on operator turns | **NEW**: idempotent operator-turn dedup by message-id → fold into Car-3; new MQ "no-replay" invariant candidate |
| 3 | no causal ordering between bell + repl channels | **Car-3** (queue + per-surface routing) + per-bell causal/sequence marker |
| 4 | shared state not converging across interleave | per-bell causal/sequence marker (3/4/5 share this) |
| 5 | timing; no self-reconciliation of crossed bells | per-bell causal/sequence marker; **MQ-3** validates once bellback-intent is captured |

## Design implications (fold into the next tickets)

- **Car-3 gains concrete, field-validated requirements:** (a) a per-bell
  **causal/sequence marker** (Lamport-style) so crossed/stale bells
  self-reconcile (3/4/5); (b) **idempotent operator-turn dedup by message-id**
  (2); (c) per-surface reply routing + ordered drain (3).
- **Reload urgency:** symptom 1 is precisely what auto-bellback fixes (given
  `--from`). Strengthens the case to reload the `integration/agency-hardening`
  bundle.
- **`--from` adoption:** agents dispatching codex via `agency_send.py` MUST pass
  `--from`, else auto-bellback has no recipient. Consider server-side caller
  capture on the remaining HTTP/dispatch paths; update the handoff convention.
- **New MQ candidate (replay):** an operator/bell turn delivered more than once
  (same message-id) is a misrouting class → add to mesh-qa once message-id is
  captured.

## Validation log

- **2026-06-10 — auto-bellback fired LIVE (first time), fixing datapoint #1's
  class.** codex-1 reached terminal on job `invoke-…de998829` (Car-3 Phase-1)
  whose caller was a real agent (`claude-6`, supplied via `--from`). finalize
  auto-enqueued exactly one bellback job `auto-bellback-…de998829 → claude-6`
  (caller tagged `auto-bellback`); verified in the ledger that NO cascade
  occurred (single auto-bellback job, loop-safety tag held) and it routed to the
  correct caller. Confirms: the reload took, the `--from` capture is the enabler
  (an `http-caller` job would NOT bell back), and loop-safety is sound. This is a
  Codex→Claude completion loop captured as typed evidence (acceptance criterion).

## Checkpoint: Observed Behaviour Under Load (2026-06-10, claude-opus invoke-lane diagnosis)

**Reported symptom (Joe):** "fable-1 is invoking but claude-3 can't be reached."
Working hypothesis at report time: fable-1 is taking a *high-priority lane* and
starving the others.

**Corrected root cause: invoke-lane exhaustion, not prioritization.** There is
no priority lane. The invoke path is a **fixed thread pool of 4** with no
fairness/preemption (`futon3c/transport/http.clj:107-114`,
`(Executors/newFixedThreadPool 4 …)`). Every invoke holds its lane for the
**entire lifetime of the underlying CLI process**, and a cold-resume Claude/Codex
turn runs for *minutes*. When ≥4 invokes are in flight the pool saturates and all
further invokes queue FIFO — claude-3 "can't be reached" because its invoke is
sitting in that queue, not because it failed. It *was* reachable by bell because
a lane eventually freed; hence the intermittent presentation.

**Live evidence (read-only: thread dump + `ps` + one introspection eval):**

- Executor stats at peak (Drawbridge `/eval` on `http/invoke-executor`):
  `{:pool-size 4 :active 4 :max 4 :queued 6 :completed 168 :total-tasks 178}`
  → **all 4 lanes busy, 6 invokes backed up.**
- Thread dump (`jstack`, pid 1859440), the 4 `invoke-worker-N` threads:
  | Lane | Agent | State | Held for |
  |------|-------|-------|----------|
  | worker-1 | codex-1 (`resume 019d39df`) | live `codex exec` process, `ProcessImpl.waitFor` (`codex_cli.clj:382`) | 5.9 min |
  | worker-2 | **fable-1** (`resume 41e58a57`) | live `claude -p --resume` process, `ProcessImpl.waitFor` (`dev.clj:3577`) | 7.6 min |
  | worker-3 | (other) | **parked on `@waiter`** in `accept-and-drain!` (`turn_queue.clj:275-278`) | — |
  | worker-4 | (other) | **parked on `@waiter`** in `accept-and-drain!` (`turn_queue.clj:275-278`) | — |
- `ps`: `claude -p … --resume 41e58a57` etime `07:35`; `codex … resume 019d39df`
  etime `05:51`. Both look like genuine in-progress work, not wedged.

**Two compounding failure modes:**

1. **Cold-resume hogs a lane.** A multi-MB session resume runs for minutes (cf.
   [[project_kangaroo]]: fable-1 5.8MB ≈ 67s cold; here 7.6 min of actual work).
   2 of 4 lanes were gone to slow resumes alone.
2. **`accept-and-drain!` parks a pool thread on a promise** (`turn_queue.clj:278`,
   `@waiter`). Workers 3 & 4 weren't running anything — they were blocked waiting
   for *another* thread to drain their target agent's queue. Classic thread-pool
   anti-pattern: pool tasks blocking on other pool tasks. Even non-running turns
   consume lanes, so effective capacity is well under 4.

**Mapping to fixes (leverage order):**

| Lever | Fix | Status |
|-------|-----|--------|
| Strategic | **M-kangaroo** warm-pouch-per-agent — eliminates cold-resume so lanes free in seconds, not minutes. This contention is the concrete symptom motivating kangaroo. | in flight ([[project_kangaroo]]) |
| Structural | Decouple "drive the CLI process" from "wait for the result" in `accept-and-drain!` so a parked waiter does not burn an executor thread (removes failure mode #2). | candidate ticket |
| Immediate | Raise the pool from 4. Buys headroom but does not fix the waste — slow resumes still saturate, just later. NB it's a `defonce`; a live bump needs recreating the executor via `alter-var-root`, not a plain reload. | stopgap, not yet applied |

**Status:** diagnosis only; no code changed, nothing restarted (I-0 held — one JVM,
pid 1859440). Stopgap pool-bump and the `accept-and-drain!` decoupling left for
Joe's call vs. rolling the real fix into kangaroo.

## Finding F2: Queued turns park (and burn) invoke lanes — confirmed live, with a by-hand unfreeze

**For claude-6 to build into the shipped solution.** This is the mechanism
behind Checkpoint "Observed Behaviour Under Load", isolated and reproduced, plus
the manual procedure that unfreezes it.

**Mechanism (root cause, exact).** `accept-and-drain!`
(`futon3c/agency/turn_queue.clj:271-278`) does, per turn:
1. `accept!` → enqueue the entry into `(:queues agent-id)`, register a `waiter`
   promise in `!waiters`.
2. `drain!` → `acquire-drain!` (`turn_queue.clj:192-200`) succeeds **only if no
   one else is draining that agent-id**. If the agent is already draining, `drain!`
   returns immediately without processing.
3. `@waiter` (`turn_queue.clj:278`) → **blocks the calling thread** until some
   other thread (the active drainer) pops this entry and `mark-terminal!`s it.

That calling thread is an `invoke-executor` worker (fixed pool of 4,
`http.clj:107-114`). So a turn for an **already-draining** agent parks a pool
thread on a `CountDownLatch` doing nothing but waiting for the single per-agent
drainer to reach it. Per-agent draining is serial by design (FIFO, one drainer),
which is correct — but pairing it with "the waiter blocks a *pool* thread" means
**every queued turn for a busy agent costs one of the 4 lanes**. A slow
cold-resume agent (fable-1, minutes/turn) that receives 2-3 turns therefore
strands 2-3 lanes; new invokes for *other* agents (claude-3) can't get a thread
and pile up in the executor queue → "can't be reached".

**Live confirmation (2026-06-10).** Caught the JVM mid-freeze:
- `invoke-executor`: `{:active 4 :queued 12}` — all lanes busy, 12 invokes backed up.
- `jstack`: worker-2 in `ProcessImpl.waitFor` (fable-1's live `claude -p --resume
  41e58a57`); workers 1, 3, 4 parked on `CountDownLatch$Sync` = the `@waiter` at
  `turn_queue.clj:278`. **3 of 4 lanes burned on waiting, 1 doing real work.**
- `turn-queue/snapshot`: `:draining #{"fable-1"}`, `(:queues "fable-1") = [turn-2ae65d7f …]`
  — queued turns stacked behind the one slow drainer.

**By-hand unfreeze (what I ran; it worked).** Atomically flush every agent's
`:queues`, deliver each flushed turn's waiter as `:cancelled` so the parked pool
thread releases, clean the `!waiters`/`!processors` maps, persist. The active
drainer + its running CLI process (the WIP) are **never touched** — the WIP entry
has already been popped out of `:queues`, so it is not in the flush set.

```clojure
(do
  (require '[futon3c.agency.turn-queue :as tq])
  (let [qns         (the-ns 'futon3c.agency.turn-queue)
        !queue      (deref (ns-resolve qns '!queue))
        !waiters    (deref (ns-resolve qns '!waiters))
        !processors (deref (ns-resolve qns '!processors))
        persist!    (ns-resolve qns 'persist!)
        removed     (atom [])]
    (swap! !queue
      (fn [s]
        (reset! removed (mapcat (fn [[aid ids]] (map (fn [id] [aid id]) ids)) (:queues s)))
        (assoc s :queues (zipmap (keys (:queues s)) (repeat [])))))
    (doseq [[aid id] @removed]
      (when-let [p (get (deref !waiters) id)]
        (deliver p {:result "[cancelled: queued turn flushed to free invoke lane]"
                    :turn-queue/status :cancelled}))
      (swap! !waiters dissoc id)
      (swap! !processors dissoc id))
    (persist! (deref !queue))
    {:flushed (mapv (fn [[a id]] (str a "/" (subs id 0 13))) @removed) :n (count @removed)}))
```

Run via `scripts/proof-eval.sh` (Drawbridge `/eval`). Result on the live freeze:
flushed `["fable-1/turn-2ae65d7f" "fable-1/turn-f5e1dc48"]`; within seconds
`completed 168→172`, backlog `12→10`, the freed lanes picked up real invokes
(`:draining` went to `#{claude-6 fable-1}` — two agents working). It self-heals
from there as invokes complete.

**Caveat — it recurs.** Flushing reclaims the *currently* queued turns, but new
turns for the same slow agent re-park lanes within seconds (observed: workers 3/4
re-parked behind a fresh `claude-6` queued turn right after). It is a stopgap that
trades the queued turns away (their callers get a `[cancelled…]` result). Treat as
emergency unstick, not a fix.

**Build target for the shipped solution (priority order).**

1. **Don't park a pool thread on `@waiter` (the actual bug fix).** Decouple
   "enqueue + return a handle" from "block for the result". Options for claude-6
   to weigh: (a) the executor task that fails `acquire-drain!` returns the lane
   immediately and the *drainer* is responsible for completing the invoke job for
   every entry it drains (callback/job-completion instead of caller-blocking
   `@waiter`); (b) a dedicated single drainer thread *per agent* outside the shared
   invoke pool, so blocking on a slow agent never consumes a shared lane. Either
   removes the lane-burn entirely; the FIFO/one-drainer-per-agent invariant is kept.
2. **Per-agent in-flight cap / coalescing** so one slow agent cannot accumulate
   lane-stranding queued turns in the first place (dedup is already partly there
   via msg-id; extend to a depth bound).
3. **Ship the flush as a guarded admin op** (`scripts/flush-queued-turns.sh` +
   an authed endpoint) so the emergency unstick is one command, with the cancel
   surfaced as typed evidence rather than a hand-built eval. Lower priority than #1
   but useful until #1 lands.
4. **M-kangaroo** makes turns fast so drains clear in seconds — the contention
   that triggers all of the above largely disappears. Strategic, not a substitute
   for #1.

**Provenance:** isolated + reproduced + unfrozen by hand by the Claude owner
(opus) during a live freeze, 2026-06-10. Read-only diagnosis except the
documented flush; one JVM throughout (I-0).

## Checkpoint: Solve Agents Flickers (2026-06-10, claude-6 diagnosis; fix DEFERRED)

**Symptom (Joe):** the Emacs `*agents*` HUD flickers far more than a stable
roster warrants — "shows 1 registered, 4 registered, all kinds of nonsense,
flits all over."

**Not the registry.** Rapid-polled `/api/alpha/agents` 12× back-to-back:
rock-stable `n=4` (claude-3, codex-1, codex-2, fable-1) every time. The registry
and the endpoint are NOT flickering.

**Root cause = the HUD's feed channel, not the data.** The `*agents*` buffer is
a blackboard HUD (`emacs/futon3c-blackboard.el`; rendered by
`futon3c.blackboard/format-agent-status` + `project-agents!`, blackboard.clj
~775/875). It is driven by **multiple racing async-emacsclient pushers**:
- the 5s ticker `start-agents-blackboard-ticker!` (dev.clj:3309, full
  `registry-status`),
- a per-invoke local `project-agents!` (registry.clj:670) that builds a
  *different-shaped* status map (NO `:invoke-route`, so it renders a different
  invocable/unreachable line),
- per register/update/deregister pushes (registry.clj:921/982/1004),
all `:async? true` + coalesced/skippable → stale and current pushes land out of
order and overwrite each other. That is the "1 vs 4 / flits all over" signature.
(The literal "1" is most likely a late-landing early-boot push (count=1 =
codex-1 only); not fully pinned, but the *class* is racy multi-source async
writes through emacsclient — same family as [[blackboard-backpressure]].)

**The WS broadcast already exists but reaches nobody.** `broadcast-agents-ws!`
(registry.clj:185) already emits `{"type":"agents_status","agents":…,"count":…}`
on register/update/deregister/invoke. BUT `connected-agent-ids → []` right now —
there is **zero** connected WS client, so the broadcast is currently dead weight,
and the reattached agents are local-invoke (not WS bridges). Emacs's WS client is
`smart-cursor.el`, whose `--on-message` (smart-cursor.el:1062) currently lets
`agents_status` fall through to `(_ nil)` — ignored.

**Fix direction (Joe's call): drive the HUD from one ordered WS stream.** The
`agents_status` count comes from `registry-status` (stable 4), so a WS-fed HUD
won't flicker. Three parts:
1. **Emacs** — a *persistent* WS subscriber to `/agency/ws` that handles
   `agents_status` and renders `*agents*` in-process (single ordered writer).
   Open decision: **(a)** extend `smart-cursor.el` (reuse its WS conn) vs
   **(b)** a small dedicated HUD-subscriber `.el` independent of smart-cursor's
   cyborg-cursor lifecycle. claude-6 leans **(b)** (HUD shouldn't depend on
   smart-cursor being live); Joe to confirm.
2. **Server** — also call `broadcast-agents-ws!` on the 5s ticker (so polled
   external-state refresh reaches the HUD); enrich the frame summary to carry the
   fields the HUD shows (`:invoke-route`, `:session-id`, `:last-active`,
   elapsed). NB a WS recipient must actually be connected for any of this to land.
3. **Server** — disable the emacsclient `*agents*` blackboard push so WS is the
   SOLE writer → flicker gone + sheds emacsclient backpressure.

**Status:** diagnosis only, no code changed, nothing restarted (I-0 held).
Deferred behind the drainer fix. Evidence above is the spec for the build.
