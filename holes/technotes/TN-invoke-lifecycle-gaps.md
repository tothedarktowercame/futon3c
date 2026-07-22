# TN-invoke-lifecycle-gaps

**Technical note — process-level defects in the Agency invoke lifecycle,
traced 2026-07-22.** Author: claude-2. Intended for handoff to a fixing agent.
Trigger: "codex-2 did not resume properly / ran 30 minutes printing nothing"
(Joe). The diagnosis found the resume was fine — and six distinct lifecycle
defects instead. Sibling note (same afternoon, interacting failure):
`futon1b/TN-futon1b-memory-incident.md` — NB its §"Hardening pass deployed
2026-07-22 (Codex, 14:04–14:08 BST)" already landed store-failure taxonomy
(`:store-timeout`/`:store-unreachable` replacing the mislabelled `:shape`),
evidence cursor pagination + admission control, and an Emacs evidence
outbox. This note covers the invoke-LIFECYCLE gaps that remain after that
pass; a fixer should read both and not redo the store-side items.

## The incident, reconstructed (times BST)

One task ("mission scope ingest", claude-1's lane) was dispatched to codex-2
**three times**: a bell (job `…-10-a2d39bab`, 12:50, FAILED at the ~30-min
Agency cap 13:21), a retry bell (job `…-11-518b8373`, 13:22, CANCELLED
13:27), and finally Joe's manual repl turn ("Can you finish that task and
bell claude-1", 13:43, via `/api/alpha/invoke-stream`, curl `--max-time
1800`). The manual turn ran a real codex process tree for ~28 minutes
(verified live: codex binary with 3m16s CPU, two ESTABLISHED api TLS
connections, two spawned futon3c-classpath JVMs loading
`mission_scope_ingest.clj`) and its processes exited ~14:11 having delivered
**nothing visible anywhere**. The repl buffer showed nothing throughout; the
operator reasonably concluded codex-2 was wedged. Roster stayed `invoking`
with no process behind it.

In parallel, zai-1 received an APM Lean formalization from the formal-methods
cron (caller `http-caller`, job `…-12-a2d75b10`, 13:58): 27 tool events of
genuinely competent Mathlib work (transcript in the ring — reading the
bundle, locating `circleIntegral` lemmas, compiling a scaffold, iterating on
errors), then **`failed` with an EMPTY error event at 5m06s** — the default
5-minute turn timeout.

## Defects (each independently verifiable)

**D1 — Operator invoke-stream turns leave no job record.** Bell dispatches
get ring jobs (events, states, artifact refs). Joe's 28-minute codex turn has
NO entry: not in `/api/alpha/invoke/jobs`, no terminal state, no recorded
reply. Whether it completed, timed out, or crashed is unknowable post hoc —
the U1 lesson ("what did you actually run?" must be answerable from the
store) applied one layer up, unfixed for the operator path. This also means
the E2 "operator turns get the same guarantees as bells" claim does not
extend to auditability.

**D2 — Roster status wedges at `:invoking`.** When a turn dies without
finalization, nothing resets the agent: codex-2 sat `invoking` (last-active
14:02:37) with zero processes, and would have queued all future work behind a
phantom. There is no public unwedge — `mark-idle!` is a closure private to
the invoke wrapper. *Manual intervention performed 2026-07-22 ~14:25: registry
status swapped `:invoking`→`:idle` for codex-2 over Drawbridge (guarded — only
if still `:invoking`). The fixer should add a watchdog: `:invoking` with no
live turn thread/process for > N min → auto-idle + typed incident evidence.*

**D3 — Query params are silently ignored across the read APIs.** Observed
today: `/api/alpha/invoke/jobs?agent=codex-2` returns the GLOBAL list
(misattributed a zai-1 job to codex-2 during live diagnosis);
`/api/alpha/evidence?tags=` ignored (the working param is `tag=`); the :7073
server ignores `tag=` entirely. Every silent ignore turned into a wrong
conclusion mid-incident. Fix: unknown/unsupported params → 400, or implement
them; align param names across :7070 and :7073.

**D4 — Turn ceilings are misfit to real workloads and illegible in failure.**
The zai default timeout (5 min) killed a Lean run whose single compile step
legitimately takes minutes; the ~30-min Agency cap killed the original codex
job the same way earlier missions have been bitten (M-zaif-harness ops note:
"any handoff whose verify step runs >~20 min must background"). In both
cases the `failed` event carries NO reason text — a timeout is
indistinguishable from a crash. Fix: per-dispatch budget override
(`timeout-ms` in the bell/invoke payload), failure events must carry
`{:reason :timeout :budget-ms N}` (or the real exception), and long-workload
task classes (Lean, JVM-boot-heavy) need documented budgets.

**D5 — Codex turns are invisible from the operator surface.** zai streams
every round into its repl buffer via `sink!` (over-sharing, per this
morning's observation); codex streams NOTHING — 28 minutes of legitimate
work is indistinguishable from a wedge. This directly caused the
triple-dispatch: cap-kill → retry → manual nudge, three prompts for one
task. Fix: forward codex runtime events (`on-runtime-event` already exists in
`codex_cli.clj` — `:output`, tool events, bytes counters) to the agent's
sink/follow stream, or at minimum a periodic "still executing: N tool events,
M output bytes" heartbeat into the repl buffer.

**D6 — Retry/cancel semantics are unlinked.** Job 11 was a retry of job 10
(stated only in prose inside its prompt); the ring has no `:retry-of` edge,
and job 11's `cancelled` at +5min has no recorded reason or actor. Duplicate
work and lost context are invisible to any later reader. Fix: typed
`:retry-of`/`:cancelled-by` fields on jobs.

## What was ruled OUT (do not chase these)

- codex resume itself: worked (the process resumed the session, connected,
  and executed 27+ tool actions across the afternoon's attempts).
- Session/lock contention with the long-lived interactive codex (PID 24683):
  different session id (`019f8928…` vs codex-2's `019f8934…`).
- The futon1b brown-out as the proximate cause of THIS incident (it likely
  caused job 10's slowness, but jobs 11/12 and the manual turn ran after the
  13:37 store restart).

## Acceptance bar for closing this note

(a) An operator invoke-stream turn appears in the job ledger with terminal
state and reply reference; (b) an `:invoking` agent with no live turn
auto-heals within minutes, emitting a typed incident event; (c) unknown query
params on jobs/evidence endpoints are rejected, and `tag=`/`agent=` behave
identically on :7070; (d) a timed-out turn's failure event names the timeout
and budget; (e) a codex turn shows liveness in its repl buffer at least every
60s; (f) findings recorded back here.
