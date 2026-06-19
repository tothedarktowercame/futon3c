# T-codex-auto-bellback — automatic completion bell to the job's caller

Parent mission: `holes/missions/M-agency-hardening.md` (W2/W3, invariant: typed
evidence + observable coordination). Dispatched to codex-1 by claude-6
(in-flight mechanic) 2026-06-10. Reviewer: claude-6.

## Goal

When a Codex invoke job finishes, automatically ring a bell back to the agent
that called it — so a forgotten "bell me back" step on a long build no longer
leaves the caller hanging. Message shape:

> 🔔 codex-N finished job `<job-id>` (state: `<terminal-state>`). `<short summary>`
> Details: /api/alpha/invoke/jobs/`<job-id>`

The caller is already recorded on every job (`:caller`, set from the bell
payload). This ticket consumes that field; it does not add new capture paths.

## Why

Joe: "codex is very inconsistent at belling back if it's left as an optional
step on a long build." Auto-bellback makes the completion signal a *system*
guarantee instead of a prose convention. The mesh-trace tool
(`scripts/mesh_trace.py`) will then show the returning edge.

## In (READ-ONLY — understand, do not restructure)

- `src/futon3c/transport/http.clj`
  - `finalize-invoke-job!` (line 514) — terminal-state writer; **the hook point**.
  - `run-invoke-job!` (line 2389) — calls finalize; shows terminal flow.
  - `handle-bell` (line 2544) — the existing enqueue path (`create-invoke-job!`
    + `.submit invoke-executor` + `run-invoke-job!`). **Reuse this mechanism**
    to enqueue the bellback; do NOT make an HTTP self-call.
  - `create-invoke-job!` (line 421) — job record shape (has `:caller`,
    `:agent-id`, `:surface`).
- `src/futon3c/agency/registry.clj` — `get-agent` / agent `:agent/type` lookup
  (to detect that the recipient is a Codex agent).
- `holes/missions/M-agency-hardening.md` — invariants, esp. **#6 no feedback
  loops** and **#3 typed evidence**.

## Out (the change)

1. `src/futon3c/transport/http.clj` — add auto-bellback, fired from the terminal
   path (inside or immediately after `finalize-invoke-job!`, while the job's
   `:caller`/`:agent-id`/terminal-state are in hand).
2. A new test namespace, e.g. `test/futon3c/transport/auto_bellback_test.clj`.

## Behaviour contract

Fire exactly one bellback for a job iff ALL hold:
- the job reached a **terminal** state (succeeded/failed/error/timeout/cancelled);
- the **recipient** (`:agent-id`) is a **Codex-type** agent (look up
  `:agent/type` in the registry; predicate must be trivially wideable to other
  types later — keep it a single `auto-bellback-recipient?` fn);
- the **caller** (`:caller`) is a real, currently-registered agent-id —
  explicitly EXCLUDE `"http-caller"`, `"joe"`, blank/nil, and `:caller` equal
  to `:agent-id` (no self-bell);
- the job is **not itself an auto-bellback** (see loop-safety).

Bellback payload: enqueue via the same mechanism as `handle-bell`, targeting
`:caller`, with a one-line prompt naming the finishing agent, job-id, terminal
state, and the short result-summary (or terminal-message). Tag the enqueued
bellback job so it is self-identifying, e.g. `:caller "auto-bellback"` (or a
`:kind :auto-bellback` field on the job) — this is what makes loop-safety
checkable.

**Idempotency:** stamp the original job with `:auto-bellback {:sent? true
:bell-job-id <id> :at <ts>}` inside the same ledger swap that marks it terminal
(or a guarded follow-up swap) so a re-`finalize` or recovery replay cannot
double-send.

**Loop safety (invariant #6):** an auto-bellback job must NEVER itself trigger
an auto-bellback. Enforce via the tag above (skip when the finishing job is an
auto-bellback). A bellback to a Claude caller won't recurse anyway (Claude isn't
a Codex recipient), but the tag must make this robust regardless of caller type.

**Config gate:** guard the whole feature behind a flag (env
`FUTON3C_AUTO_BELLBACK` or a config key), **default ON**, so it can be disabled
without a code change.

**Evidence:** the bellback is a normal invoke job, so it already lands in the
durable invoke-jobs ledger — no separate evidence wiring needed. Confirm the
returning edge is visible via `scripts/mesh_trace.py`.

## Tests (`clojure -X:test`, offline — must not need the live JVM)

Drive `finalize-invoke-job!` (and/or a small extracted pure predicate +
effect fn) with a seeded ledger; assert on enqueued-bellback calls (stub the
enqueue fn) rather than real subprocesses:
1. Codex recipient + real registered caller → exactly ONE bellback to caller;
   message contains job-id + terminal state.
2. Re-finalize the same job → NO second bellback (idempotent).
3. Claude recipient → no bellback (predicate gate).
4. caller ∈ {"http-caller","joe",nil,blank, =agent-id} → no bellback.
5. An auto-bellback job itself reaching terminal → no recursive bellback.
6. Feature flag OFF → no bellback.

Prefer extracting a pure decision fn (`(should-auto-bellback? job recipient-type
flag) -> bool`) + a thin effectful enqueuer so cases 1–6 are unit-testable
without the executor.

## Gates (all must pass before you bell back)

- `clj-kondo --lint` clean on every changed `.clj` (no new warnings).
- `futon4/dev/check-parens.el` passes on changed Clojure files.
- `clojure -X:test` green (at minimum the new ns; run the transport suite if
  feasible).

## In-flight constraints (HARD — a live collaboration is using this JVM)

- **Do NOT restart, kill, or `pkill` the serving JVM. Do NOT Drawbridge-reload.**
  The live reload is claude-6's job, at a window Joe picks. You only edit + run
  the offline test JVM (`clojure -X:test` is a separate short-lived process — fine).
- **Work in an isolated git worktree** of futon3c (e.g.
  `git worktree add ../futon3c-auto-bellback -b codex/auto-bellback`) so your
  edits don't collide with the live collaborators' working tree. Commit there.
  **Do NOT push and do NOT merge to master** — claude-6 reviews the branch/sha
  first.
- Git conflict markers parse as valid Clojure symbols and pass paren checks —
  if you ever rebase/merge, grep for `<<<<<<<`/`=======`/`>>>>>>>` explicitly.

## Done = bell claude-6 back

`python3 futon3c/scripts/agency_send.py --from codex-1 --to claude-6 --kind bell`
with: branch name + commit sha(s), the gate results (clj-kondo / check-parens /
test counts), the list of changed files, and any decisions you made (esp. the
recipient predicate and the loop-safety tag). claude-6 will review the diff,
re-run the gates, then do the live Drawbridge reload.
