# T-agency-desktop-save — durable agent roster (W5), restart → same agents, warm

Parent: `holes/missions/M-agency-hardening.md` **W5** (durable session registry /
kill-restart robustness, invariant 8). Joe's "Desktop Save Mode for Agency."
Dispatched by claude-6, 2026-06-10. Reviewer: claude-6. Branch off **master**.

## Goal

Persist the agent registry roster to disk so a JVM shutdown + restart brings the
SAME agents back registered — and (because restore rebuilds invoke-fns via the
now-kangaroo-gated factory) **warm**. Joe's flow becomes: wait for a pause →
shut down the JVM → restart → everyone's back, in a Kangaroo. No live `dev.clj`
reload, no hot-swap surgery.

## Existing ground (reuse, don't rebuild)

- `handle-agent-restore` (`/api/alpha/agents/restore`, http.clj ~1982) already
  rehydrates ONE agent from `{agent-id, type, session-id, session-file, cwd,
  emacs-socket, …}` and rebuilds its invoke-fn via `make-local-agent-invoke-fn`
  → `(resolve 'futon3c.dev/make-claude-invoke-fn)` (so restored agents are
  kangaroo-warm post-flag). **Replay through this path; don't reinvent it.**
- The invoke-jobs ledger (`!invoke-jobs-ledger`, persisted EDN with
  `recover-inflight-jobs` on load) is the durable-persist pattern to mirror.
- Today agents only return via the surviving Emacs daemon re-posting restore;
  codex/fable-1/non-Emacs agents are NOT covered. This closes that.

## Deliverables

1. **Roster persistence** (new `src/futon3c/agency/roster_store.clj` or in
   `registry.clj`): a durable EDN snapshot of the restorable roster, written on
   register / deregister / session-or-metadata change (mirror the invoke-jobs
   ledger: atom + `persist!` on swap, path via env, default `data/` or `/tmp`).
   Persist per agent ONLY the restore payload: `agent-id, type, session-id,
   session-file, cwd, emacs-socket, model, metadata, agency/contracts`. **Never
   persist the invoke-fn / live process** (rebuilt on restore). Continuous
   persist (not just a shutdown hook) so it survives `kill -9` (invariant 8).
2. **Restore-on-boot**: in startup (`futon3c.dev.bootstrap`/`start-futon3c!`),
   AFTER the registry + factory are up, read the roster and replay each agent
   through the restore path (call the restore logic directly or POST
   `/agents/restore`). **Idempotent** — coexists with Emacs re-attach
   (re-registering the same agent-id refreshes, never duplicates).
3. **No false online state (invariant 7)**: a restored agent starts
   `:restored`/`:expected-but-detached`, NOT falsely `:idle`/live, until it
   proves liveness (first successful invoke or Emacs re-attach). A restored agent
   whose session/process can't be reached surfaces as stale, not online.
4. **Flag-gated, load-dark**: gate restore-on-boot behind `FUTON3C_AGENT_RESTORE`
   (default OFF for v1). Persistence itself is always-on (harmless writes). So
   the build lands dark; claude-6 flips restore-on-boot after verifying a
   round-trip.

## Tests

- roster persist → load round-trips the restore fields.
- restore-on-boot replays a seeded roster → agents registered with rebuilt
  invoke-fns (stub the factory/restore so it's hermetic).
- idempotent: replaying an already-registered agent refreshes, no duplicate.
- restored agent is `:restored`/detached until proven, not falsely online.
- flag OFF → no restore-on-boot; persistence still writes.

## Gates / in-flight

clj-kondo clean; `futon4/dev/check-parens.el`; `clojure -X:test` green. No JVM
restart/reload (claude-6 owns it). Worktree off master: `git worktree add
../futon3c-desktop-save -b codex/agency-desktop-save master`. Commit there, NO
push/merge.

## Out of scope (v1)

Cross-host roster sync; persisting in-flight turns (the durable queue covers
that); reconciling the Emacs-side buffers (they re-attach themselves — just be
idempotent).

## Done = bell claude-6

with branch + sha, gate results, the roster schema + persist trigger, how
restore-on-boot replays (direct vs HTTP) + idempotency proof, the flag, and the
restored-state handling.
