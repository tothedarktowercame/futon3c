# T-kangaroo-v1-pouch — warm claude pouch (cr version, flag-gated)

Parent: `holes/missions/M-kangaroo.md` (read its DERIVE log E1/E2 + ARGUE +
INSTANTIATE). Dispatched by claude-6, 2026-06-10. Reviewer: claude-6. Branch off
**master**.

## Goal

Stop cold-resuming `claude -p --resume` per turn. Serve each claude agent's turns
from ONE persistent warm `claude --input-format stream-json` process. Flag-gated,
load-dark; warmth applies to agents minted after reload (cr new). Both the REPL
buffer and Agency bells already route through `invoke-agent!` → the agent's
invoke-fn, so warming the invoke-fn warms both surfaces (the durable queue
already serializes per-agent, so one pouch is fed one turn at a time).

## PROVEN mechanism (M-kangaroo E1/E2 — do not re-derive)

A persistent `claude --print --resume <sid> --input-format stream-json
--output-format stream-json --verbose --permission-mode bypassPermissions`
process:
- accepts multiple turns on stdin, ONE JSON line per user message:
  `{"type":"user","message":{"role":"user","content":[{"type":"text","text":"…"}]}}`
- emits per-turn events `system → (rate_limit_event?) → assistant×N → result`;
  **turn boundary = the `{"type":"result"}` event**; `session_id` appears in the
  events.
- `--resume` carries context AND stays warm; pays the resume cost once at spawn.

## Deliverables

1. **`src/futon3c/agency/agent_pouch.clj`** (new): per-agent registry (defonce
   atom) of one warm process. API:
   - `enabled?` — `System/getProperty "FUTON3C_KANGAROO"` then `config/env-bool`,
     default **false** (mirror `turn-queue/enabled?`).
   - `(feed-turn! agent-id prompt {:keys [session-id model cwd timeout-ms]})` →
     spawn-if-absent the warm process (bound to session-id/cwd/model), write one
     user line, read events until `result`, return `{:result <assistant-text>
     :session-id <sid>}`. One process per agent-id (I-1); guard feed-turn per
     agent with a lock.
   - lifecycle: crash/EOF detection (process died → evict), idle-evict (TTL),
     max-warm cap; `snapshot` + `clear!` for tests/observability.
2. **`dev.clj` `make-claude-invoke-fn`** — gate behind `agent-pouch/enabled?`:
   ON → route the turn via `agent-pouch/feed-turn!`; on spawn-fail / dead process
   / feed timeout → **fall back to the existing cold body**. OFF → current cold
   body BYTE-FOR-BYTE (extract it to a local `invoke-once` like the Car-3 gate
   did; OFF must be provably unchanged).
3. **Tests** — hermetic, NO real claude CLI: stub the binary with a fake
   `claude` script (emits canned stream-json events for given stdin) via a
   configurable bin path. Cover: spawn + 2-turn warm on one process; feed-turn
   parses `result` + returns text + session_id; process crash → cold fallback;
   **flag-OFF path unchanged** (a `dev_test` that throws if the pouch is entered
   while disabled, like Car-3); idle-evict.

## Cold-fallback contract (hard)

Pouch spawn failure, dead process, or feed-turn timeout MUST fall through to the
cold `claude -p --resume` body. A turn is never stranded by warmth.

## Gates / in-flight constraints

- clj-kondo clean (no new warnings); `futon4/dev/check-parens.el`; `clojure
  -X:test` green incl. the flag-OFF transport/registry suite unchanged.
- No JVM restart/reload (claude-6 reloads dark, then a measured flip). Worktree:
  `git worktree add ../futon3c-kangaroo -b codex/kangaroo-v1 master`. Commit
  there, NO push/merge.
- Flag default OFF → reload is behaviour-neutral; do NOT try to warm
  already-registered agents (warmth is for agents minted after reload).

## Out of scope (v1)

Codex/cx pouch (mission E4); cross-JVM-restart warmth; REPL-side changes (none
needed — the invoke-fn chokepoint covers it).

## Done = bell claude-6

with branch + sha, gate results, the flag name + proof the OFF path is
unchanged, the pouch API + lifecycle decisions, and how the claude binary was
stubbed for tests.
