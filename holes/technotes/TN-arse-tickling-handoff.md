# TN: ArSE Tickling Handoff

**Date**: 2026-03-05 (updated 2026-03-06)
**Session**: c050ae94 (Linode claude-1)
**Status**: Pipeline proven (7/40 completed), orchestration design revised

## What ArSE Tickling Is

Tickle-regulated overnight synthetic QA generation for First Proof problems.
Codex (on laptop) generates hypergraph-native QA threads; Claude (on Linode)
reviews them. Evidence store tracks progress for resumable batch runs.

## What Got Built

### futon3c (committed: 8c886c3)

- **`src/futon3c/agents/arse_work_queue.clj`** — ArSE work queue module.
  Loads 40 entities from `futon6/data/arse-queue/entities.json`, provides
  `load-arse-entities`, `entity->issue`, `next-unprocessed`,
  `emit-arse-evidence!`, `queue-status`, etc.

- **`dev/futon3c/dev.clj`** — Added ArSE REPL helpers:
  - `(dev/arse-progress!)` — show N/40 completed, breakdown by problem
  - `(dev/run-arse-entry!)` — process single item (codex gen + optional claude review)
  - `(dev/run-arse-batch! :n N)` — overnight batch, resumable
  - Options: `:entity-id`, `:problem`, `:review?`, `:agent-id`, `:timeout-ms`

### futon6 (committed on laptop, pushed)

- `scripts/arse-store.py` (renamed from ase-store.py)
- `scripts/test-arse-generate.py` (renamed)
- `scripts/prepare-arse-tickle-queue.py` (renamed)
- `scripts/run-arse-hotspot-loop.py` (renamed)
- `scripts/retrieve-proof-context.py` (ASE→ArSE refs updated)
- `data/first-proof/codex-arse-pipeline-handoff.md` (renamed)
- `data/arse-queue/` (renamed from ase-queue/):
  - `entities.json` — 40 work items with embedded prompts
  - `review-prompts.json` — Claude review prompts keyed by entity
  - `queue-manifest.json` — queue metadata
- `data/synthetic-qa/`:
  - `problem2-prompts.jsonl` — 8 prompts (P2 gaps)
  - `problem3-prompts.jsonl` — 8 prompts (P3 gaps)
  - `problem7-prompts.jsonl` — 16 prompts (P7 gaps, pre-existing)
  - `problem8-prompts.jsonl` — 8 prompts (P8 gaps)

## Queue Distribution

| Problem | Items | Gap Types |
|---------|-------|-----------|
| P2 | 8 | test vectors, Rankin-Selberg integrals |
| P3 | 8 | local-global compatibility |
| P7 | 16 | curriculum gaps (pre-existing) |
| P8 | 8 | functorial lifts |
| **Total** | **40** | |

## End-to-End Test Results

### Completed items (2/40)

1. **synth-p2-problem-000**: Generated in 95s by Codex, reviewed by Claude.
   - Review verdict: REQUEST_CHANGES
   - Issues found: wrong thread_id (said p7 not p2), local integral formula
     conflated GL(1)×GL(n) with GL(n)×GL(n), "compactly supported" imprecise
   - Math content was correct in spirit — ramification obstruction argument valid

2. **synth-p2-problem-001**: Generated in 58s by Codex (no review).
   - Generation succeeded, evidence recorded.

### Evidence tags

ArSE evidence uses tags `[:tickle :arse-generation <event-tag>]` where
event-tag is one of: `:workflow-start`, `:generation-complete`, `:workflow-complete`.

Check progress: `(dev/arse-progress!)` via Drawbridge or REPL.

## How to Resume

### Prerequisites
1. Linode REPL running: `./scripts/dev-linode-env`
2. Codex WS bridge connected from laptop (codex-1 registered, WS connected)
3. Drawbridge on port 6768 (check: `curl -s -H 'x-admin-token: TOKEN' -d '(+ 1 1)' http://localhost:6768/eval`)

### Run a batch
```clojure
;; Via Drawbridge (fire-and-forget to avoid eval timeout):
(future
  (let [run! @(ns-resolve 'futon3c.dev 'run-arse-batch!)]
    (run! :n 10 :review? false)))

;; Or from REPL directly:
(dev/run-arse-batch! :n 38)              ; all remaining
(dev/run-arse-batch! :problem 7 :n 16)   ; just P7
(dev/run-arse-batch! :n 5 :review? false) ; quick test, no review
```

### Drawbridge gotcha

The `/eval` endpoint has a 5-minute timeout (`eval-timeout-ms` in
`src/repl/http.clj:60`). For long-running batch operations, wrap in a
`(future ...)` and write results to `/tmp/` or check evidence store.

### Codex issues encountered

Codex WS bridge is fragile in this session:
- First Drawbridge eval timed out (5min) but the JVM future completed fine
- Second dispatch via future succeeded (58s generation)
- Some futures silently fail to write results — error handling in the
  future wrapper may be swallowing exceptions
- The review verdict on the first item was REQUEST_CHANGES (quality issues
  in Codex output, not infrastructure issues)

## Known Issues to Fix

1. **Drawbridge eval timeout vs orchestrator timeout**: Both default to 5min,
   causing races. Either bump Drawbridge timeout or always use futures.

2. **Codex output quality**: First generated thread had wrong thread_id
   (synth-p7 instead of synth-p2) and mathematical imprecision. The prompt
   is good; Codex needs to follow it more carefully.

3. **Duplicate workflow-start evidence**: Entry 001 has two workflow-start
   entries (from two dispatch attempts). The `next-unprocessed` function
   correctly handles this (checks for workflow-complete, not workflow-start),
   but it's messy.

4. **IRC not running**: The batch progress IRC messages (`send-fn`) silently
   no-op when IRC isn't booted. Not a bug per se, but you won't see progress
   in #futon without it.

5. **Review prompt routing**: `run-arse-entry!` calls `orch/request-review!`
   which invokes claude-1. If claude-1 IS the current session (as it was
   here), the review gets routed to *this* chat buffer. Works but confusing.

## Architecture Notes

The pipeline flows: `run-arse-entry!` → `orch/assign-issue!` →
`reg/invoke-agent! "codex-1"` → WS bridge → laptop Codex → response back
via WS → optional `orch/request-review!` → `reg/invoke-agent! "claude-1"`
→ review verdict → evidence emitted.

Evidence store key: entity-id. Tags: `[:tickle :arse-generation <event>]`.
Issue numbers use 20000+ offset (CT uses 10000+).

## Revised Orchestration Design (2026-03-06 pilot learnings)

### What worked
- **Whistle API** (`POST /api/alpha/whistle`) delivers full prompts to codex
  reliably, returns responses in 30s. No IRC flooding.
- **`invoke-agent!`** via WS works when the bridge is healthy.
- **Entity-id fix**: `run-arse-entry!` now matches by `:entity-id-str` (string)
  not `:entity-id` (keyword). Without this, batch items were never found.
- **ngircd bridge** clean output for claude (no `[accepted]`/`[done]` framing),
  full framing kept for codex.

### What doesn't work
- **Long prompts via IRC**: `send-irc!` splits multi-line prompts into separate
  PRIVMSG lines. Codex only reads lines containing `@codex`, so it sees the
  first line and ignores the rest. Result: "Need the topic details..."
- **WS invoke reliability**: intermittent — sometimes 2s, sometimes 600s timeout.
  Same entity succeeded in 30s via whistle but timed out via `run-arse-entry!`.
- **Direct IRC connections**: agents must NOT open their own IRC connections as
  `claude` or `tickle-1` (nick conflicts with bridge bots). The bridge owns
  those nicks. Proactive messages use `claude-1` nick (agent-specific).

### Correct orchestration pattern
IRC is for **short coordination messages**, not prompt delivery:
- **Tickle** (Haiku, separate agent) posts brief IRC nudges to keep agents
  working: `@codex check issue #20042`, `@claude review is ready`
- **Long content** (prompts, results) goes to GitHub issues or repo files,
  referenced by number in IRC
- **Whistle API** for direct prompt delivery when IRC handoff isn't needed
- Agents MUST NOT flood IRC with multi-line prompts (I-1 violation when
  impersonating other agents, and IRC protocol violation regardless)

### Identity rules (I-1)
| Nick | Owner | Purpose |
|------|-------|---------|
| claude | ngircd bridge | Relays @claude mentions to claude-1 invoke |
| codex | ngircd bridge | Relays @codex mentions to codex-1 invoke |
| claude-1 | claude-1 (proactive) | Direct IRC posts from claude agent |
| tickle-1 | tickle system (Haiku) | Orchestrator — dispatches work, nudges agents |

### Next steps
1. Implement tickle-1 as Haiku agent that posts brief IRC handoffs
2. ArSE prompts go to GitHub issues; tickle posts `@codex see issue #NNNNN`
3. Codex reads the issue, generates QA, commits to repo
4. Tickle posts `@claude review synth-p2-s5-000 in futon6/data/synthetic-qa/`
5. Claude reviews, posts verdict to IRC and evidence store

## Infrastructure Fixes (2026-03-06 session continuation)

Several infrastructure issues were fixed while getting ArSE Tickling operational:

### Blackboard reconnection

The `*agents*` and `*invoke: claude-1*` side-windows stopped appearing because
the Emacs daemon uses a named socket (`workspace1`) and `emacsclient` was
called without `-s workspace1`.

**Fix**: `src/futon3c/blackboard.clj` now checks (in order):
1. `@!emacs-socket` atom (settable at runtime: `(reset! bb/!emacs-socket "workspace1")`)
2. `FUTON3C_EMACS_SOCKET` env var
3. `EMACS_SOCKET_NAME` env var

Both `scripts/dev-linode-env` and `scripts/dev-laptop-env` auto-detect the
daemon socket from `/run/user/$UID/emacs/` at startup.

`scripts/claude-picker` also updated to pass the socket to `emacsclient`.

### Multi-agent registration (`/api/alpha/agents/auto`)

New HTTP endpoint: `POST /api/alpha/agents/auto` with body `{"type": "claude"}`.
Allocates next available `claude-N` (I-1 compliant — each repl gets its own
agent identity). Creates a real `invoke-fn` via `make-claude-invoke-fn`.

`claude-repl.el` updated to:
1. Try `/agents/auto` first (server assigns ID)
2. Fall back to `GET /agents` and find existing `claude-*` agent
3. Default to `claude-1` if all else fails

**Note**: The `/agents/auto` route requires a server restart to take effect
(HTTP handler is compiled at startup). For the current session, `claude-2`
was registered manually via Drawbridge.

### Current agent registry

| Agent | Type | Status | Notes |
|-------|------|--------|-------|
| claude-1 | :claude | invoking | Primary session (c050ae94) |
| claude-2 | :claude | idle | Registered via Drawbridge for workspace2 |
| codex-1 | :codex | idle | WS bridge from laptop |
