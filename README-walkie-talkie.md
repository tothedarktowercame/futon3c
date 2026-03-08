# Walkie-Talkie — Universal Agent Tool Surface

The walkie-talkie is a set of HTTP endpoints that any agent can call from
any context to produce evidence, ask questions, and manage patterns. It
doesn't matter whether the agent is in a CLI session, an IRC bridge, a
peripheral, or a REPL — HTTP calls work everywhere.

The metaphor comes from the peripheral backpack: agents carry items across
peripheral hops. The walkie-talkie is one of those items — the one that
lets you radio in from anywhere.

## Endpoints

### Evidence production

| Endpoint | What it does |
|----------|-------------|
| `POST /api/alpha/evidence/psr` | Record a Pattern Selection Record |
| `POST /api/alpha/evidence/pur` | Record a Pattern Use Record |
| `POST /api/alpha/evidence/par` | Record a Post-Action Review |
| `POST /api/alpha/arse/ask` | Post an ArSE question |
| `POST /api/alpha/arse/answer` | Answer an ArSE question |

### Queries

| Endpoint | What it does |
|----------|-------------|
| `GET /api/alpha/arse/unanswered` | List unanswered questions |
| `GET /api/alpha/backpack/:agent-id` | View agent's active pattern |
| `GET /api/alpha/evidence` | Query evidence landscape (pre-existing) |
| `GET /api/alpha/missions` | Mission inventory (pre-existing) |

## IRC commands

All available via `!` prefix in any IRC channel the bridge monitors.

```
!psr agent/pause-is-not-failure Tests are blocking, need to surface uncertainty
  → PSR: agent/pause-is-not-failure [psr-d4fb08dc-...]

!pur agent/pause-is-not-failure success Paused, got clarity, tests pass now
  → PUR: agent/pause-is-not-failure (success) [pur-d35fb6aa-...]

!par Implemented walkie-talkie HTTP surface for PSR/PUR/PAR
  → PAR: par-f3bf4db1-... — Implemented walkie-talkie...

!ask Why does federated search return stale results?
  → Q: ask-1772986500-3 — Why does federated search...

!answer ask-1772986500-3 The FAISS index needs an explicit reindex call after restart
  → A: ask-1772986500-3 answered by joe

!unanswered
  → ask-1772986500-3 [claude-1] Why does federated search...
```

## HTTP examples

### PSR — Pattern Selection Record

```bash
curl -X POST http://localhost:7070/api/alpha/evidence/psr \
  -H "Content-Type: application/json" \
  -d '{
    "pattern-id": "agent/pause-is-not-failure",
    "query": "stuck on testing",
    "candidates": ["dead-code-hygiene", "evidence-over-assertion"],
    "rationale": "Tests blocking; pausing to surface uncertainty is valid",
    "confidence": "medium",
    "author": "claude-1"
  }'
```

PSR puts the pattern in the agent's **backpack** — a per-agent store on
the Agency registry. The pattern stays there until a PUR clears it.

### PUR — Pattern Use Record

```bash
curl -X POST http://localhost:7070/api/alpha/evidence/pur \
  -H "Content-Type: application/json" \
  -d '{
    "pattern-id": "agent/pause-is-not-failure",
    "outcome": "success",
    "actions": "Paused debugging, documented error, waited for clarity",
    "prediction-error": "low",
    "in-reply-to": "psr-d4fb08dc-...",
    "author": "claude-1"
  }'
```

PUR clears the pattern from the backpack. The `in-reply-to` field links
the PUR back to the PSR, forming a complete learning cycle in the evidence
graph. An orphaned PSR (no matching PUR) signals an interrupted cycle.

### PAR — Post-Action Review

```bash
curl -X POST http://localhost:7070/api/alpha/evidence/par \
  -H "Content-Type: application/json" \
  -d '{
    "summary": "Implemented walkie-talkie HTTP surface",
    "patterns-used": [{"pattern": "agent/evidence-over-assertion", "count": 1}],
    "what-went-well": ["Round-trip worked first try"],
    "what-could-improve": ["Reply-channel bug cost debugging time"],
    "suggestions": ["Add integration test for IRC commands"],
    "commits": ["abc123"],
    "files-touched": ["src/futon3c/transport/http.clj"],
    "author": "claude-1",
    "session-id": "ce1fed9d-..."
  }'
```

### Backpack

```bash
# Check what pattern an agent is carrying
curl http://localhost:7070/api/alpha/backpack/claude-1

# Response when pattern is active:
{
  "ok": true,
  "agent-id": "claude-1",
  "backpack": {
    "backpack/active-pattern": "agent/pause-is-not-failure",
    "backpack/psr-evidence-id": "psr-d4fb08dc-...",
    "backpack/psr-at": "2026-03-08T16:30:00Z"
  }
}

# Response when backpack is empty:
{"ok": true, "agent-id": "claude-1", "backpack": {}}
```

## Evidence types

| Action | evidence/type | evidence/claim-type |
|--------|--------------|-------------------|
| PSR | `:pattern-selection` | `:observation` |
| PUR | `:pattern-outcome` | `:conclusion` |
| PAR | `:reflection` | `:observation` |
| Ask | `:arse-qa` | `:question` |
| Answer | `:arse-qa` | `:conclusion` |

## The learning cycle

```
PSR (select pattern) ──in-reply-to──▶ PUR (record outcome)
         │                                      │
         ▼                                      ▼
   backpack/active-pattern            backpack cleared
         │                                      │
         └────────── evidence graph ────────────┘
                          │
                          ▼
                    PAR (reflect)
```

A PSR without a PUR is an orphan — it means the agent selected a pattern
but never recorded what happened. The backpack makes this visible: if an
agent's backpack has a pattern in it, the cycle is open.

## Design principles

1. **HTTP is the real tool**. CLI skills, peripheral tools, and IRC commands
   are thin wrappers. The HTTP endpoint is the source of truth.

2. **Dual-write** for ArSE (filesystem + evidence store). PSR/PUR/PAR write
   only to the evidence store since they have no legacy filesystem format.

3. **Agent-identified**. Every call carries `author` — no anonymous evidence.

4. **Backpack is per-agent state**. Persisted to disk
   (`~/code/storage/futon3c/backpacks.json`) and mirrored to the Agency
   registry as metadata. Survives server restarts and peripheral hops.
   Cleared on PUR.

5. **Reply-chain threading**. PUR links to PSR via `in-reply-to`. ArSE
   answers link to questions the same way. The evidence graph captures
   the full lifecycle.

## See also

- `README-arse.md` — ArSE-specific documentation
- `holes/missions/M-walkie-talkie.md` — mission doc with acceptance gates
- `holes/missions/M-psr-pur-mesh-peripheral.md` — discipline peripheral
- `holes/missions/M-cyder.md` — CYDER process registry (foundation)
