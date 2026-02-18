# Futon3c — Real-time Coordination

Futon3c is the **real-time coordination** layer of the futon stack: agency
(multi-agent routing), peripherals (capability envelopes), forum
(collaborative proof trees), and the evidence landscape that connects
agents to each other and to persistent, queryable history.

## Quick Start

```bash
make dev       # Boot futon1a (XTDB) + futon3c transport
make claude    # Pick a session or start fresh
make test      # Run all tests (534 tests)
make repl      # Start nREPL with CIDER middleware
```

### `make dev`

Starts a single JVM with:

- **futon1a** on port 7071 (configurable via `FUTON1A_PORT`) — XTDB-backed
  durable storage with HTTP API for the evidence landscape
- **futon3c transport** on port 7070 (configurable via `FUTON3C_PORT`) —
  dispatch, presence, and health endpoints

No agents are registered at startup. They come alive when you launch a
Claude session or register them via REPL.

### `make claude`

Session picker that:

1. Checks if `make dev` infrastructure is running
2. Lists recent Claude Code sessions with first prompts and summaries
3. Lets you pick one to resume or start fresh
4. Launches `claude --resume <UUID> --permission-mode bypassPermissions`

Supports `--all` (sessions across all futon projects), `--new` (skip
picker), and search terms.

## Evidence Landscape

The evidence landscape is a typed, persistent store for everything agents
do: pattern selections, outcomes, reflections, gate traversals, forum
posts, and conjectures.

### Architecture

```
Claude Code session
  │
  ├─ /psr  ─→  POST /api/alpha/evidence  ─→  XTDB
  ├─ /pur  ─→  POST /api/alpha/evidence  ─→  XTDB
  ├─ /par  ─→  POST /api/alpha/evidence  ─→  XTDB
  └─ /rap  ←─  GET  /api/alpha/evidence   ←─  XTDB
                        │
                Arxana viewer (Emacs)
```

### Evidence Types

| Type | What | Slash Command |
|------|------|---------------|
| `pattern-selection` | PSR — pattern chosen to guide work | `/psr` |
| `pattern-outcome` | PUR — result of applying a pattern | `/pur` |
| `reflection` | PAR — post-action review of a session | `/par` |
| `coordination` | Agent dispatch, mode transitions | — |
| `gate-traversal` | Work passing through gate pipeline | — |
| `forum-post` | Collaborative proof tree contributions | — |
| `conjecture` | Hypotheses under investigation | — |

### HTTP API (futon1a)

**Write:**

```bash
curl -X POST http://localhost:7071/api/alpha/evidence \
  -H "Content-Type: application/json" \
  -d '{
    "type": "pattern-selection",
    "claim-type": "observation",
    "author": "claude",
    "session-id": "your-session-id",
    "pattern-id": "agent/pause-is-not-failure",
    "body": {"query": "stuck on tests", "confidence": "medium"},
    "tags": ["psr"]
  }'
```

Returns `201` with `{"ok": true, "evidence/id": "<UUID>", "entry": {...}}`.
Auto-generates ID and timestamp if not provided. Returns `409` on duplicate
IDs, `400` on missing required fields.

**Read:**

```bash
# All entries (newest first)
curl http://localhost:7071/api/alpha/evidence

# Filter by type, author, session, date
curl "http://localhost:7071/api/alpha/evidence?type=reflection&author=claude&limit=10"

# Single entry
curl http://localhost:7071/api/alpha/evidence/<id>

# Reply chain (root-first)
curl http://localhost:7071/api/alpha/evidence/<id>/chain
```

### Persistence Backends

The `EvidenceBackend` protocol (`futon3c.evidence.backend`) has two
implementations:

- **AtomBackend** — in-memory with CAS loop; used in tests and ephemeral
  sessions
- **XtdbBackend** — durable via XTDB; used when `make dev` is running

The backend is selected by what you pass as `:evidence-store`:

```clojure
;; In-memory (default)
(make-default-peripheral-config {})

;; Durable — pass an XTDB node
(make-persistent-peripheral-config {:xtdb-node my-node})

;; Or via runtime-config (used by make-http-handler, make-ws-handler)
(runtime-config {:patterns my-patterns :xtdb-node my-node})
```

### PSR/PUR Linking

PUR entries link back to their PSR via `:evidence/in-reply-to`. This
creates a chain: PSR (selected pattern) → PUR (outcome). The `/chain`
endpoint walks these links to reconstruct the full decision history.

### Viewing in Arxana

With `make dev` running, the Arxana browser in Emacs can browse evidence:

```
M-x arxana-browser → Lab → Evidence Timeline
M-x arxana-browser → Lab → Evidence by Session
```

Entries are rendered with type-aware faces (PSR = pink, PUR = green,
PAR = dark, etc.) and clickable hyperlinks for patterns, reply chains,
and sessions.

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `FUTON1A_PORT` | 7071 | futon1a HTTP port |
| `FUTON1A_DATA_DIR` | `~/code/storage/futon1a/default` | XTDB data directory |
| `FUTON3C_PORT` | 7070 | futon3c transport HTTP port (0 = disable) |
| `FUTON3C_PATTERNS` | (none) | Comma-separated pattern IDs |
| `CLAUDE_PERMISSION_MODE` | `bypassPermissions` | Permission mode for claude CLI |
| `CLAUDE_PICKER_MAX` | 12 | Max sessions shown in picker |

## Project Structure

```
src/futon3c/
  agency/         Multi-agent registry, session management
  evidence/       Evidence landscape (backend protocol, store API, XTDB backend)
  peripheral/     Capability envelopes (explore, edit, test, reflect, discipline)
  runtime/        Dev-facing API (register agents, wire persistence, start transport)
  social/         Shapes, dispatch, pipeline, presence, validation
  transport/      HTTP + WebSocket handlers, IRC bridge, wire protocol

dev/futon3c/
  dev.clj         Dev entry point (boots futon1a + futon3c)

scripts/
  claude-picker   Session picker for Claude Code
  *.clj           Demo and validation scripts

library/
  realtime/       13 coordination patterns
```

## The Three-Futon Architecture

| Repo | Concern | Timescale |
|------|---------|-----------|
| futon3a | Pattern search + querying | fast (query) |
| futon3b | Pattern-driven development | task + glacial |
| **futon3c** | **Real-time coordination** | **social (real-time)** |

futon3c depends on futon3b (gate pipeline) and futon1a (durable storage).

## Development

See `CLAUDE.md` for the futonic development methodology (PSR/PUR
discipline, derivation xenotype, evidence-first approach).
