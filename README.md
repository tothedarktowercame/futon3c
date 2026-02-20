# Futon3c — Real-time Coordination

Futon3c is the **real-time coordination** layer of the futon stack: agency
(multi-agent routing), peripherals (capability envelopes), forum
(collaborative proof trees), and the evidence landscape that connects
agents to each other and to persistent, queryable history.

For an agent-facing current-state map, see
`docs/system-now-next.md` ("What exists / What's next").
For the canonical system-building topology, see
`docs/wiring-contract.md` ("diagram -> implementation projection").
For commit-scoped architectural proof, see
`docs/wiring-claims.edn` + `docs/wiring-evidence.edn`.
For Mission Peripheral / Mission Control / War Room convergence, see
`docs/mission-contract.md` + `docs/mission-claims.edn` + `docs/mission-evidence.edn`.

## Quick Start

```bash
make dev       # Boot futon1a (XTDB) + futon3c transport
make claude    # Pick a session or start fresh
make codex     # Pick a Codex session or start fresh
make test      # Run all tests
make repl      # Start nREPL with CIDER middleware
```

### `make dev`

Starts a single JVM with:

- **futon1a** on port 7071 (configurable via `FUTON1A_PORT`) — XTDB-backed
  durable storage with HTTP API for the evidence landscape
- **futon3c transport** on port 7070 (configurable via `FUTON3C_PORT`) —
  dispatch, presence, and health endpoints
- **Drawbridge** on port 6768 (configurable via `FUTON3C_DRAWBRIDGE_PORT`) —
  nREPL-over-HTTP for low-latency mission-control queries and admin eval

No agents are registered at startup. They come alive when you launch a
Claude or Codex session, or register them via REPL.

Mission control is initialized at startup and stays hot in the same JVM.
From Drawbridge eval:

```clojure
(require '[futon3c.mission-control.service :as mcs])
(mcs/list-sessions)
(mcs/run-review! {:author "joe"})
```

### `make claude`

Session picker that:

1. Checks if `make dev` infrastructure is running
2. Lists recent Claude Code sessions with first prompts and summaries
3. Lets you pick one to resume or start fresh
4. Launches `claude --resume <UUID> --permission-mode bypassPermissions`

Supports `--all` (sessions across all futon projects), `--new` (skip
picker), and search terms.
Use flags via make as: `make claude ARGS="--all"` (or call the script directly).

### `make codex`

Session picker for Codex that:

1. Checks if `make dev` infrastructure is running
2. Opens Codex's built-in resume picker (`codex resume`)
3. Lets you pick a prior session/thread or start fresh

Supports `--all` (show all sessions), `--new` (skip resume, start fresh),
`--last` (resume most recent), explicit session/thread ID, and `--repl`
(open `emacs/codex-repl.el`).
Use flags via make as: `make codex ARGS="--all"` (or call the script directly).
Examples:
- `make codex ARGS="--repl --last"` — hop into Emacs REPL on your latest Codex session
- `make codex ARGS="--repl <SESSION_ID>"` — hop into Emacs REPL for a specific session

When using `--repl`, `codex-repl.el` logs evidence turn-by-turn:
- session bootstrap (`claim-type: goal`, event `session-start`)
- user and assistant chat turns (`claim-type: question` / `observation`)
- chained via `in-reply-to`, so Arxana's Evidence Threads view shows a real thread.

## Evidence Landscape

The evidence landscape is a typed, persistent store for everything agents
do: pattern selections, outcomes, reflections, gate traversals, forum
posts, and conjectures.

Codex REPL turns are recorded as `type: coordination` entries on
`subject: session/<sid>` with `body.event = "chat-turn"`.

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

### HTTP API (futon3c transport)

**Write:**

```bash
curl -X POST http://localhost:7070/api/alpha/evidence \
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
curl http://localhost:7070/api/alpha/evidence

# Filter by type, author, session, date
curl "http://localhost:7070/api/alpha/evidence?type=reflection&author=claude&limit=10"

# Single entry
curl http://localhost:7070/api/alpha/evidence/<id>

# Reply chain (root-first)
curl http://localhost:7070/api/alpha/evidence/<id>/chain
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
M-x arxana-browser → Evidence → Evidence Timeline
M-x arxana-browser → Evidence → Evidence by Session
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
| `FUTON3C_DRAWBRIDGE_PORT` | 6768 | Drawbridge HTTP port (0 = disable) |
| `FUTON3C_DRAWBRIDGE_BIND` | `127.0.0.1` | Drawbridge bind interface |
| `FUTON3C_DRAWBRIDGE_ALLOW` | `127.0.0.1,::1` | Comma-separated allowlist of remote addrs |
| `FUTON3C_ADMIN_TOKEN` | (falls back to `ADMIN_TOKEN`/`.admintoken`) | Drawbridge auth token |
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
  codex-picker    Session picker for Codex CLI
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
