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
make tools     # Install repo-local clojure + bb into .tools/ (cluster-friendly)
make dev       # Boot futon1a (XTDB) + futon3c transport
make claude    # Pick a session or start fresh
make codex     # Pick a Codex session or start fresh
make codex-repl # Open Codex Emacs REPL launcher
make fresh     # Clear local agent continuity/session files
make test      # Run all tests
make repl      # Start nREPL with CIDER middleware
```

## ALFWorld

ALFWorld is integrated as an HTTP “simulation peripheral”: a Python server
wraps TextWorld/ALFWorld and Clojure/bb code drives it via `curl`.

In two terminals:

```bash
make dev
```

```bash
make alfworld-server
make alfworld-runner   # defaults: 10 games + verbose
```

Overrides:
```bash
make alfworld-server ALFWORLD_PORT=3457
make alfworld-runner ALFWORLD_RUNNER_GAMES=50 ALFWORLD_RUNNER_ARGS=
```

### `make dev`

Starts a single JVM with:

- **futon1a** on port 7071 (configurable via `FUTON1A_PORT`) — XTDB-backed
  durable storage with HTTP API for the evidence landscape
- **futon3c transport** on port 7070 (configurable via `FUTON3C_PORT`) —
  dispatch, presence, and health endpoints
- **Drawbridge** on port 6768 (configurable via `FUTON3C_DRAWBRIDGE_PORT`) —
  nREPL-over-HTTP for low-latency mission-control queries and admin eval

Role-aware defaults are available via `FUTON3C_ROLE`:
- `linode`: enables IRC (`6667`), registers `claude-1`, leaves `codex-1` for federation/proxy.
- `laptop`: disables local IRC (`0`), registers `codex-1`, skips `claude-1`.
- `default`: legacy behavior (IRC on, both agents registered).

Convenience launchers:
- `./scripts/dev-linode-env` — Linode profile + `make dev`
- `./scripts/dev-laptop-env` — laptop profile + `make dev`

Examples:
```bash
# Linode
FUTON3C_SELF_URL=http://172.236.28.208:7070 \
FUTON3C_LAPTOP_URL=http://<laptop-host>:47070 \
./scripts/dev-linode-env

# Laptop
FUTON3C_SELF_URL=http://<laptop-host>:47070 \
FUTON3C_LINODE_URL=http://172.236.28.208:7070 \
./scripts/dev-laptop-env
```

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
`make codex-repl` is a shortcut for `make codex ARGS="--repl"`.
Examples:
- `make codex ARGS="--repl --last"` — hop into Emacs REPL on your latest Codex session
- `make codex ARGS="--repl <SESSION_ID>"` — hop into Emacs REPL for a specific session

### Codex WS Bridge

`make dev` in laptop role starts an in-process Codex WebSocket bridge by default
(`FUTON3C_CODEX_WS_BRIDGE=true` for `FUTON3C_ROLE=laptop`). This keeps `codex-1`
reachable through `/api/alpha/invoke` and IRC dispatch without a separate bridge
process.

When laptop role has a configured Linode peer (`FUTON3C_LINODE_URL` or the first
entry in `FUTON3C_PEERS`), the Codex bridge now targets that peer over outbound
WS and auto-registers `codex-1` there as `ws-bridge=true`. This avoids requiring
inbound laptop networking for IRC → Codex delivery.

`make codex-repl` reuses the same session file (`/tmp/futon-codex-session-id`) and
prefers the live Agency session when available, so Emacs and IRC can pivot through
the same continuity lane.

When Codex needs to post from Emacs to IRC, it can emit an explicit directive
(`IRC_SEND <channel> :: <message>`), which `codex-repl` forwards to
`POST /api/alpha/irc/send` on the local Agency endpoint.

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

## Reflection Layer

The reflection layer exposes Clojure runtime metadata as structured,
queryable data. Every loaded namespace, every public var, its arglists,
doc string, file, and line number — all available through pure functions,
peripheral tools, and HTTP endpoints.

This serves two purposes:

1. **Self-representing stack**: Strategic claims about code ("component
   S-dispatch handles routing") can be grounded in what the runtime
   actually exports, not in prose. A claim that can't resolve to a var
   is a hypothesis, not evidence.

2. **Documentation checklist**: The reflection data is the authoritative
   inventory of what exists. Compare it against human-facing docs to
   find what's undocumented. Compare it against devmap components to
   find what's unimplemented. The gaps are discoverable mechanically.

### Architecture

```
Tier 1: Pure functions     futon3c.reflection.core
        ↓ called by
Tier 2: Peripheral tools   :reflect-namespaces, :reflect-ns, :reflect-var,
                           :reflect-deps, :reflect-java-class
        ↓ exposed via
Tier 3: HTTP endpoints     /api/alpha/reflect/*
```

### Core Functions (`futon3c.reflection.core`)

| Function | Returns | Use |
|----------|---------|-----|
| `list-namespaces` | `[{:ns sym :doc str :file str}]` | Inventory of all loaded code |
| `reflect-ns` | `[{:name sym :arglists :doc :file :line}]` | Public API of a namespace |
| `reflect-ns-full` | Same, including private vars | Full implementation inventory |
| `reflect-var` | `ReflectionEnvelope` | Complete metadata for one var |
| `reflect-deps` | `{:requires :imports :required-by}` | Dependency graph |
| `reflect-java-class` | `{:name :bases :flags :members}` | Java interop introspection |

All functions return `{:error ...}` on failure (namespace not found,
class not found) — no exceptions thrown.

### ReflectionEnvelope (`futon3c.reflection.envelope`)

The canonical shape for var metadata, validated by Malli:

```clojure
{:reflection/ns          'clojure.string       ;; namespace symbol
 :reflection/symbol      'join                  ;; var name
 :reflection/file        "clojure/string.clj"   ;; source file
 :reflection/line        180                     ;; source line
 :reflection/arglists    '([coll] [sep coll])   ;; function signatures
 :reflection/doc         "Returns a string..."   ;; docstring
 :reflection/resolved-at #inst "2026-02-23T..."  ;; when resolved (staleness)
 :reflection/private?    false
 :reflection/macro?      false
 :reflection/dynamic?    false}
```

The `:reflection/resolved-at` timestamp enables staleness detection: if a
strategic claim was grounded against a var that no longer exists (or has
changed), the discrepancy is detectable.

### HTTP API

```bash
# List all loaded namespaces
curl http://localhost:7070/api/alpha/reflect/namespaces

# Filter by pattern
curl "http://localhost:7070/api/alpha/reflect/namespaces?pattern=futon3c.social"

# Public vars in a namespace
curl http://localhost:7070/api/alpha/reflect/ns/clojure.string

# All vars (public + private)
curl http://localhost:7070/api/alpha/reflect/ns/clojure.string/full

# Full metadata for one var
curl http://localhost:7070/api/alpha/reflect/var/clojure.core/map

# Namespace dependency graph
curl http://localhost:7070/api/alpha/reflect/deps/futon3c.reflection.core

# Java class reflection
curl http://localhost:7070/api/alpha/reflect/java/java.util.HashMap
```

### Peripheral Tools

The reflection tools are available in the **:explore** and
**:mission-control** peripherals. Agents in these peripherals can call
`:reflect-namespaces`, `:reflect-ns`, `:reflect-var`, `:reflect-deps`,
and `:reflect-java-class` as regular tool invocations.

This means an agent exploring the codebase can ask structural questions
about the runtime without shelling out to grep — and get typed,
validated answers rather than text to parse.

### Java Legacy Codebase Use Case

The `reflect-java-class` function uses `clojure.reflect/reflect` to
introspect any Java class on the classpath. For legacy Java codebases,
this provides structural navigation without reading source: class
hierarchies, method signatures, field types, and access flags. Add the
Java JARs to the classpath, and the reflection layer can answer "what
methods does this class have?" with machine-readable precision.

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `FUTON1A_PORT` | 7071 | futon1a HTTP port |
| `FUTON1A_DATA_DIR` | `~/code/storage/futon1a/default` | XTDB data directory |
| `FUTON3C_ROLE` | `default` | Deployment role (`linode`, `laptop`, or `default`) used for role-based defaults |
| `FUTON3C_PORT` | 7070 | futon3c transport HTTP port (0 = disable) |
| `FUTON3C_IRC_PORT` | role-dependent | IRC server port (`linode`: 6667, `laptop`: 0, `default`: 6667) |
| `FUTON3C_BIND_HOST` | role-dependent | Bind host for IRC server (`linode/default`: `0.0.0.0`, `laptop`: `127.0.0.1`) |
| `FUTON3C_DRAWBRIDGE_PORT` | 6768 | Drawbridge HTTP port (0 = disable) |
| `FUTON3C_DRAWBRIDGE_BIND` | `127.0.0.1` | Drawbridge bind interface |
| `FUTON3C_DRAWBRIDGE_ALLOW` | `127.0.0.1,::1` | Comma-separated allowlist of remote addrs |
| `FUTON3C_ADMIN_TOKEN` | (falls back to `ADMIN_TOKEN`/`.admintoken`) | Drawbridge auth token |
| `FUTON3C_PATTERNS` | (none) | Comma-separated pattern IDs |
| `FUTON3C_REGISTER_CLAUDE` | role-dependent | Register local `claude-1` invoke-fn at startup |
| `FUTON3C_REGISTER_CODEX` | role-dependent | Register local `codex-1` invoke-fn at startup |
| `FUTON3C_PEERS` | (none) | Comma-separated peer Agency URLs for federation announcements |
| `FUTON3C_SELF_URL` | (none) | This host's reachable Agency base URL for federation callbacks |
| `CLAUDE_PERMISSION_MODE` | `bypassPermissions` | Permission mode for claude CLI |
| `CLAUDE_PICKER_MAX` | 12 | Max sessions shown in picker |

## Project Structure

```
src/futon3c/
  agency/         Multi-agent registry, session management
  evidence/       Evidence landscape (backend protocol, store API, XTDB backend)
  reflection/     Clojure runtime reflection (core functions, envelope schema)
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
