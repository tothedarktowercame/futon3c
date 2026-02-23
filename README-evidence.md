# How Evidence Flows

Evidence entries are the persistent, typed record of everything agents do
in the futon stack. This document explains where evidence comes from, where
it goes, and how the laptop and server work together.

## The Short Version

```
                        LAPTOP                              SERVER (Linode)
                   ┌────────────────┐                  ┌────────────────────┐
                   │                │                   │                    │
 Codex REPL ──POST─┤  futon3c       │                   │  futon3c           │
 (codex-repl.el)   │  localhost:7070├───WS replication──▶│  :7070             │
                   │       │        │  {"type":"evidence"│       │            │
                   │       ▼        │   "entry":{...}}   │       ▼            │
                   │  local XTDB    │                   │  server XTDB       │◀── Claude /psr /pur /par
                   │  (futon1a)     │                   │  (futon1a)         │◀── IRC messages
                   └────────────────┘                   │       │            │◀── Peripheral sessions
                                                        │       ▼            │◀── POST /api/alpha/evidence
                                                        │  Arxana / Web UI   │
                                                        └────────────────────┘
```

**Laptop**: Evidence is written to local XTDB, then replicated over WS to the server.
**Server**: Evidence is written directly to server XTDB. That's where everything converges.

## Evidence Sources (Who Creates Entries)

### 1. Claude Code slash commands (`/psr`, `/pur`, `/par`)

When you run `/psr` in Claude Code, the skill invokes the **discipline
peripheral tools** (`psr-search`, `psr-select`, `pur-update`,
`par-punctuate`) in `real_backend.clj`. These tools:

- Create a proof-path record (PSR/PUR/PAR + gate events)
- Persist it to the futon3b proof-path file store
- Append a synthetic evidence entry to the evidence store

The evidence entry has type `:gate-traversal` and links to the proof-path.
This is how discipline records become queryable in the evidence landscape.

**Where it's stored**: The evidence store in the peripheral's backend config —
on the server, this is the XTDB backend.

### 2. Codex REPL (`codex-repl.el`)

The Emacs Codex REPL emits evidence by POSTing directly to
`/api/alpha/evidence` on the local Agency endpoint:

- **Session start**: `claim-type: goal` with tags `[codex session-start repl]`
- **User turns**: `claim-type: question` with tags `[codex chat emacs-codex-repl]`
- **Codex responses**: `claim-type: observation` with author `codex`
- **Errors**: `claim-type: correction`

Entries are chained via `in-reply-to` so the full conversation is a
navigable thread. Subject is `{ref/type: session, ref/id: <session-id>}`.

**Where it's stored**: POSTs to `localhost:7070/api/alpha/evidence` on the
laptop. This writes to the laptop's local XTDB via futon1a. Then WS
replication sends it to the server.

### 3. Peripheral sessions (explore, edit, test, reflect, etc.)

Every peripheral emits a three-entry evidence chain:

1. **Start** (`claim-type: goal`) — "peripheral :explore opened"
2. **Steps** (`claim-type: step`) — one per tool invocation, chained via `in-reply-to`
3. **Stop** (`claim-type: conclusion`) — includes fruit (what the peripheral produced)

The reflect peripheral uses `evidence/type: reflection`; all others use
`evidence/type: coordination`.

**Where it's stored**: Direct append via `common/maybe-append-evidence!` to
whatever evidence store is in the peripheral's context.

### 4. IRC messages

Both directions are recorded:

- **Incoming** (user PRIVMSG → IRC server): `type: forum-post`,
  `transport: irc`, author = IRC nick
- **Outgoing** (agent → IRC channel via WS relay): `type: forum-post`,
  `transport: ws-relay`, author = agent nick
- **Errors** (keepalive failures, relay timeouts): `type: coordination`,
  `claim-type: tension`

Subject is `{ref/type: thread, ref/id: "irc/<channel>"}`.

**Where it's stored**: Direct append in the IRC server/relay code.

### 5. Agent invocations (`/api/alpha/invoke`)

When an agent is invoked (from Emacs chat, IRC, or HTTP), the handler
emits two evidence entries:

- **Prompt**: caller's message, `type: forum-post`
- **Response**: agent's reply, `type: forum-post`

Subject is `{ref/type: thread, ref/id: "emacs/chat"}`.

**Where it's stored**: Direct append in `handle-invoke` (http.clj).

### 6. Direct HTTP POST

Any client can POST a raw evidence entry:

```bash
curl -X POST http://localhost:7070/api/alpha/evidence \
  -H "Content-Type: application/json" \
  -d '{"type":"coordination", "claim-type":"observation",
       "author":"joe", "body":{"text":"manual note"},
       "tags":["manual"]}'
```

This is how external tools (scripts, notebooks, etc.) feed into the
evidence landscape.

## Evidence Replication (Laptop → Server)

### The Problem

On the laptop, Codex runs locally. Its evidence (REPL turns, peripheral
steps, discipline records) goes into the laptop's local XTDB. But the
server is the system of record — that's where Arxana, the web viewer,
and Mission Control read from.

### The Solution

The Codex WS bridge (which already connects the laptop to the server for
invoke routing) also replicates evidence. Two components:

**Sender** (`transport/ws/replication.clj`, runs on laptop):
- Polls the local evidence store every 30 seconds
- Skips entries already tagged `:replicated` (loop protection)
- Skips entries already acked by the server
- Sends remaining entries as WS frames: `{"type":"evidence", "entry":{...}}`
- Tracks a high-water mark and pending set for reliability

**Receiver** (`transport/ws.clj`, runs on server):
- Receives `{"type":"evidence", ...}` frames
- Rejects entries already tagged `:replicated` (loop protection)
- Coerces JSON string values back to keywords (Cheshire boundary)
- Tags the entry with `:replicated` and records provenance:
  - `:evidence/replicated-by` — which agent sent it
  - `:evidence/replicated-at` — when it arrived
- Appends to the server's XTDB
- Sends back `evidence_ack` frame

### What You Need to Do

**Nothing.** If the Codex WS bridge is connecting to a remote target
(the normal laptop→Linode setup), replication is enabled by default.

Verify it's working:
```bash
# In make dev output, look for:
[dev] codex ws bridge evidence replication: enabled (interval 30000ms)

# Query the server for replicated entries:
curl 'http://linode:7070/api/alpha/evidence?tag=replicated'
```

### Env Vars

| Variable | Default | Description |
|----------|---------|-------------|
| `FUTON3C_CODEX_WS_REPLICATE_EVIDENCE` | `true` when targeting remote | Enable/disable replication |
| `EVIDENCE_REPLICATION_INTERVAL_MS` | 30000 | Poll interval in milliseconds |

## Where Evidence Lives

### On the Server (Linode)

One XTDB instance, one evidence store. Everything converges here:

- Claude's discipline records (PSR/PUR/PAR)
- IRC conversation history
- Peripheral session trails
- Agent invocation logs
- Replicated entries from the laptop

This is the store that Arxana, the web viewer, Mission Control, and the
`/api/alpha/evidence` REST API all read from.

### On the Laptop

A separate XTDB instance with its own evidence store. This holds:

- Codex REPL conversation turns
- Local peripheral sessions
- Local discipline records

These entries are replicated to the server by the WS sender. After
replication, they exist in both places (local copy stays, server copy
is tagged `:replicated`).

If you restart the laptop, the local XTDB still has the entries (it's
durable). They won't be re-replicated because the sender's high-water
mark is in memory — but the server already has them, and any re-sends
would be rejected as duplicates anyway.

## Reading Evidence

### HTTP API

```bash
# All entries (newest first)
curl http://localhost:7070/api/alpha/evidence

# Filter by type
curl 'http://localhost:7070/api/alpha/evidence?type=reflection'

# Filter by author
curl 'http://localhost:7070/api/alpha/evidence?author=claude-1'

# Filter by session
curl 'http://localhost:7070/api/alpha/evidence?session-id=sess-abc123'

# Single entry
curl http://localhost:7070/api/alpha/evidence/<id>

# Reply chain (root-first)
curl http://localhost:7070/api/alpha/evidence/<id>/chain

# Count
curl 'http://localhost:7070/api/alpha/evidence/count?type=forum-post'
```

### Arxana (Emacs)

```
M-x arxana-browser → Evidence → Evidence Timeline
M-x arxana-browser → Evidence → Evidence by Session
```

### Web Viewer

With `FUTON1A_STATIC_DIR` pointing to `futon4/dev/web`:
```
http://localhost:7071/evidence-viewer/
```

## Evidence Types

| Type | Short | What |
|------|-------|------|
| `pattern-selection` | PSR | Pattern chosen to guide work |
| `pattern-outcome` | PUR | Result of applying a pattern |
| `reflection` | PAR | Post-action review of a session |
| `coordination` | — | Agent dispatch, mode transitions, peripheral steps |
| `gate-traversal` | — | Work passing through the gate pipeline |
| `forum-post` | — | Chat messages (IRC, Emacs, invoke) |
| `conjecture` | — | Hypotheses under investigation |
| `presence-event` | — | Agent connect/disconnect |
| `mode-transition` | — | Peripheral hop events |
| `correction` | — | Error corrections |

## Evidence Entry Shape

```clojure
{:evidence/id          "e-<UUID>"
 :evidence/subject     {:ref/type :session :ref/id "sess-abc123"}
 :evidence/type        :coordination
 :evidence/claim-type  :observation
 :evidence/author      "claude-1"
 :evidence/at          "2026-02-23T14:30:00Z"
 :evidence/body        {:text "whatever the entry contains"}
 :evidence/tags        [:peripheral :explore]
 :evidence/session-id  "sess-abc123"
 :evidence/in-reply-to "e-<previous-UUID>"   ;; optional, forms reply chains
 :evidence/pattern-id  :agent/pause-is-not-failure  ;; optional, for PSR/PUR
 :evidence/fork-of     "e-<UUID>"            ;; optional, for branching
 :evidence/ephemeral?  false                 ;; optional, excluded from some queries
 :evidence/conjecture? false}                ;; optional, marks hypotheses
```
