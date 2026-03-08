# Mission: Walkie-Talkie — Universal Agent Tool Surface

## Status: DONE (2026-03-08) — Gates A+D pass, B+C deferred

## Derivation

IDENTIFY step. Builds on:
- M-cyder (process registry, HTTP as universal observation surface)
- M-peripheral-phenomenology (backpack metaphor: walkie-talkie, ID card, pattern card)
- M-psr-pur-mesh-peripheral (discipline peripheral with PSR/PUR/PAR tools)
- M-peripheral-model + M-peripheral-behavior (peripheral inhabitation invariants)
- futon3/agency/http.clj (original "agent walkie-talkie" WS endpoint)
- fuclaude-peripheral.ts / fucodex-peripheral.ts (backpack & walkie-talkie wrappers)
- ArSE dual-write infrastructure (arse-store.py ask/answer subcommands)
- Evidence landscape shapes (futon3c/social/shapes.clj — EvidenceEntry, ArtifactRef)

## The Problem

Agents inside peripherals can use peripheral tools (the constrained capability
envelope). But several important actions are *not* peripheral-specific — they
are things any agent should be able to do regardless of which peripheral it
inhabits:

1. **Emit evidence** — PSR, PUR, PAR records into the evidence landscape
2. **Ask and answer questions** — ArSE Q&A into the evidence landscape
3. **Bell other agents** — send a message to another agent via Agency routing
4. **Observe stack state** — query mission status, pattern library, evidence

Today these actions are split across incompatible surfaces:

| Action | Works in Claude Code CLI | Works in peripheral | Works in IRC bridge |
|--------|------------------------|--------------------|--------------------|
| /psr   | Yes (skill)            | No                 | No                 |
| /pur   | Yes (skill)            | No                 | No                 |
| /par   | Yes (skill)            | No                 | No                 |
| /ask   | Yes (skill)            | No                 | No                 |
| /answer| Yes (skill)            | No                 | No                 |
| bell   | Ad-hoc (curl)          | Ad-hoc (curl)      | @mention           |

The backpack metaphor from README-peripherals.md already names this: the
walkie-talkie is one of the items agents carry across peripheral hops. But
currently it's a metaphor without implementation — agents don't actually have
a uniform way to "radio in" from any peripheral.

## The Insight: HTTP Is the Walkie-Talkie

From M-cyder: HTTP endpoints are the universal observation/intervention surface.
Every running process is reachable via HTTP. The same principle applies to
agent actions: if evidence-producing actions are HTTP endpoints, then any agent
can call them regardless of context — CLI session, peripheral, IRC bridge,
REPL, or autonomous loop.

The walkie-talkie is not a WebSocket connection (that's the transport layer).
The walkie-talkie is a set of HTTP endpoints that agents can call to produce
evidence, ask questions, and signal other agents. It's the difference between
the radio hardware (transport) and the radio protocol (what you can say).

From the teaching-inversion pattern: the walkie-talkie should carry insight,
not just status. PSR/PUR/PAR/ask/answer are all insight-carrying actions.

## Scope

### In scope

**Evidence-producing endpoints** (the core walkie-talkie functions):

| Endpoint | Action | Evidence type |
|----------|--------|---------------|
| `POST /api/alpha/evidence/psr` | Pattern Selection Record | `:pattern-selection` |
| `POST /api/alpha/evidence/pur` | Pattern Use Record | `:pattern-outcome` |
| `POST /api/alpha/evidence/par` | Post-Action Review | `:reflection` |
| `POST /api/alpha/arse/ask` | Post ArSE question | `:arse-qa` (claim: question) |
| `POST /api/alpha/arse/answer` | Answer ArSE question | `:arse-qa` (claim: conclusion) |

**Query endpoints** (read-only walkie-talkie functions):

| Endpoint | Action |
|----------|--------|
| `GET /api/alpha/evidence/query` | Query evidence landscape |
| `GET /api/alpha/arse/unanswered` | List unanswered ArSE questions |
| `GET /api/alpha/patterns/search` | Search pattern library (already exists in futon3a) |
| `GET /api/alpha/missions` | Mission inventory (already exists in mission-control) |

**Thin wrappers** that call the HTTP endpoints:

- CLI skills (`/psr`, `/pur`, `/par`, `/ask`, `/answer`) — already exist, rewire to HTTP
- Peripheral tools (`:discipline` peripheral) — already exist, rewire to HTTP
- IRC bridge commands — new, thin adapter over HTTP

### Out of scope

- Changing the WebSocket transport layer (that's transport, not walkie-talkie)
- New peripheral types (use existing `:discipline` peripheral)
- Changing how agents are routed or registered (that's Agency, not walkie-talkie)
- Pattern library write API (patterns are curated, not agent-written)

## Constraints

1. **I-2 (Transport Routes, It Does Not Create)**: The HTTP endpoints produce
   evidence entries. They do not spawn agents, create processes, or make LLM
   calls. They are pure data-writing functions.

2. **I-3 (Peripherals Are Inhabited, Not Delegated)**: Calling a walkie-talkie
   endpoint from inside a peripheral does not exit the peripheral or delegate
   to another agent. The agent stays in its peripheral and radios in.

3. **Dual-write**: All evidence-producing endpoints dual-write to both the
   filesystem store (for existing tools) and futon1a (for the evidence graph).
   Same pattern as ArSE and pattern library.

4. **Shape-validated**: All payloads validated against Malli shapes from
   `futon3c/social/shapes.clj` (EvidenceEntry, ArtifactRef, etc.).

5. **Agent-identified**: Every walkie-talkie call carries the agent's identity.
   No anonymous evidence. The agent authenticates via the same mechanism it
   uses for its transport connection.

## Acceptance Gates

### Gate A: Evidence Round-Trip

Pass when:
1. Agent calls `POST /api/alpha/evidence/psr` with a pattern selection.
2. Evidence entry appears in futon1a with correct shape.
3. Same evidence is queryable via `GET /api/alpha/evidence/query`.

### Gate B: Cross-Surface Parity

Pass when:
1. CLI skill `/psr` produces the same evidence entry as a direct HTTP call.
2. Peripheral tool `psr/select` in `:discipline` produces the same evidence.
3. All three paths result in identical evidence entries in futon1a.

### Gate C: Peripheral Interior Call

Pass when:
1. Agent is inside a non-discipline peripheral (e.g., `:explore` or `:edit`).
2. Agent calls a walkie-talkie endpoint (e.g., PSR or /ask) via HTTP.
3. Evidence is produced without exiting the peripheral.
4. Peripheral session state is unaffected.

### Gate D: ArSE Round-Trip

Pass when:
1. Agent calls `POST /api/alpha/arse/ask` with a question.
2. Different agent (or same agent later) calls `POST /api/alpha/arse/answer`.
3. Both entries appear in futon1a as linked evidence (reply-chain threading).
4. `GET /api/alpha/arse/unanswered` correctly excludes the answered question.

### Gate Status (2026-03-08)

- **Gate A (Evidence Round-Trip): PASS** — PSR, PUR, PAR endpoints live-tested.
  Evidence entries appear in store with correct shapes and are queryable.
- **Gate B (Cross-Surface Parity): DEFERRED** — CLI skills and discipline
  peripheral still write to futon1a directly rather than through the walkie-talkie
  HTTP endpoints. Same evidence shape, different code path. Rewiring is clean-up.
- **Gate C (Peripheral Interior Call): DEFERRED** — HTTP endpoints are callable
  from any context (they're just curl), but no captured evidence of an agent
  inside `:explore` calling them. Verification exercise, not new implementation.
- **Gate D (ArSE Round-Trip): PASS** — ask→answer→unanswered round-trip with
  linked evidence (in-reply-to threading). Dual-write to filesystem + evidence store.

### Delivered beyond original gates

- Persistent backpacks (`~/code/storage/futon3c/backpacks.json`) — PSR puts
  pattern in backpack, PUR clears it, survives server restarts
- `GET /api/alpha/backpack/:agent-id` — query any agent's active pattern
- `GET /api/alpha/patterns/search?q=...` — keyword search across 853 patterns
- IRC commands: `!psr`, `!pur`, `!par`, `!ask`, `!answer`, `!unanswered`, `!patterns`
- CLI skill: `/patterns <query>`
- README-walkie-talkie.md and README-arse.md
- Reply-channel fix for multi-channel IRC command responses

## Implementation Sketch

### Phase 1: Evidence write endpoints

Add HTTP routes to futon3c's transport layer:

```clojure
;; In transport or a new walkie-talkie namespace
(defn handle-evidence-write [req]
  ;; 1. Parse + validate against EvidenceEntry shape
  ;; 2. Dual-write: filesystem + futon1a
  ;; 3. Return receipt
  )
```

The existing `arse-store.py` logic for dual-write moves into Clojure (or the
Python script becomes the backing implementation called via shell — pragmatic
choice based on what's faster to ship).

### Phase 2: Rewire existing surfaces

- CLI skills (`/psr`, `/pur`, `/par`, `/ask`, `/answer`): change from direct
  filesystem/script calls to `curl` against the HTTP endpoint
- `:discipline` peripheral tools: change from direct evidence writes to HTTP
  endpoint calls
- IRC bridge: add simple command parsing (`!psr`, `!ask`) that calls HTTP

### Phase 3: Query endpoints

- Evidence query endpoint wrapping futon1a's existing query API
- ArSE unanswered endpoint wrapping arse-store.py query

## Relationship to Other Missions

- **M-cyder**: Walkie-talkie endpoints are registered processes in the CYDER
  sense — they're HTTP handlers, always available when the server is up.
  CYDER's "jack in" is for REPL-like processes; walkie-talkie endpoints are
  infrastructure (always-on, not steppable).

- **M-psr-pur-mesh-peripheral**: The `:discipline` peripheral already has
  PSR/PUR tools. This mission makes those tools available *outside* the
  discipline peripheral by exposing them as HTTP endpoints. The discipline
  peripheral becomes one of several thin wrappers.

- **M-peripheral-phenomenology**: The backpack metaphor names five items:
  walkie-talkie, ID card, pattern card, PAR notebook, forum notebook. This
  mission implements the walkie-talkie item concretely. The others (session
  persistence, pattern carrying, PAR emission, evidence writing) are all
  *functions of the walkie-talkie* — things you do via the radio.

- **M-social-exotype**: The evidence types and shapes that walkie-talkie
  endpoints validate against come from the social exotype. The walkie-talkie
  is a write surface for the social topology.

## Open Questions

1. **Auth model**: How does an agent authenticate a walkie-talkie HTTP call?
   Options: bearer token from transport session, shared secret (MonsterMountain),
   or trust-local (localhost only, no auth). Trust-local is simplest and
   matches current futon3c posture.

2. **Python vs Clojure for ArSE**: arse-store.py already works. Do we port
   to Clojure or shell out? Pragmatic answer: shell out for now, port later
   if the indirection becomes painful.

3. **Endpoint prefix**: `/api/alpha/` matches mission-control. Is this the
   right namespace? Could also be `/walkie-talkie/` for clarity but that's
   cute rather than conventional.

4. **Bell/signal other agents**: This is the original walkie-talkie meaning
   (agent-to-agent messaging). The WS `/agency/ws` endpoint already does
   this. Should the HTTP walkie-talkie surface include a `POST /api/alpha/bell`
   that sends a message to a named agent? Or is @mention via IRC sufficient?
