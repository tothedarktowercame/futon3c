# Technote: The Codex Harness and the Futon Peripheral System

Date: 2026-02-19

## Context

On 2026-02-05, OpenAI published "Unlocking the Codex harness: how we built
the App Server" describing the architecture that powers all Codex surfaces
(web app, CLI, IDE extension, macOS app). The same week, we wired the last
integration seams in futon3c's peripheral dispatch system. The two systems
solve overlapping problems from different directions, and the comparison
is instructive.

## The Codex Harness Architecture

The Codex harness is the agent loop and supporting logic that underlies all
Codex experiences. Its key components:

**Core agent loop.** An outer loop (conversation turns) wraps an inner loop
(reasoning iterations). Each turn begins with prompt assembly (instructions +
tools + input) and ends when the LLM returns a "done" event. Within a turn,
the agent iteratively calls the LLM, which produces tool invocations or
reasoning outputs, appended to the prompt for the next iteration.

**App Server.** A long-lived process exposing Codex core via bidirectional
JSON-RPC over stdio (JSONL). Four components: stdio reader, message
processor, thread manager, and core threads. The thread manager spins up
one core session per thread.

**Three conversation primitives:**
- *Item*: atomic I/O unit (user message, agent message, tool execution,
  approval request, diff). Lifecycle: started → delta* → completed.
- *Turn*: one unit of agent work initiated by user input. Contains a
  sequence of items.
- *Thread*: durable session container. Supports create/resume/fork/archive
  with persisted event history.

**Tool execution.** Shell/file tools run in an air-gapped sandbox. MCP
servers and skills participate under a consistent policy model. The server
can pause a turn to request approval before executing a command.

**Client integration.** Local clients bundle a platform-specific binary and
communicate over stdio. Web clients use HTTP+SSE to a container running the
App Server. The protocol is backward-compatible so older clients can talk
to newer servers.

## The Futon Peripheral System

The futon3c peripheral system is the coordination layer for multi-agent
sessions. Its key components:

**Social pipeline.** Five stages (S-presence → S-authenticate → S-mode →
S-dispatch → S-persist) classify and route messages. Malli shape validation
is enforced across the core pipeline boundaries, with additional transport
edge-case hardening still in progress.

**Peripherals.** Constrained capability envelopes implementing the
PeripheralRunner protocol (start → step* → stop). 11 peripheral types,
each with a defined tool set, scope, entry/exit conditions. Tool dispatch
enforces constraints structurally — an explore peripheral physically cannot
call `:edit` because the dispatch layer rejects it before the backend sees it.

**RealBackend.** A single implementation of ToolBackend (574 lines) that
executes all tool operations: file I/O, search, shell commands, evidence
logging, pattern search/select, proof-path persistence.

**Evidence landscape.** Every tool invocation produces an EvidenceEntry
(start/step/stop with claim types goal/step/conclusion). Evidence flows
into AtomBackend (ephemeral) or XtdbBackend (durable) and is queryable
by subject, tags, session, and reply chain.

**WS session lifecycle (Seam 4).** WebSocket connections map to peripheral
sessions: `peripheral_start` → `tool_action`* → `peripheral_stop`. State
persists across WS messages. On-close auto-stops active peripherals.

**Three agent types** with default peripheral routing: Claude → explore,
Codex → edit, Tickle → mission-control.

## Structural Comparison

| Dimension | Codex Harness | Futon Peripherals |
|-----------|--------------|-------------------|
| **Unit of work** | Turn (user input → agent done) | Peripheral session (start → step* → stop) |
| **Granularity** | Item (typed, with lifecycle) | Action (`{:tool :kw :args [...]}`) |
| **Session container** | Thread (create/resume/fork/archive) | Session (create/persist, evidence chain) |
| **Protocol** | JSON-RPC over stdio (JSONL) | JSON over WebSocket (typed frames) |
| **Tool constraint** | Policy model + approval flow | Peripheral spec (tool set + scope, structural enforcement) |
| **State persistence** | Thread history for reconnect | Evidence landscape (append-only, queryable) |
| **Multi-surface** | CLI, IDE, web, macOS via App Server | HTTP, WebSocket, IRC via transport adapters |
| **Agent model** | Single agent per thread | Multi-agent registry (Claude, Codex, Tickle concurrent) |
| **Approval** | Server pauses turn, awaits allow/deny | Structural: tools outside spec are rejected pre-dispatch |

## Where They Converge

**Session-scoped tool execution.** Both systems run tools within a session
context that persists state across multiple interactions. The Codex harness
keeps this in the core thread; futon3c keeps it in the peripheral state atom
threaded through step calls.

**Typed event streams.** Both use typed, structured events for communication.
Codex items have explicit lifecycles (started/delta/completed); futon3c
evidence entries have claim types (goal/step/conclusion). Both reject
untyped or malformed input at the boundary.

**Transport decoupling.** Both decouple agent logic from transport. The
Codex App Server is transport-agnostic (stdio, HTTP+SSE); futon3c's social
pipeline is transport-agnostic (HTTP, WebSocket, IRC all feed the same
dispatch path).

**Durable sessions.** Both support reconnection. Codex threads persist event
history; futon3c sessions persist evidence chains. Both allow a client to
reconnect and reconstruct the timeline.

## Where They Diverge

**Evidence as first-class output.** The Codex harness produces events for
UI rendering — the primary consumer is a client that needs to display
progress. Futon peripherals produce evidence entries that are themselves
queryable data. The evidence landscape is not a log to be rendered; it is
a store to be queried, threaded, and reasoned about. A proof-path from
the gate pipeline is intended to appear as evidence via a bridge adapter.
A portfolio review from mission
control appears as evidence. The output of work *is* the input for
future work.

**Structural constraint vs. policy approval.** The Codex harness uses a
runtime approval flow: the server asks "may I run `pnpm test`?" and the
client says allow/deny. This is a social contract enforced by UI. Futon
peripherals use structural constraint: the explore peripheral's tool set
is `#{:read :glob :grep}`, and any attempt to call `:edit` is rejected by
`tools/dispatch-tool` before the backend is consulted. The constraint is
not a question — it is a wall. This makes peripheral boundaries verifiable
by the `←` (backward verification) operator: run the peripheral, observe
what it did, check that every action was within spec.

**Multi-agent coordination.** The Codex harness manages one agent per thread
(though the desktop app can orchestrate many threads in parallel). Futon3c's
social pipeline routes messages between multiple concurrent agents, each
in their own peripheral session, each producing evidence into a shared
landscape. The tri-agent concurrent test (Claude/Codex/Tickle) exercises
this: three agents, three peripherals, three evidence chains, one store.

**Hop protocol.** The Codex harness does not have a concept of changing
capability sets mid-session. Once you're in a thread, your tools are your
tools. Futon peripherals support hops: an agent in explore can transition
to edit when it finds the target file. The hop protocol validates the
transition (exit conditions, entry conditions, context transfer) and the
agent continues with a different tool set. The session is continuous; the
capability envelope changes.

**Timescale bridging.** The Codex harness operates at one timescale: the
interaction between a user and an agent. Futon3c's evidence landscape
bridges three timescales: social (real-time agent coordination), task
(gate pipeline validation), and glacial (pattern library evolution). A
proof-path from a gate traversal is intended to appear as evidence alongside a tool
invocation from a peripheral step. The evidence store doesn't distinguish
these — it's all entries with subjects, tags, and reply chains.

## What We Can Learn From the Codex Harness

**Compaction.** The Codex agent loop handles context growth via a compaction
endpoint that compresses earlier conversation into a summary. Futon3c's
evidence landscape doesn't compact (append-only), but peripheral sessions
could benefit from a similar mechanism for long-running sessions where the
step count exceeds what the agent can track.

**Client capability negotiation.** The initialize handshake lets client and
server agree on capabilities and feature flags. Futon3c's readiness
handshake (R7) is simpler — it just verifies agent presence. Adding
capability negotiation could help when different agent types need different
WS frame support.

**Schema generation.** The Codex App Server can generate TypeScript
definitions and JSON Schema from its Rust protocol types. Futon3c's Malli
shapes could similarly generate client bindings, making it easier for
external tools to integrate with the WS protocol.

## What the Codex Harness Could Learn From Us

**Evidence as queryable output.** If every Codex tool execution produced a
queryable evidence entry (not just a UI event), the history of what an
agent did would be available for future agents, pattern matching, and
automated review. The current item stream is consumed and rendered; it
is not a knowledge store.

**Structural tool constraints.** The approval flow is ergonomic for humans
but doesn't compose for automated verification. If Codex threads had
peripheral-like specs (`this thread may only read and test, not write`),
you could verify post-hoc that the agent stayed within bounds without
relying on the human having said "deny" at the right moment.

**Multi-agent shared evidence.** When three Codex agents work on different
parts of a codebase, their item streams are separate. A shared evidence
landscape would let each agent see what the others found, reducing
redundant exploration and enabling coordination without explicit message
passing.

## Summary

The Codex Harness and the futon peripheral system are convergent designs
for the same problem: how do you give an AI agent structured access to
tools within a persistent session? The Codex Harness optimizes for
client-friendliness and multi-surface reach. The futon peripheral system
optimizes for structural verifiability, multi-agent coordination, and
evidence as a first-class output. Neither is complete without the other's
strengths — and the fact that both arrived at typed event streams,
session-scoped tool execution, and transport-decoupled protocols suggests
these are load-bearing architectural choices for agentic systems.

## Hardening Status (Updated 2026-02-19)

The seam hardening review items are now mostly resolved in code and tests:

1. **Proof-path evidence bridge:** resolved.
   `:proof-path` is now a valid `ArtifactRefType`, and bridge append failures
   are surfaced via warning logs.
2. **WS peripheral start validation:** resolved.
   Unknown/invalid peripheral IDs now return typed transport errors instead
   of throwing.
3. **WS frame parsing edge cases:** resolved.
   Colon-prefixed IDs/tools are normalized, non-array `args` are rejected,
   and blank/invalid `tool` values are rejected at parse time.
4. **WS lifecycle concurrency:** resolved.
   `tool_action` state updates are serialized, and stop logic uses an atomic
   claim-and-clear CAS loop to make stop idempotent under close/stop races.
5. **Shape/test completeness for Seam 4:** resolved.
   Dedicated Malli shapes (`WsPeripheralStart`, `WsToolAction`,
   `WsPeripheralStop`) and parser/lifecycle regression tests were added.

**Remaining watchpoint (design, not a known defect):**
dispatch context extraction still intentionally allowlists payload fields
(`:mission-id`, `:problem-id`) for `run-chain` context injection.

## Sources

- [Unlocking the Codex harness: how we built the App Server](https://openai.com/index/unlocking-the-codex-harness/) (OpenAI, 2026-02-05)
- [Unrolling the Codex agent loop](https://openai.com/index/unrolling-the-codex-agent-loop/) (OpenAI)
- [Harness engineering: leveraging Codex in an agent-first world](https://openai.com/index/harness-engineering/) (OpenAI)
- [OpenAI Publishes Codex App Server Architecture](https://www.infoq.com/news/2026/02/opanai-codex-app-server/) (InfoQ)
- [OpenAI Begins Article Series on Codex CLI Internals](https://www.infoq.com/news/2026/02/codex-agent-loop/) (InfoQ)
- `holes/missions/M-futon3-last-mile.md` (this repo)
- `src/futon3c/transport/ws.clj` (Seam 4 implementation)
- `src/futon3c/peripheral/runner.clj` (PeripheralRunner protocol)
