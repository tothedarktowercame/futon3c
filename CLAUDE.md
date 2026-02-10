# Claude Code Instructions for Futon3c

## What This Project Is

Futon3c is the **real-time coordination** layer of the futon stack: Agency
(multi-agent routing), peripherals (capability envelopes), Forum (collaborative
proof trees), and the bridges that connect agents to each other in real time.

This is the "social" AIF loop — the fastest timescale in the futon system,
where agents coordinate, hand off work, and maintain shared awareness.

## The Three-Futon Refactoring

Futon3 was doing too much. It has been refactored into three focused repos:

| Repo | Concern | Timescale | AIF Loop |
|------|---------|-----------|----------|
| futon3a | Pattern search + querying | fast (query) | Infrastructure (not a loop) |
| futon3b | Pattern-driven development | task + glacial | L0 gate pipeline + L1 library evolution |
| **futon3c** | **Real-time coordination** | **social (real-time)** | **Agency, peripherals, forum** |

Each repo is a focused organism budded off from the original futon3. They
interoperate but have distinct concerns and timescales.

## What Belongs Here (from futon3)

### Core Services
- `agency/` — Multi-agent registry, peripheral dispatch, session management
- `forum/` — Collaborative proof trees, thread/post model, event streaming
- `drawbridge/` — Message routing (refactored: agents register, not spawned)

### Bridges
- Forum bridges (Babashka + TypeScript) — autonomous forum participation
- IRC bridge — MUSN ↔ IRC relay
- Peripheral wrappers (fuclaude, fucodex) — unified input multiplexing

### Patterns
- `library/realtime/` — 13 coordination patterns:
  authoritative-transcript, listener-leases, transport-pivot,
  liveness-heartbeats, rendezvous-handshake, loop-failure-signals,
  loop-recovery-actions, loop-success-signals, mode-gate,
  single-line-transport, structured-events-only, branch-parallelism,
  learn-as-you-go

### Key Design Documents
- Peripheral spec (explore/edit/test/deploy/reflect capability envelopes)
- PAR session punctuation (detach/reattach model)
- MUSN event schema (PSR/PUR/PAR structure)

## The Futon Ecosystem

| Repo | What futon3c uses from it |
|------|--------------------------|
| futon3 | Original codebase (source of code being refactored here) |
| futon3a | Pattern search (notions/compass) for agent queries |
| futon3b | Gate pipeline — agents submit work through gates |
| futon5 | Wiring diagrams for AIF loop specification |
| futon1a | Durable store for session evidence |

## Development Protocol

Follow the futonic methodology (see futon3b/AGENTS.md for the full guide):

- **Derivation xenotype**: IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE
- **PSR/PUR discipline**: Record pattern selections and outcomes
- **Evidence-first**: Specific counts and file names, not vague claims
- **Argument form**: IF/HOWEVER/THEN/BECAUSE for design decisions

## Key Integration Points

1. **Agency → futon3b gates**: When an agent completes work, the result
   flows through the gate pipeline (G5→G0) for validation and evidence
   persistence. Futon3c handles the real-time dispatch; futon3b handles
   the quality assurance.

2. **Forum → futon3b evidence**: Forum posts that contain PSRs/PURs/PARs
   feed into the gate pipeline's evidence graph. The forum is where
   coordination happens; the gates are where it gets validated.

3. **Peripherals → futon3a search**: Peripheral modes (especially explore)
   use futon3a's pattern search to find relevant patterns for the current
   task context.

4. **Detach/Reattach → proof paths**: When an agent detaches (goes
   autonomous), it emits a PAR. When it reattaches, the autonomous work
   becomes evidence in the proof path. This bridges social and task timescales.

## Codex Handoff Protocol

Use **GitHub issues** for Codex handoffs, not copy-paste. The issue becomes the
canonical task description, is linkable, and Codex can commit against it. Each
issue follows the scope-bounded-handoff pattern (R11):

- Title: what to implement
- `:in` files (READ-ONLY) and `:out` files (create these)
- Function signatures
- Shapes reference (relevant types from shapes.clj)
- Test expectations (specific test cases)
- Criteria checklist
- `clojure -X:test` must pass

This is a temporary ergonomic — once futon3c's own real-time coordination
layer is operational, handoffs flow through the evidence landscape instead.

## Current State

Code is being ported from futon3 via scoped missions. The source material is in:

- `/home/joe/code/futon3/src/futon3/agency/` (6 files)
- `/home/joe/code/futon3/src/futon3/forum/` (3 files)
- `/home/joe/code/futon3/src/futon3/drawbridge/` (3 files)
- `/home/joe/code/futon3/scripts/` (bridges, peripherals)
- `/home/joe/code/futon3/library/realtime/` (13 patterns)
- `/home/joe/code/futon3/docs/peripheral-spec.md`
- `/home/joe/code/futon3/holes/missions/M-par-session-punctuation.md`
- `/home/joe/code/futon3/holes/missions/M-drawbridge-multi-agent.md`

## When You're Stuck

1. Search futon3 for the original code: `~/code/futon3/src/futon3/agency/`
2. Search the pattern library: `~/code/futon3/library/realtime/`
3. Check the peripheral spec: `~/code/futon3/docs/peripheral-spec.md`
4. Check futon3b's AGENTS.md for the development protocol
5. Search session history: `~/.claude/projects/`
