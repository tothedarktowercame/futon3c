# Claude Code Instructions for Futon3c

## What This Project Is

Futon3c is the **real-time coordination** layer of the futon stack: Agency
(multi-agent routing), peripherals (capability envelopes), Forum (collaborative
proof trees), and the bridges that connect agents to each other in real time.

This is the "social" AIF loop — the fastest timescale in the futon system,
where agents coordinate, hand off work, and maintain shared awareness.

## Inhabiting the War Machine Pilot

**If you are taking on the pilot role — driving the War Machine as a REPL —
read `README-pilot.md` first.** It is the map for inhabitants: the spec
(`holes/specs/repl.spec.edn`), the loop apparatus (`war_machine_pilot.clj`,
`repl_trace.clj`, `loop_learning.clj`, `repl_spec_verify.clj`), how to run a
cycle, and the hard disciplines (never restart the JVM; `request-tick!` not
`tick!`; consent-gate / gate-at-merge; earned closure; the E-cheesemonger
hole-budget). The pilot REPL is the differential operator `v·∇` over the WM
field; the LOOP turn keeps the substrate AND the VSATARCS documentation current.

## Canonical Wiring Contract

Before implementing architecture changes, read:

- `docs/wiring-contract.md` (futon3c projection contract)
- `docs/wiring-claims.edn` (machine-readable architectural claims)
- `docs/wiring-evidence.edn` (commit-scoped evidence for each claim)
- `docs/mission-contract.md` (Mission Peripheral / Mission Control / War Room contract)
- `docs/mission-claims.edn` (mission-surface claims)
- `docs/mission-evidence.edn` (commit-scoped mission-surface evidence)
- `/home/joe/code/futon5/data/missions/social-exotype.edn` (social topology)
- `/home/joe/code/futon5/data/missions/coordination-exotype.edn` (task/glacial topology)
- `/home/joe/code/futon5/data/missions/futon3-coordination.edn` (concrete gate pipeline)

Tests are necessary but not sufficient. Topology changes must update the
wiring contract and projection mapping, not just test assertions.

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

## Architectural Invariants

These are hard constraints. Violating any of them is a design error, not a
tradeoff. If you find yourself reaching for a pattern that conflicts with
these invariants, stop and rethink.

### I-0: One JVM Is Plenty

**There is exactly one serving JVM on this machine: the futon3c JVM.** It
hosts the futon3c API (port 7070), the futon1a / substrate-2 hyperedge
store (7071), the WebArxana app (3100), the War Machine API endpoints
(`/api/alpha/war-machine`, `/api/alpha/aif-stack/live`, etc., all on 7070),
and the Drawbridge nREPL-over-HTTP (6768). Everything serves out of this
one process.

Do **NOT** start a second runtime JVM for any of the apps that have already
been folded in (WebArxana, War Machine, etc.) — their backends live here.
A second JVM is only acceptable for short-lived dev tooling like a CLJS
shadow-cljs watcher when actively editing ClojureScript source; that tool
exits when you stop editing CLJS, and it serves nothing in the request
path (futon3c serves the pre-compiled JS as static files).

**Test:** `pgrep java` should return one PID at rest. If you see more
than one, the second one is either a stale shadow-cljs watcher (kill it)
or a regression that needs to be folded back in. Historical context:
this consolidation was completed 2026-05-23 (M-weird-modernism's ARGUE
round). Before then, three JVMs were running (futon3c, WebArxana
shadow-cljs, War Machine shadow-cljs); shadow-cljs processes for those
apps are dev-tooling-only and should not be left running.

**For code reloads inside the futon3c JVM, use Drawbridge nREPL** (per
`README-drawbridge.md`); do not kill and restart the serving JVM.

### I-1: Agent Identity Is Singular

One agent = one session = one identity. An agent that is already running is
never "represented" by spawning a new process. If Claude is running in this
CLI session, IRC messages are **routed to this session** via the transport
layer — never farmed out to a new `claude -p` invocation. The agent inhabits
its peripherals; it does not delegate them to clones.

**Test**: grep for `sh/sh.*claude` or `ProcessBuilder.*claude` in transport
or dev code. If it exists, it's a violation.

### I-2: Transport Routes, It Does Not Create

The transport layer (IRC, WS, HTTP) routes messages to agents that already
exist. It never creates new agents, spawns new processes, or makes API calls
on behalf of agents. The relay bridge translates between transport protocols
(IRC PRIVMSG ↔ WS frame). That's all.

**Test**: transport code should have zero requires on `clojure.java.shell`,
`ProcessBuilder`, or any AI SDK. If it imports these, it's a violation.

### I-3: Peripherals Are Inhabited, Not Delegated

When an agent enters a peripheral (chat, explore, edit), the **same agent**
operates within that peripheral's constraints. The peripheral constrains what
the agent can do (tools, scope, exit conditions). It does not hand the work
to a different agent or process. Inhabitation means: the agent's context,
memory, and identity persist across the peripheral boundary.

**Test**: peripheral code should never spawn subprocesses that act as the
agent. Helper processes (language servers, test runners) are fine. Agent
impersonation is not.

### I-4: Read Before You Write

Before writing new code, exhaustively search for existing implementations.
The futon stack has scripts, bridges, and modules that already solve many
problems. The correct workflow is: search `scripts/`, search futon3 source
material, read the mission doc, understand what's already built — THEN wire
it together. Writing new code to solve an already-solved problem wastes time,
introduces bugs, and ignores hard-won design decisions embedded in the
existing implementation.

**Discipline**: When starting any integration task, spend the first pass
reading — `Glob` for related files, `Grep` for related functions, `Read`
the scripts directory. Only after you understand what exists should you
decide whether new code is needed. If the answer is "this already exists
in a script," the task is wiring, not writing.

**Test**: before creating a new `.clj` file, search for existing files with
similar names or purposes. If you find one, read it fully before proceeding.
If you find yourself reimplementing logic that exists in `scripts/`, stop.

### I-5: No futon3 Dependencies

futon3 is source material for porting, not running infrastructure. Nothing
in futon3c may depend on futon3 being importable or on the classpath. See
M-peripheral-gauntlet §"Foundational Constraint." When porting from futon3,
the existing code documents what worked and what failed — read it as design
documentation, not as code to copy blindly.

## Agent Prompting: Surface Contracts

When agents operate across multiple surfaces (IRC, Emacs buffer, WS), they
need factual context about where their output will appear. The correct pattern
is **surface contracts**: tell the agent what surface it's on and how its
output will be delivered. This is transport metadata, not capability restriction.

**Good** (surface context — invariant-compliant):
- "Current surface: IRC. Your returned text will be posted to #futon as \<nick\>."
- "Current surface: emacs-codex-repl. Your response is shown only in this buffer."
- "Do not claim to write relay files unless this turn actually executed such a tool."

**Bad** (capability restriction — violates the spirit of I-1/I-3):
- Adding `--tools ""` to disable agent tools
- Injecting system prompts that tell agents to "keep responses short" or "avoid tool use"
- Setting `--effort low` to lobotomize the agent

The distinction: surface contracts give agents accurate information about their
environment. Capability restrictions make agents less capable. We want fully
capable agents that know where they are.

**Reference commit**: `f5c3e25` — "Enforce explicit surface contracts for Codex
replies" — `irc-invoke-prompt` in dev.clj and `codex-repl--surface-contract` in
codex-repl.el.

### Typed Bell Contract

When `FUTON3C_TYPED_BELLS` is enabled, a bell may declare its illocutionary
type with `type` and an optional referent with `ref`. Valid types are `query`,
`answer`, `assert`, `challenge`, `agree`, `define`, `retract`, `suggest`, and
`request`. Untyped bells remain ordinary requests; unknown types are rejected.

Use `type=query` when you are asking another agent to help resolve a question.
If no `ref` is supplied, the server creates an ArSE thread and stamps the thread
id into the job's `ref`. Use `type=answer --ref <ask-id>` when answering that
thread; answers without a `ref` are malformed.

Shell example:

```bash
python3 futon3c/scripts/agency_send.py --to codex-1 --from claude-6 \
  --kind bell --type query <<'EOF'
Is the S3 ArSE bridge active in this JVM?
EOF
```

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
