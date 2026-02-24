# Mission: Futon3x Last Mile — Independent Agent Runs

**Date:** 2026-02-19
**Status:** IDENTIFY (mission proposal, brushed up 2026-02-24)
**Blocked by:** None (all substrate prototypes settled or active; components
exist but are not yet integrated end-to-end)
**Cross-ref:** M-self-representing-stack (futon4) makes this mission's
evidence landscape navigable; M-improve-irc (futon3c) is a side quest
for transport reliability

## Motivation

The futon3x series (futon3, futon3a, futon3b, futon3c) has been built from
the bottom up across 16 prototypes. The substrate layers are now settled:

- **f2/P0 AIF Stack Baseline**: :settled (2026-02-19). Golden microtraces
  locked, parameters frozen, regression tests pass. The AIF loop
  (observe→perceive→affect→policy→act) is the reference implementation
  that the social-timescale cycle machine lifts to the coordination level.

- **f3/P0 MUSN Transport Baseline**: :settled (2026-02-19). Transport
  contract v1 frozen, golden transcripts reproducible, CI replay wired.
  futon3c's independent transport layer (HTTP+WS+IRC, 112 tests) provides
  the successor.

On top of these, futon3c has:
- 841 tests (as of 2026-02-24), 11 peripherals (10 wired)
- Social pipeline (5 stages: presence→auth→mode→dispatch→persist)
- Evidence landscape (AtomBackend + XtdbBackend via futon1a)
- Agency registry with session lifecycle (Claude, Codex, Tickle)
- Transport layer (HTTP + WebSocket + IRC, F1-F6 stability)
- RealBackend implementing all tool operations (574 lines)
- Mission + mission control peripherals
- Runtime helpers (agents.clj) that wire everything together

And futon3b has:
- Gate pipeline (G5→G0) with typed evidence and structured rejections
- L1 canonicalizer (tension observer + Baldwin cycle)
- Proof-path store (durable EDN)

**The gap is integration, not implementation.** The pieces exist but have
never been assembled into a running system where an agent connects,
enters a peripheral, executes tool operations against real files,
emits evidence, and disconnects — all without a human in the loop.

The IFR from futon3 still applies: *FUTON3 turns messy activity into
organised knowledge by checking work against shared patterns and
producing auditable records that other futons can use to learn, improve,
and act.* The result of *this* mission is that we can see it actually
working.

## Theoretical Anchoring

### 1. The Social AIF Loop

The futon3c cycle machine (`peripheral/cycle.clj`) is the futon2 AIF
loop lifted to the coordination timescale:

| AIF (futon2 ants)       | Social AIF (futon3c agents)           |
|-------------------------|---------------------------------------|
| observe (sensory input) | observe (read codebase, corpus check) |
| perceive (belief update)| propose (design approach)             |
| affect (precision)      | validate (test, verify)               |
| policy (action select)  | classify + integrate (evidence emit)  |
| act (motor output)      | commit + gate-review (durable output) |

Both f2/P0 and f3/P0 being :settled means the two ends of this analogy
are stable. This mission connects the middle.

### 2. The Three Timescales, Connected

| Timescale | System | Status |
|-----------|--------|--------|
| Social (real-time) | futon3c social pipeline + peripherals | Tested (614), not live |
| Task (fast) | futon3b gate pipeline (G5→G0) | Tested (30), not bridged to evidence landscape |
| Glacial (slow) | futon3b L1 canonicalizer + futon3a similarity | Tested, not wired to social layer |

This mission bridges social↔task by connecting peripheral evidence
emission to the evidence landscape, and by routing gate pipeline
proof-paths as evidence entries.

### 3. Futonic Logic at the Integration Level

```
(futonic-loop
  (input   象  := the assembled stack — transport, social pipeline,
                   peripherals, evidence, gate pipeline, runtime config)
  (choose  部  := select integration seams to wire)
  (articulate 咅 := make each seam explicit with a test)
  (if (forms 鹽) := do the components compose at runtime?)
  (and (sense 香) := can an agent perceive its own evidence trail?)
  (and (regulate 🔮) := do tool-set constraints hold under real execution?)
  (then
    (act-within 未知 := run the tri-agent scenario end-to-end)
    (evaluate 味 := did evidence land in the landscape? are proof-paths queryable?))
  (else
    (apply 捨 := record what failed to compose, file as integration tension)))
```

### 4. Corneli Table 24 Instantiation

| Entity | Last-Mile Level | Implementation |
|--------|----------------|---------------|
| X (project) | This mission | M-futon3-last-mile |
| P (problem) | Integration gaps | 5 seams identified below |
| J (conjecture) | "Components compose at runtime" | End-to-end test |
| S (solution) | Wiring code + smoke tests | Parts I-III below |
| H (heuristic) | Patterns from existing library | `stack-coherence/*`, `realtime/*` |
| E (evidence) | Integration tests + live evidence entries | Success = evidence in landscape |

## What's Changed Since IDENTIFY (2026-02-24 brush-up)

### The Parallel Integration Path

While this mission waited at IDENTIFY, a de facto integration happened
through `dev.clj` — the REPL-driven runtime that wires agents for
day-to-day use:

- **Claude and Codex invoke end-to-end**: `make-claude-invoke-fn` and
  `make-codex-invoke-fn` in dev.clj call the agents, stream responses,
  and update the registry. This bypasses the formal PeripheralRunner path
  but achieves the same result: agents run, produce output, and their
  state is tracked.
- **IRC transport is live**: agents receive and respond to IRC messages
  via the relay bridge. Transport routing works (I-2 compliant).
- **Agents blackboard**: registry projects live agent status (idle/invoking,
  activity tracking, relative timestamps) to Emacs via emacsclient.
- **Evidence persists via XTDB**: the evidence landscape is operational
  and queryable via HTTP API.
- **Mission Control runs**: Tickle invokes MC portfolio reviews that
  scan repos and produce coverage data.

**The gap this reveals:** The formal peripheral runner path (PeripheralRunner
→ RealBackend → tool dispatch → evidence emission) is tested but not the
path agents actually use. The dev.clj path is pragmatic but doesn't enforce
peripheral constraints (tool-set boundaries, exit conditions, capability
envelopes). The last-mile work is making the formal path the *actual* path.

### The futon3a Gap

(Identified 2026-02-24, conversation between Joe and Claude.)

During the M-self-representing-stack DERIVE session, pattern searches were
done by generic Explore agents grepping `.flexiarg` files — not by querying
futon3a's pattern search API. futon3a has notions/compass for pattern search,
but the day-to-day workflow doesn't route through it. The PSR/PUR discipline
exists in CLAUDE.md instructions, but the infrastructure path (agent wants
pattern → queries futon3a → gets ranked results → selects → records PSR)
isn't wired end-to-end.

This is the cross-repo integration gap: the futon3x trilogy is individually
functional but not yet an organism. The flows (3c routes to 3a for patterns,
3a results feed into 3b's gate pipeline, 3b evidence flows back through 3c)
are still manual. This mission's scope-out of futon3a integration ("separate
mission") may need revisiting — without 3a, the pattern search loop that
makes agents self-improving is bypassed.

## Scope In

- End-to-end agent session: WS connect → social pipeline → peripheral →
  tool execution → evidence emission → response
- futon3b ↔ futon3c evidence bridge (proof-paths as evidence entries)
- Tri-agent smoke test with real tool backends (Claude, Codex, Tickle)
- Mission control portfolio review producing real evidence
- Validation that the whole stack works under `clj -X:test`

## Scope Out

- futon3a similarity search integration into G3 (separate mission)
- Emacs UI / sliding blackboard (M-sliding-blackboard)
- Multi-peripheral hop sequences (M-peripheral-gauntlet — dogfood target)
- Forum refactor (M-forum-refactor — dogfood target)
- Mana system activation (stays in futon5)
- New peripheral implementations (chat wiring, etc.)

## What Exists (MAP inventory)

### Transport → Social Pipeline Path

| Component | File | Status |
|-----------|------|--------|
| WS adapter | `transport/ws.clj` | Wired. Calls S-presence, S-dispatch. |
| HTTP adapter | `transport/http.clj` | Wired. 15 tests. |
| S-presence | `social/presence.clj` | Wired. Agent lookup in registry. |
| S-authenticate | `social/authenticate.clj` | Wired. |
| S-mode | `social/mode.clj` | Wired. Resolves peripheral from agent type. |
| S-dispatch | `social/dispatch.clj` | Wired. Routes to peripheral runner. Knows Claude→explore, Codex→edit, Tickle→mission-control. |
| S-persist | `social/persist.clj` | Wired. Emits coordination evidence. |

### Peripheral → Tool Execution Path

| Component | File | Status |
|-----------|------|--------|
| PeripheralRunner protocol | `peripheral/runner.clj` | Defined. start/step/stop. |
| Tool dispatch | `peripheral/tools.clj` | ToolBackend protocol defined. |
| RealBackend | `peripheral/real_backend.clj` | **Implemented (574 lines).** read, glob, grep, edit, write, bash, bash-readonly, web-fetch, musn-log, psr-search, psr-select, pur-update, pur-mark-pivot, par-punctuate. |
| Evidence emission | `peripheral/evidence.clj` | Wired. make-start/step/stop-evidence. |
| Peripheral registry | `peripheral/registry.clj` | 11 specs loaded. Factory dispatch. |

### Runtime Wiring

| Component | File | Status |
|-----------|------|--------|
| Agent registration | `runtime/agents.clj` | register-claude!, register-codex!, register-tickle! |
| Default peripheral config | `runtime/agents.clj` | make-default-peripheral-config builds RealBackend |
| Persistent config | `runtime/agents.clj` | make-persistent-peripheral-config wraps XTDB |
| Runtime config | `runtime/agents.clj` | runtime-config assembles registry + patterns |
| HTTP handler factory | `runtime/agents.clj` | make-http-handler |
| WS handler factory | `runtime/agents.clj` | make-ws-handler |

### Evidence Landscape

| Component | File | Status |
|-----------|------|--------|
| EvidenceBackend protocol | `evidence/backend.clj` | Defined. AtomBackend implemented. |
| Evidence store API | `evidence/store.clj` | append/query/reply-chain/fork (~300 lines) |
| XTDB backend | `evidence/xtdb_backend.clj` | XtdbBackend via futon1a HTTP API |
| Thread projection | `evidence/threads.clj` | Reply-chain → thread view |

### Gate Pipeline (futon3b)

| Component | File | Status |
|-----------|------|--------|
| Pipeline (G5→G0) | `futon3b/src/futon3/gate/pipeline.clj` | 30 tests, 107 assertions |
| Proof-path store | `futon3b/src/futon3b/query/relations.clj` | append/load/search |
| L1 observe + canon | `futon3b/src/futon3/gate/{observe,canon,level1}.clj` | Operational |

### What's Missing (Integration Seams)

**Seam 1: S-dispatch → peripheral runner (live tool execution)**

S-dispatch knows *which* peripheral to route to, but the current
dispatch path calls `invoke-fn` on the agent rather than starting a
peripheral runner with a real backend. The peripheral bridge
(`dispatch.clj` Part I notes) needs completion: dispatch should
instantiate a PeripheralRunner with a RealBackend, call `start`, route
tool requests through `step`, and call `stop` on session end.

**Seam 2: Peripheral evidence → evidence landscape**

Peripherals emit evidence maps (`make-step-evidence`, etc.) but the
`common/maybe-append-evidence!` path depends on `:evidence-store` in
state. When wired through the runtime config, evidence should flow
into the AtomBackend (ephemeral) or XtdbBackend (durable) automatically.
Need to verify this path works end-to-end and that evidence is queryable
after a peripheral session.

**Seam 3: futon3b proof-paths → futon3c evidence entries**

The gate pipeline produces proof-path EDN files in `data/proof-paths/`.
These should also appear as evidence entries in the futon3c evidence
landscape. Two options: (a) the RealBackend's `pur-update` tool already
calls `persist-proof-path!` — verify this produces queryable evidence;
(b) add a proof-path → evidence adapter that converts proof-path EDN to
EvidenceEntry and appends to the landscape.

**Seam 4: WS session lifecycle → peripheral lifecycle**

A WebSocket connection represents an agent session. The WS adapter needs
to map: on-connect → register agent + start peripheral; on-message →
peripheral step; on-close → peripheral stop + deregister. The
`tri_agent_ws_alleycat_smoke.clj` script validates concurrent connection
but doesn't exercise tool execution.

**Seam 5: Tri-agent end-to-end with real tools**

The final validation: three agents connect, each enters their default
peripheral (Claude→explore, Codex→edit, Tickle→mission-control), each
executes at least one tool operation against the real filesystem, each
produces evidence, and the evidence is queryable from the landscape.

## Derivation Path

### Part I: Wire the Seams (DERIVE)

Wire seams 1-4 with minimal code changes. The goal is composition,
not new features.

**1a. Peripheral session management in S-dispatch**

S-dispatch needs a session-scoped peripheral runner. When a message
arrives for an agent with a resolved peripheral, dispatch should:
- Look up or create a PeripheralRunner for that agent's session
- Call `start` if new, `step` with the tool request, `stop` on disconnect
- Thread the runtime config's RealBackend and evidence-store through

**1b. Evidence store threading**

Verify that `make-default-peripheral-config` → RealBackend → peripheral
→ `maybe-append-evidence!` → evidence store is a complete path. Write
a test that starts a peripheral with a real backend, executes a tool,
and verifies evidence appeared in the store.

**1c. Proof-path → evidence bridge**

The RealBackend's `persist-proof-path!` calls `relations/append-proof-path!`
which writes EDN files. Add a complementary call that also appends a
synthetic evidence entry to the evidence landscape, so proof-paths are
queryable from both stores.

**1d. WS lifecycle → peripheral lifecycle**

Extend the WS adapter's on-message handler to route tool requests
through the peripheral runner's `step` method. Extend on-close to call
`stop` and collect fruit.

### Part II: Smoke Tests (VERIFY)

**2a. Single-agent peripheral round-trip**

Test: register a mock agent, start an explore peripheral with RealBackend,
execute `:read` on a known file, verify evidence entry was emitted with
correct tool/args/result.

**2b. Evidence landscape round-trip**

Test: execute a full peripheral session (start → 3 steps → stop), query
the evidence store for the session-id, verify the evidence chain is
complete (start → steps → stop, all linked via in-reply-to).

**2c. Proof-path evidence bridge**

Test: call `pur-update` through a peripheral, verify both a proof-path
EDN file and an evidence entry exist.

**2d. Tri-agent concurrent session**

Test: three agents connect via WS, each executes one tool through their
default peripheral, all three produce evidence, evidence is queryable.
This extends `tri_agent_ws_alleycat_smoke.clj` with real tool execution.

### Part III: Live Demonstration (INSTANTIATE)

**3a. Tickle portfolio review**

Run Tickle through the mission-control peripheral against the live
repos. Tickle calls `:mc-review`, which scans all repos and produces
a portfolio review. The review lands as an evidence entry. Query it
back from the evidence landscape.

**3b. Claude codebase exploration**

Run Claude through the explore peripheral. Claude reads a file, greps
for a pattern, globs for related files. Each tool invocation produces
evidence. The evidence chain is queryable.

**3c. Codex scoped edit**

Run Codex through the edit peripheral. Codex reads a file, makes an
edit, runs a test. Each step produces evidence. The evidence chain
is queryable.

**3d. Cross-agent evidence query**

After all three agents have run, query the evidence landscape for
all entries in the session. Verify that entries from all three agents
appear, are correctly typed, and form valid reply chains.

## Success Criteria

| Criterion | What It Means |
|-----------|--------------|
| Agent connects via WS and enters peripheral | Social pipeline routes to the correct PeripheralRunner |
| Tool execution produces real results | RealBackend reads/writes actual files |
| Every tool invocation emits evidence | Evidence entries appear in the landscape |
| Evidence chains are linked and queryable | in-reply-to threading, session-id filtering |
| Proof-paths appear as evidence entries | futon3b proof-paths bridge to futon3c evidence |
| Three agents run concurrently | Claude (explore), Codex (edit), Tickle (mission-control) |
| Portfolio review produces real output | Mission control scans live repos and emits review evidence |
| All existing tests still pass | No regressions in the 841-test suite |

## Dogfood Targets

Once this mission completes, the following missions become candidates for
dogfooding — running through the infrastructure this mission validates:

- **M-peripheral-gauntlet**: real-time multi-peripheral scenarios.
  Agents hop between peripherals during live sessions. Requires the
  end-to-end path this mission wires.

- **M-forum-refactor Parts II-III**: forum as evidence landscape.
  Forum posts become evidence entries. Requires the evidence emission
  path this mission validates.

- **M-sliding-blackboard**: Emacs UI integration. Requires transport
  and evidence endpoints to be live.

These are not dependencies — they are *consumers* of what this mission
produces. The measure of success for M-futon3-last-mile is that these
missions can be attempted with real agents running through real
peripherals.

## Relationship to Adjacent Work

| Mission/Component | Relationship |
|-------------------|-------------|
| f3/P0 (MUSN Transport) | :settled — this mission builds on the successor transport in futon3c |
| f2/P0 (AIF Stack) | :settled — the cycle machine is the social-timescale AIF loop |
| f3/P12 (Social Pipeline) | Consumed — this mission wires the pipeline to real execution |
| f3/P13 (Evidence Landscape) | Consumed — this mission makes evidence flow end-to-end |
| f3/P14 (Peripheral Runtime) | Consumed — this mission runs peripherals with real backends |
| f3/P15 (Transport Layer) | Consumed — this mission runs agents through live WS connections |
| f3/P16 (Mission System) | Consumed — Tickle runs mission-control for the first real portfolio review |
| futon5 ct/mission.clj | Validates — mission wiring diagrams describe what this mission assembles |

## Source Material

| Source | What We Take |
|--------|-------------|
| `futon3c/src/futon3c/runtime/agents.clj` | Runtime wiring: register agents, build config, create handlers |
| `futon3c/src/futon3c/peripheral/real_backend.clj` | Tool execution: all 15+ tools implemented |
| `futon3c/src/futon3c/social/dispatch.clj` | Route selection: Claude→explore, Codex→edit, Tickle→mission-control |
| `futon3c/src/futon3c/transport/ws.clj` | WS lifecycle: connect → handshake → dispatch → close |
| `futon3c/src/futon3c/peripheral/evidence.clj` | Evidence emission: start/step/stop evidence helpers |
| `futon3c/src/futon3c/evidence/store.clj` | Evidence API: append/query/reply-chain |
| `futon3c/scripts/tri_agent_ws_alleycat_smoke.clj` | Existing concurrent agent smoke test (to be extended) |
| `futon3b/src/futon3b/query/relations.clj` | Proof-path store: append/load/search |
| `futon3c/src/futon3c/peripheral/mission_control_backend.clj` | Portfolio review: inventory, devmaps, coverage |
