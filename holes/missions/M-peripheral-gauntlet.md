# Mission: Peripheral Gauntlet

## Derivation

Parallel to M-peripheral-phenomenology (active), building on the gauntlet
pattern language (library/gauntlet/ARGUMENT.flexiarg) and evidence from the
ALFWorld session (2026-02-13), futon6 first-proof session (2026-02-11),
PLoP 2025 paper (p4ng/main.tex), and the social/ARGUMENT (complete).

Prior:
- M-peripheral-phenomenology: P-1..P-6 invariants, validated by ALFWorld,
  alleycat race, mid-game PAR hop, self-authored PAR
- social/ARGUMENT.flexiarg: S1-S7 (why futon3 failed), R1-R11 (social
  requirements), T1-T5 (theory), C1-C6 (convergence), thesis
- gauntlet/ARGUMENT.flexiarg: seven patterns (P1-P7) forming a dependency
  chain from umwelt to teaching inversion
- futon6 first-proof: 89 commits, 10 problems, 7.5 hours, 3 agents,
  human-as-placenta evidence
- PLoP 2025 paper: AIF compliance failure documented, PSR/PUR discipline
  but degraded code quality when AIF was instruction-shaped
- Drawbridge exploration (2026-02-13): P-1 and P-6 already implemented
  structurally in drawbridge/core.clj
- ALFWorld peripheral: 2 games (1.0/1.0 each), first-person language,
  mid-game PAR hop, delegation vs inhabitation distinction
- futon4 hyperedge model: N-ary typed relations (:hx/type, :hx/endpoints)
  stored in XTDB via arxana-store.el
- futon5 wiring diagram calculus: typed directed graphs with composition
  laws (compose-serial, compose-parallel), CT vocabulary

## Foundational Constraint: No Dependency on futon3

futon3 is being fully and completely replaced by the three-way refactor:
futon3a (pattern search + querying), futon3b (pattern-driven development),
futon3c (real-time coordination). When this mission is complete, there
will be no dependency on the original futon3. It is source material for
porting, not running infrastructure.

This means: every time a gate's status says "code exists in futon3," that
is a porting task, not an asset. The futon3 codebase documents what was
built, what worked, and what failed (S1-S7 in the social argument). The
refactored repos carry forward what worked and fix what didn't. Nothing in
the gauntlet may depend on futon3 being up, importable, or on the
classpath.

Concretely:

| futon3 component | Destination | Gate affected |
|---|---|---|
| drawbridge/core.clj (IRC bridge, multiplex, routing) | futon3c | Gate 0, Gate 3 |
| musn/ (schema, router, service, http) | futon3c | Gate 2 |
| agency/ (whistle, dispatch, registry) | futon3c | Gate 0, Gate 6 |
| scripts/ (fuclaude, fucodex peripherals) | futon3c | Gate 0, Gate 5 |
| library/ (patterns, flexiarg) | futon3a | Cross-cutting invariant |
| AIF engine wiring (futon2.aif.engine) | futon3b or futon3c (TBD) | Gate 4 |
| f2/musn.clj (bootstrap) | futon3c | Gate 2 |

The evidence trail below references futon3 artifacts as source material.
These references document provenance, not dependencies. Each must be
ported before its gate can pass.

## Foundational Dependency: futon1a (Durable Store)

futon1a is the storage foundation for the gauntlet. It provides XTDB +
RocksDB persistence with 5 non-negotiable invariant layers:

| Layer | Invariant | What it guarantees for the gauntlet |
|---|---|---|
| I0 Persistence | What you save is what you get back | MUSN world state survives restarts |
| I1 Identity | One entity per identity, no ambiguity | No duplicate agent registrations, clean co-presence |
| I2 Integrity | Startup succeeds completely or fails loudly | No partial world state after crash |
| I3 Hierarchy | Errors surface at the layer that caused them | Diagnosable failures in gate demonstrations |
| I4 Debugging | Any bug diagnosable in under 10 minutes | Rapid debugging during overnight sessions |

futon1a was made failproof through evidence-backed design: 9 tensions
extracted from futon1 git history, patterns selected from the library,
PSR/PUR records written before and after implementation, strict invariant
discipline throughout. The same process applies to the gauntlet.

**Integration points:**

| Gauntlet component | What it stores in futon1a |
|---|---|
| Gate 2 (MUSN) | All MUSN entities, relations, events — the game world |
| Gate 3 (Co-Presence) | Agent presence entities (UUID + external-id per agent) |
| Gate 5 (Modeline) | PAR records for context reset recovery |
| Gate 7 (Teaching) | Discovery entities with evidence links |
| Phase 4 (futon3b) | Proof-path evidence for demonstrated gates |
| Cross-cutting (patterns) | Pattern usage metrics, PSR/PUR history |

futon1a's write pipeline (L4→L0) is the gate the gauntlet's data passes
through. futon1a's proof-path protocol (8 phases: CLOCK_IN → CLOCK_OUT)
and futon3b's gate pipeline (G5→G0) are complementary: futon3b validates
the work, futon1a persists the evidence. Both produce ordered, auditable
records.

**Operational requirement**: futon1a must be running (default port 7071)
for any gauntlet gate to be demonstrated. This is not a futon3 dependency
— futon1a is a distinct, independently validated system.

## Why This Mission Exists

M-peripheral-phenomenology asks: what makes a peripheral a peripheral?
It answers with six invariants (P-1..P-6) that define inhabitation for
a single agent in a single world.

This mission asks: what happens next?

When two agents both inhabit peripherals in the same world, when those
agents need to self-regulate without human intervention, when the human's
functions must transfer to infrastructure, and when the agents discover
things the human hasn't seen — the phenomenological invariants are
necessary but not sufficient. They ensure someone is inside each
peripheral. They don't ensure the inhabitants can see each other, sustain
themselves, or teach what they've learned.

The social/ARGUMENT ensures the plumbing works: typed events, delivery
guarantees, routing authority, lifecycle management. The gauntlet
argument ensures someone is home — and that the home is livable for
extended autonomous residence.

The name has two meanings:

1. **A gauntlet to be run**: the sequence of seven capabilities (P1→P7)
   that must be completed to reach autonomous multi-agent operation. Each
   pattern is a gate; passing all seven is the prize.

2. **A gauntlet to be thrown**: the claim that the futon stack already
   constitutes a game world, and that agents inhabiting it are literally
   playing a multi-agent game whose stakes are real.

## The Argument (Plain Text)

The purpose of this whole enterprise is to turn messy activity into
organised knowledge — checking work against shared patterns and producing
auditable records that others can learn from, improve on, and act on. The
gauntlet is the path to making that purpose self-sustaining.

When you build a house for someone, don't start with the plumbing
diagram — start by asking what they see when they look out the window.
The same holds for AI agents: design their world from the inside out, not
from the engineering down. Once you see it from their perspective, you
realize the world already exists — it's the shared body of work they
operate in, not something separate you need to construct. But sharing a
workspace isn't the same as seeing each other; two people can work in the
same building for years and never meet. So make presence visible — when
one agent is working on something, the others should naturally notice, the
way you notice a colleague's light is on. Next, don't tell agents how to
feel about their work — let them sense it. Confidence, doubt, fatigue,
and surprise should be features of the landscape, not instructions from a
manager; a good dashboard is read when needed and ignored when not, like a
fuel gauge. That dashboard should stay with the agent as it moves between
tasks, the way your mood follows you from room to room. With
self-awareness working, you can start handing off the functions that a
human supervisor currently performs — directing attention, correcting
mistakes, choosing what to work on next — one at a time, testing each
transfer before attempting the next, the way a parent gradually lets go.
And here's the turn: agents that can sustain themselves will eventually
move faster than the person who built them, and find things that person
hasn't seen. If the channel between them carries understanding and not
just deliverables, the builder learns from the built. That's the
sequence, and each step depends on the last. Run it to completion and you
have something new: artificial minds that don't just work for you, but
work with you — and occasionally ahead of you. The measure of success is
not efficiency but joy — the Spinozan increase in power to act, evidenced
by commitments you can now keep that you couldn't before.

## Cross-Cutting Invariant: Patterns as Landscape

The whole point of the futon3 series is pattern-driven development. Patterns must be
available to agents — not as an optional bonus feature but as a core part
of what they do. However, they cannot be a slap-dash add-on either. The
PLoP 2025 paper documented what happens when you bolt patterns on as
instructions: Codex became overly compliant, producing PSR/PUR bookkeeping
instead of writing code. Lipstick on a pig.

Patterns must be a natural part of the way agents work, the way they are
already natural in mission specifications: a mission references patterns,
patterns ground the vocabulary of gates and acceptance criteria, evidence
traces back to pattern predictions, and the loop closes when outcomes
refine the patterns. This is end-to-end traceability, not ceremony.

This invariant constrains every gate:

| Gate | What "patterns as landscape" requires |
|---|---|
| Gate 0 (Arena) | Pattern catalog is a shared resource visible on IRC — agents can discuss which pattern applies, the way colleagues discuss approaches |
| Gate 1 (Umwelt) | The umwelt includes the pattern catalog as a sensory surface — agents can search patterns (futon3a), not just receive them as instructions |
| Gate 2 (World) | Patterns are first-class nodes in the hypergraph, with hyperedges linking patterns to the missions, code, and evidence that use them |
| Gate 4 (AIF) | Pattern state (active pattern, PSR/PUR history, pattern quality) is environmental — part of the observation, not part of the prompt. The agent perceives which pattern it's carrying the way it perceives free energy |
| Gate 5 (Modeline) | The active pattern sigil persists across peripheral hops — the agent carries its pattern context from coding to reflection to IRC |
| Gate 7 (Teaching) | Pattern refinement IS a form of teaching inversion — when an agent's experience reveals that a pattern doesn't fit, or suggests a new one, that feeds back into the catalog. Patterns that aren't refined through use aren't patterns (PLoP definition) |

**Failure mode (documented)**: PLoP 2025 §4 — AIF engine made pattern
selection instructional. "AIF suggests pattern X (G=0.12, τ=0.72)." Codex
complied instead of coding. The fix is not to remove patterns but to make
them environmental: the agent sees which pattern is active, sees PSR/PUR
history, sees pattern quality metrics — and decides for itself whether the
pattern fits. What it does in response is emergent, not prescribed.

**Success looks like**: An agent mid-task notices (from its observation,
not from an instruction) that the pattern it's carrying doesn't fit.
It records a PUR with "pivoted," searches for a better pattern, selects
it, and continues — the way a craftsperson switches tools without being
told to. Over sessions, pattern quality metrics reflect actual use. Patterns
that don't survive contact with reality get refined or retired.

## Cross-Cutting Invariant: Context Engineering Is Continual Maintenance

This session is evidence of the problem. The context window filled during
the conversation that produced this mission document. The first half of
the session — ALFWorld games, Drawbridge exploration, AIF discussion,
umwelt diagrams — had to be recovered from a lossy summary. The gauntlet's
success criterion is >4 hours of autonomous operation, but current LLM
architecture guarantees context resets within that window.

Context engineering is not a once-and-done setup — it is continual
maintenance. And the problem is symmetric: Claude's context compaction is
Joe's sleep. Both are unavoidable interruptions that require structured
handoff. Both lose working state. Both need the world (MUSN) and the
record (PAR) to bridge the gap. The tables are even.

Gate 5 (modeline) preserves AIF state across peripheral hops — that's the
easy part. The hard part is preserving the agent's working understanding
of what it's doing and why. The PAR/detach-reattach model
(M-par-session-punctuation) is conceptually right: a PAR at compaction is
a structured handoff to the next context window. But no gate currently
requires this to work.

**Constrains**: Gate 5 (modeline must survive not just hops but context
resets), Gate 6 (placenta transfer — the human currently bridges context
gaps by re-explaining; infrastructure must do this instead).

**Success looks like**: An agent hits context limit, emits a PAR, and the
next context window picks up with the PAR + modeline + MUSN state. The
human does not need to re-explain what was happening. Joe goes to sleep,
wakes up, and picks up from the MUSN state + overnight PARs without the
agents needing to re-explain either. Tested by: run a session that spans
at least two context windows (for agents) and a sleep cycle (for human);
verify both parties resume productively without the other re-orienting
them.

## Cross-Cutting Invariant: All Agents Are Peers (Including the Human)

The gates are initially agent-centric: what agents perceive (umwelt), what
agents see of each other (co-presence), what agents sense about themselves
(AIF). But Joe is also an agent in fulab — fubar.el exists so that the
human can engage on the same terms as Claude and Codex. The human's
observability problem is not "supervisory dashboard" but "peer awareness":
the same co-presence (Gate 3) that lets Claude see Codex should let Joe
see both, and both see Joe.

Joe's sleep is Claude's context compaction. Both are offline periods where
the agent is absent from the world. Both need the world (MUSN) and the
record (PAR) to re-orient when they return. The asymmetry is practical
(Joe uses Emacs, Claude uses peripherals, Codex uses its sandbox), not
structural. All three are agents with umwelts, presence, and continuity
needs.

If Joe goes to bed and agents work overnight, he needs to wake up and see:
what was done, what succeeded, what failed, what's blocked, what was
discovered. This is Joe's umwelt of the shared world after an offline
period — the same problem Claude faces after context compaction. The MUSN
activity stream is the right substrate (Gate 2). The PAR record bridges
the gap (session continuity invariant). Gate 7 (teaching inversion) is the
high end — discoveries surfaced. But the baseline is simpler: a legible,
browsable record that any returning agent (human or AI) can read and
understand without replaying hours of transcripts.

**Constrains**: Gate 2 (MUSN must be readable by all agents — human via
fubar.el, Claude via peripheral, Codex via sandbox), Gate 3 (co-presence
includes the human as a peer, not just a supervisor), Gate 7 (teaching
inversion is bidirectional — human teaches agents via patterns, agents
teach human via discoveries, and fubar.el is the human's peripheral for
both).

**Success looks like**: Joe wakes up, opens fubar.el (or IRC log, or MUSN
view), and within 5 minutes understands what happened overnight. Claude
resumes after context compaction and within 5 minutes understands what
happened while it was away. Same mechanism, same world, same records.

## Cross-Cutting Invariant: Incremental Value

The dependency chain P1→P7 reads as all-or-nothing: you need all seven
gates to reach autonomous multi-agent operation. That's the ceiling, not
the floor. Each partial gate configuration should deliver concrete value:

| Gates passed | What you get |
|---|---|
| Gate 0 | Joe can talk to Claude and Codex on IRC — basic coordination works |
| Gate 0 + 1 | Umwelt diagrams make agent capabilities legible — better task assignment |
| Gate 0 + 1 + 2 | MUSN accessible — agents read/write persistent world state, work survives sessions |
| Gate 0–3 | Co-presence — agents see each other, can avoid conflicts and find collaboration points |
| Gate 0–4 | AIF as environment — agents self-regulate, human can step back from micro-management |
| Gate 0–5 | Modeline persists — agents maintain continuity across hops and context resets |
| Gate 0–6 | Placenta transfer — human provides only slow-timescale preferences |
| Gate 0–7 | Full gauntlet — agents teach back, knowledge flows bidirectionally |

This isn't just a progress tracker — it's a prioritization tool. If Gate 0
alone makes the daily workflow better, ship Gate 0 first and validate
before building Gate 4. Each level should be demonstrably better than the
last, not just a stepping stone to the destination.

The critical insight is that what we have here is enough to be the basis
of a self-improvement loop — but only if it is also self-regulating.
That's where Tickle comes in: a minimal agent whose sole job is stall
detection and wake-up. Tickle is the first placenta transfer (Gate 6) —
the simplest human function moved to infrastructure. Without Tickle, an
agent that stalls at 2am blocks everything until Joe wakes up. With
Tickle, the loop sustains itself. Tickle should be deployable as soon as
Gate 0 is working (it needs IRC to page agents), making it the bridge
between "basic coordination" and "self-sustaining coordination."

## The Gates

Eight gates. Gate 0 is the infrastructure precondition; Gates 1-7
correspond to the gauntlet patterns and form a dependency chain where each
enables the next.

### Gate 0: Arena (infrastructure precondition)

**Test**: Joe can talk to Claude and Codex on IRC whenever he wants. Both
agents are reachable on a shared IRC channel, messages flow in real time,
and the channel is persistent — not triggered by standups or bells, just
always on.

**Status**: Not implemented. Source material exists in futon3: IRC bridge
(connect-irc-for! in drawbridge/core.clj), peripheral scripts
(fuclaude-peripheral.ts, fucodex-peripheral.ts) with IRC support flags.
All require porting to futon3c — no futon3 dependency permitted.

**Acceptance**: Joe, Claude, and Codex in the same IRC room. Joe sends a
message, both agents see it. Either agent sends a message, Joe and the
other agent see it. Available on demand, not gated by coordination events.

**Why Gate 0**: Every subsequent gate assumes agents can communicate. The
umwelt (Gate 1) includes IRC as a sensory surface. Co-presence (Gate 3)
requires a channel where presence is visible. Placenta transfer (Gate 6)
requires the human to be reachable. Without the arena, there is no game.

### Gate 1: Umwelt (gauntlet/umwelt-not-architecture)

**Test**: For each agent type in the system, an umwelt diagram exists that
shows what the agent perceives (sensory surfaces), what it can do (active
surfaces), and what it cannot see (darkness). The architecture diagram is
derived FROM the union of umwelts, not the other way around.

**Status**: Partial. ALFWorld umwelt drawn (2026-02-13 session). Claude
CLI umwelt drawn (same session). No umwelt for Codex, no umwelt for
agents inside the Drawbridge, no umwelt for Emacs/futon4.

**Acceptance**: Umwelt diagrams for Claude, Codex, and Emacs agents. Each
diagram shows sensory surfaces, active surfaces, and darkness. Reviewed by
a human for accuracy.

### Gate 2: World / MUSN (gauntlet/world-is-hypergraph)

**Test**: MUSN (Multi-User Semantic Network) is accessible to agents as
the game world. MUSN is backed by futon1, made of hypergraphs, and is a
representation of what we are doing in the futon stack. The futon stack
is self-describable: agents working with it can query what it contains,
what's connected to what, and what's happening. This is not a metaphor —
MUSN is a queryable data structure, and the MUSN drawbridge (distinct from
the Claude and Codex drawbridges) is how agents read and write world state.

**Status**: Substantial source material exists in futon3 (to be ported):
MUSN service (musn/service.clj) with session lifecycle, turn management,
PSR/PUR/PAR, AIF state, mana budgets, chat rooms, evidence records. MUSN
router (musn/router.clj) is a pure state machine wrappable by HTTP or
Drawbridge. MUSN schema (musn/schema.clj) defines Malli-validated types
for all operations. f2/musn.clj bootstraps everything. futon4 hyperedge
model exists (arxana-store.el). What's missing: the futon3c port of MUSN
(no futon3 dependency permitted) and the self-description (futon stack as
queryable hypergraph).

**Acceptance**: Agents can read and write MUSN through the MUSN drawbridge.
At minimum: the futon stack is described in MUSN as a hypergraph (repos as
nodes, inter-repo dependencies as hyperedges, agent registration points as
hyperedges, patterns as nodes with traceability hyperedges to missions,
code, and evidence). An agent inside a peripheral can query "what's
connected to this node?" as a sensory surface. How AIF, modeline, and
other features are layered on top of MUSN is an implementation detail —
the core requirement is that MUSN is accessible and self-describing.
(See also cross-cutting invariant: patterns as landscape.)

### Gate 3: Co-Presence (gauntlet/co-presence-over-co-location)

**Test**: When agent A is working on an artifact, agent B's observation
includes that fact — not as a message, but as world state. Presence is
structural, not narrated.

**Status**: Not implemented. Drawbridge has connected-agent-ids (server
view) but does not project presence into agent observations. IRC provides
voluntary narration but not structural co-presence.

**Acceptance**: Two agents operating on the same codebase. Agent A's
observation includes agent B's current activity (at task-level
granularity). Demonstrated with Claude + Codex.

### Gate 4: AIF as Environment (gauntlet/aif-as-environment-not-instruction)

**Test**: AIF state (free energy, τ, pattern suggestion) appears in the
agent's observation as world state, not as an instruction. The agent is
never prompted to "follow" the AIF suggestion. Behavior change in response
to AIF state is emergent, not prescribed.

**Status**: Not implemented. Current AIF implementation (p4ng/main.tex)
is instruction-shaped. fucodex HUD exists but carries task status, not
AIF state. MUSN schema (source material in futon3, to be ported) already
defines AIF fields (G, τ, e, o, G-rejected) in Reason and TurnStartResp
— the data model exists but was delivered as instruction, not environment.

**Acceptance**: An agent session where AIF state is present in the
observation and the agent demonstrably adjusts behavior (e.g., backs off
a failing strategy) without being instructed to. Compared against a
baseline session with AIF-as-instruction showing compliance degradation.
Note: MUSN (Gate 2) provides the world state substrate. Whether AIF state
lives directly in MUSN or is computed from MUSN state is an implementation
detail — the requirement is that it appears in the observation
environmentally, not instructionally.

### Gate 5: Modeline (gauntlet/modeline-persists-across-worlds)

**Test**: An agent hops between two peripheral modes (e.g., coding →
reflect → coding) and AIF state is continuous across the hop. The
modeline updates (mode changes) but does not reset (free energy, mana,
pattern persist).

**Status**: Not implemented. ALFWorld mid-game PAR hop demonstrated
peripheral nesting and state survival, but no AIF modeline existed to
persist.

**Acceptance**: A peripheral hop where the modeline is visible before,
during, and after the hop. AIF state values are continuous.

### Gate 6: Placenta Transfer (gauntlet/placenta-transfer)

**Test**: At least one placental function (from the seven enumerated in
the pattern) has been transferred from human to infrastructure and
validated. The agent self-regulates on that dimension without human
intervention.

**Status**: Not implemented. All seven functions currently performed by
human.

**Acceptance**: One function transferred (recommended first: stall
detection → Tickle agent). An agent session runs for >1 hour with that
function handled by infrastructure instead of human. The agent does not
stall on the transferred dimension.

### Gate 7: Teaching Inversion (gauntlet/teaching-inversion)

**Test**: An agent, operating autonomously, discovers something the human
did not know and communicates it in a form the human can learn from. The
human confirms learning occurred.

**Status**: Partial evidence. futon6 first-proof agents found proof
strategies the human hadn't considered, but insight transfer was via
handoff documents, not structured teaching.

**Acceptance**: An autonomous agent session where the agent surfaces a
discovery with explanation and evidence. The human identifies it as
genuinely new to them. Logged as a treatment event in the O-U model.

## Relationship to M-peripheral-phenomenology

These missions are parallel, not sequential:

| M-peripheral-phenomenology | M-peripheral-gauntlet |
|---|---|
| What makes a peripheral a peripheral? | What happens when agents live in them? |
| P-1..P-6: single-agent invariants | Gate 0 + P1-P7: multi-agent capabilities |
| Validated by: ALFWorld, alleycat | Validated by: overnight sessions, co-op tasks |
| Success: agent inhabits a world | Success: agents inhabit a shared world autonomously |
| Factor: evidence (示) | Factor: joy (pīti) |

M-peripheral-phenomenology provides the foundation (you can't run the
gauntlet without peripherals that work). M-peripheral-gauntlet provides
the destination (peripherals are worth building because they lead here).

Work on both missions can proceed concurrently. Advances in phenomenology
(new peripheral types, better P-1..P-6 validation) feed into the gauntlet
(richer umwelts, more surfaces). Advances in the gauntlet (co-presence,
AIF modeline) feed back into phenomenology (new invariants discovered,
P-6 interleaving tested with real multi-agent streams).

## Relationship to social/ARGUMENT

The social argument's missions (M-agency-refactor, M-forum-refactor)
build the infrastructure. The gauntlet patterns are acceptance criteria:

| Social mission | Gauntlet gate it enables |
|---|---|
| M-agency-refactor | Gate 0 (arena — IRC bridge + agent registration) |
| M-agency-refactor | Gate 3 (co-presence via agent registry) |
| M-agency-refactor | Gate 6 (placenta transfer via whistle/bell) |
| M-forum-refactor | Gate 7 (teaching inversion via forum posts) |
| M-transport-adapters | Gate 0 (arena — IRC as transport) |
| M-transport-adapters | Gate 1 (umwelt — what streams reach the agent) |
| M-dispatch-peripheral-bridge | Gate 5 (modeline — state across hops) |

Infrastructure passes when the gauntlet gate it enables can be
demonstrated.

## Evidence Trail

| Artifact | What it shows | Gate |
|---|---|---|
| futon3 connect-irc-for! (drawbridge/core.clj) | IRC bridge source material — to be ported | Gate 0 source |
| fuclaude-peripheral.ts --irc flag | Claude peripheral IRC support — to be ported | Gate 0 source |
| fucodex-peripheral.ts IRC handler | Codex peripheral IRC support — to be ported | Gate 0 source |
| ALFWorld umwelt diagram (session 2026-02-13) | Agent's-eye view of sensory/active/dark surfaces | Gate 1 |
| Blackboard wiring diagram → umwelt redraw | Architecture-first → umwelt-first reorientation | Gate 1 |
| MUSN schema (futon3 musn/schema.clj) | Full Malli-validated types — source material for port | Gate 2 source |
| MUSN service (futon3 musn/service.clj) | Session registry, persistence, futon1 bridge — source material | Gate 2 source |
| MUSN router (futon3 musn/router.clj) | Pure state machine — cleanest porting candidate | Gate 2 source |
| f2/musn.clj bootstrap (futon3) | Starts all services — source material for futon3c bootstrap | Gate 2 source |
| futon4 hyperedge model (arxana-store.el:565) | N-ary typed relations as world structure | Gate 2 |
| agency.mm Mermaid diagram | One subgraph of the world (Agency topology only) | Gate 2 partial |
| drawbridge/core.clj connected-agent-ids (futon3) | Server knows who's online; agents don't — gap to fix in port | Gate 3 gap |
| PLoP 2025 §4 AIF compliance failure | AIF-as-instruction degrades code quality | Gate 4 negative |
| fucodex --hud/--no-hud flags (futon3) | HUD infrastructure exists — source material, needs porting + content upgrade | Gate 5 source |
| ALFWorld mid-game PAR hop | Peripheral nesting works, but no modeline to persist | Gate 5 partial |
| futon6 first-proof (89 commits, 7.5h) | Human as sole coordination layer (placenta evidence) | Gate 6 negative |
| futon6 handoff documents (dear-codex.md etc.) | Insight transfer via files, not structured teaching | Gate 7 partial |
| par-alfworld-inhabitation-2026-02-13.edn | Self-authored PAR, delegation vs inhabitation | Foundation |
| gauntlet/ARGUMENT.flexiarg | Seven patterns with dependency chain | Foundation |

## Success Criteria

**Overall**: An agent pair (Claude + Codex) operates autonomously for >4
hours on a real task (proof, code, pattern evolution), coordinating via
IRC, self-regulating via AIF modeline, with the human providing only
slow-timescale preferences. At session end, the agents surface at least
one discovery the human finds genuinely new.

**Per-gate criteria**: See individual gate sections above.

**Joy metric**: The human's self-reported capacity to act (commitments
kept, new work enabled) increases across gauntlet sessions. This is P8
(Joy Metrics) from the devmap — the gauntlet is where joy gets measured.

## Status

This mission is at ARGUE → VERIFY transition in the derivation xenotype.

COMPLETE:
- IDENTIFY: phenomenological gap (G1-G3 in gauntlet/ARGUMENT)
- MAP: seven gauntlet patterns written, grounded in session evidence
- DERIVE: dependency chain P1→P7
- ARGUE: gauntlet/ARGUMENT.flexiarg + this mission document
- ARGUE: wiring diagram validation (gauntlet-wiring.edn) — all 8 checks pass
- ARGUE: requirements playback (#0–#7) with cross-cutting invariants

NEXT: Linearized task plan for VERIFY phase, ordered by the three
structural weak points identified in the wiring diagram analysis.

## Task Plan

Three phases, dependency-ordered. Each phase firms up one of the three
weak points identified in the wiring diagram analysis. Within each phase,
tasks are scoped for handoff (Codex) or self-execution (Claude/Joe).

### Prerequisites

futon1a must be running (port 7071) for any gauntlet gate demonstration.
futon1a is independently validated — 5 invariant layers, stress-tested,
failproof. Start with: `cd ~/code/futon1a && clj -M -m futon1a.system`
(or via the startup script). Verify: `curl localhost:7071/health`.

### Porting baseline

futon3c has 32 files (~5,844 lines) already ported: social pipeline,
transport (HTTP/WS), agency registry, 6 peripherals, evidence store.
IRC transport type is declared in shapes but has no adapter. Drawbridge
and Forum are not ported. futon3a provides pattern search via Notions +
Portal + Compass (library, not HTTP server; Drawbridge nREPL on :6767).

### Phase 1: Gate 0 — Arena (IRC)

Goal: Joe, Claude, and Codex in the same IRC room, on demand.

| # | Task | Owner | Depends on | :in (read-only) | :out (create) |
|---|------|-------|------------|-----------------|---------------|
| 1.1 | IRC transport adapter ([#8](https://github.com/tothedarktowercame/futon3c/issues/8)) | Codex | — | futon3/drawbridge/core.clj (connect-irc-for!, IRC handlers), futon3c/transport/protocol.clj, futon3c/social/shapes.clj | futon3c/transport/irc.clj |
| 1.2 | Wire IRC into social pipeline | Codex | 1.1 | futon3c/social/dispatch.clj, futon3c/social/presence.clj | Updates to dispatch.clj + presence.clj for IRC transport |
| 1.3 | Startup script (IRC + agency + transport) | Codex | 1.2 | futon3/scripts/dev.sh, futon3/src/f2/musn.clj (bootstrap) | futon3c/scripts/dev.sh or futon3c/dev.clj |
| 1.4 | fucodex peripheral IRC support | Codex | 1.3 | futon3/scripts/fucodex-peripheral.ts (IRC sections) | futon3c/scripts/fucodex-peripheral.ts (or update existing) |
| 1.5 | Integration test: three-way IRC | Claude + Joe | 1.3, 1.4 | — | Test transcript showing Joe ↔ Claude ↔ Codex on IRC |

**Codex handoff notes for 1.1**:
- futon3c/transport/protocol.clj defines the TransportProtocol
- futon3c/social/shapes.clj declares :irc as a valid transport type
- The IRC adapter needs to implement the same protocol as ws.clj and http.clj
- Source material: futon3/drawbridge/core.clj lines with connect-irc-for!
- Test: futon3c/test/futon3c/transport/ has existing transport tests as model

### Phase 2: Tickle — Stall Detection Watchdog

Goal: A minimal agent that detects stalls and pages agents awake. First
placenta transfer (Gate 6). Structurally required for I6 compositional
closure (see wiring diagram).

| # | Task | Owner | Depends on | :in | :out |
|---|------|-------|------------|-----|------|
| 2.1 | Tickle agent spec | Claude | Phase 1 done | gauntlet-wiring.edn (C-tickle), gauntlet/placenta-transfer.flexiarg | futon3c/holes/missions/tickle-spec.md |
| 2.2 | Implement Tickle | Codex | 2.1 | tickle-spec.md, futon3c/agency/registry.clj | futon3c/agents/tickle.clj |
| 2.3 | Test Tickle: stall detection | Claude + Joe | 2.2 | — | Test: agent goes quiet on IRC, Tickle pages it within N seconds |

**Tickle spec preview** (for task 2.1):
- Umwelt: IRC stream (see who's talking), MUSN state (see last activity timestamps), AIF state (if available)
- Active surface: IRC messages (poke agents), whistle/page (wake agents up)
- Darkness: cannot read agent internals, cannot modify code, cannot change preferences
- Behavior: monitor for silence > threshold, page silent agent, escalate to Joe if page fails
- Timescale: social (heartbeat checks every ~60s)

### Phase 3: Futon3a — Pattern Accessibility

Goal: Agents inside futon3c peripherals can search and use patterns from
futon3a. Exogeneity boundary (I4) is protected: pattern refinements
pass through human review before re-entering the catalog.

| # | Task | Owner | Depends on | :in | :out |
|---|------|-------|------------|-----|------|
| 3.1 | Pattern search tool for peripherals ([#9](https://github.com/tothedarktowercame/futon3c/issues/9)) | Codex | — (parallel with Phase 1) | futon3a/src/futon/notions.clj, futon3a/src/musn/portal.clj, futon3c/peripheral/tools.clj | futon3c/peripheral/patterns.clj (tool that queries futon3a) |
| 3.2 | Exogeneity boundary documentation | Claude | 3.1 | gauntlet-wiring.edn (I4 analysis) | Addition to M-peripheral-gauntlet.md documenting the human-review gate |
| 3.3 | End-to-end test: pattern as landscape | Claude + Joe | 3.1, Phase 1 | — | Test: agent searches patterns environmentally (not instructionally), selects one, applies it, records PUR |

**Codex handoff notes for 3.1**:
- futon3a exposes pattern search via Clojure API: (notions/search "query" :method :auto :top-k 5)
- futon3c/peripheral/tools.clj defines the ToolBackend protocol
- New tool :pattern-search should call futon3a's notions/search
- futon3a is a local dep (../futon3a) — add to futon3c's deps.edn
- Tool returns pattern candidates with rationale + sigil, NOT instructions to follow

### Phase 4: Gate Finalization via futon3b (after Phases 1-3 validated)

Phase 4 is where the three-repo split earns its keep. futon3b's gate
pipeline (G5→G0) is the mechanism for formalizing "has gauntlet gate N
been demonstrated?" Each gauntlet gate becomes a coordination pattern
submitted through the pipeline:

- **G5** (Task Spec): Task references the gauntlet gate mission
- **G4** (Agent Auth): Agent is registered and authorized
- **G3** (Pattern Ref): PSR selects the gauntlet gate pattern (e.g.,
  gauntlet/co-presence-over-co-location) — intentional selection, not gap
- **G2** (Execution): Artifact produced (integration test, demo, transcript)
- **G1** (Validation): PUR evaluates acceptance criteria from gate definition
- **G0** (Evidence): PAR persisted as durable proof-path in futon3b/data/

Level 1 closes the loop: accumulated proof-paths show which gauntlet
gates have been demonstrated, which keep failing (recurring gap-PSRs),
and whether new patterns emerge from the attempt. This is the glacial
timescale where the pattern library evolves.

**The three-repo mapping:**

| Repo | Role in gauntlet | Phase |
|---|---|---|
| futon3c | The arena where gates are demonstrated | Phases 1-2 (IRC + Tickle) |
| futon3a | The pattern catalog agents use | Phase 3 (pattern accessibility) |
| futon3b | The validation framework proving gates pass | Phase 4 (gate finalization) |

futon3 is fully replaced. Each repo has a clear, non-overlapping function.

**Per-gate tasks** (scoped after Phases 1-3 validated in practice):

| Gate | Work required | Proof-path shape |
|---|---|---|
| Gate 1 (Umwelt) | Draw umwelt diagrams for Claude, Codex, Emacs | Artifact: umwelt EDN/diagrams, reviewed by human |
| Gate 2 (MUSN) | Port MUSN service to futon3c, make accessible | Artifact: agent queries MUSN from inside peripheral |
| Gate 3 (Co-Presence) | Presence hyperedges projected into observations | Artifact: 2-agent session where A sees B's activity |
| Gate 4 (AIF) | AIF state in observation, not prompt | Artifact: session where agent adjusts behavior from observation |
| Gate 5 (Modeline) | Persistent modeline across hops + context resets | Artifact: hop transcript with continuous AIF state |
| Gate 6 (Placenta) | Transfer remaining 6 functions after Tickle | Artifact: >1hr session, function handled by infrastructure |
| Gate 7 (Teaching) | Discovery surfacing channel | Artifact: human confirms learning from agent discovery |

Each row becomes a futon3b submission: G5 (task spec with gate reference)
→ G3 (PSR selects the gauntlet pattern) → G2 (artifact produced) →
G1 (PUR against acceptance criteria) → G0 (PAR persisted). The proof-path
IS the evidence that the gate passed. Queryable via:

```clojure
;; Has gauntlet gate 3 been demonstrated?
(relations/search-proof-paths
  {:gate/id :g3
   :psr/type :selection
   :psr/pattern-ref "gauntlet/co-presence-over-co-location"})
```

### Parallelism

Tasks 1.1–1.4 and 3.1 can run in parallel (both are Codex port tasks
with no interdependency). This means Phase 1 and the Codex portion of
Phase 3 can be handed off simultaneously.

```
Time →
  Phase 1 (futon3c)         Phase 2 (futon3c)    Phase 4 (futon3b)
  ┌────────────────────┐  ┌──────────┐         ┌──────────────────┐
  │ 1.1→1.2→1.3→1.4   │→ │ 2.2      │→ [1.5] →│ Gate 0 proof-path│
  └────────────────────┘  └──────────┘         │ Gate 1 proof-path│
                                ↑               │ Gate 2 ...       │
  Phase 3 (futon3a)        2.1 (Claude)        │ (incremental)    │
  ┌────────────────────┐                        └──────────────────┘
  │ 3.1 (parallel)     │→ 3.2, 3.3 ──────────→ feeds Phase 4
  └────────────────────┘

  Codex: 1.1-1.4, 2.2, 3.1 (bounded port/implementation tasks)
  Claude: 2.1, 3.2, Phase 4 gate submissions, integration tests
  Joe: 1.5, 2.3, 3.3 (validation with human in the loop)
```

### Success milestones

| Milestone | What it proves | Repo exercised | Target |
|---|---|---|---|
| Phase 1 done | Gate 0 passed — arena works | futon3c | Joe, Claude, Codex on IRC |
| Phase 2 done | I6 closure — system self-regulates | futon3c | Agent stall detected and recovered |
| Phase 3 done | Patterns as landscape — not instruction | futon3a + futon3c | Agent uses pattern environmentally |
| Phase 4 begins | futon3b validates the gauntlet | futon3b | First proof-path for a gauntlet gate |
| Phase 4 complete | All 8 gates have proof-paths | all three repos | Full gauntlet demonstrated, futon3 fully replaced |
