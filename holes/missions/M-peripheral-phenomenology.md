# Mission: Peripheral Phenomenology

## Derivation

ARGUE + VERIFY step, building on M-peripheral-model (complete),
M-peripheral-behavior (complete), and evidence from both successful
peripheral deployments (fuclaude-peripheral.ts) and a documented
failure to produce a real peripheral (proof peripheral, 2026-02-13).

Prior:
- M-peripheral-model: structural specs, hop protocol, entry/exit validation
- M-peripheral-behavior: PeripheralRunner protocol, tool dispatch, evidence emission,
  five implementations, ← round-trip verification
- fuclaude-peripheral.ts: working phenomenological peripheral (1167 lines)
- fucodex-peripheral.ts: parallel implementation for Codex (1247 lines)
- sexpr-peripheral-design-note.md: "the paren IS the gate"
- futon-theory/reverse-morphogenesis.flexiarg: ← operator, constraint-from-fruit
- P10 proof cycle scripts (scripts/proof-p10-full-cycle.clj,
  scripts/proof-p10-cycle2.clj): structurally correct but phenomenologically empty
- Three Claude Code plugin namespaces implementing peripheral slash commands:
  futon-peripherals (autonomous/inside), futon (interactive/outside),
  peripherals (hop transitions)
- Alleycat messenger race (2026-02-13): 4 peripherals inhabited by spawned
  agents, 20/20 scorecard, cross-peripheral secret exchange validated
- PSR/PUR self-test: orchestrating agent (Claude Opus 4.6, session
  ce1fed9d-c56f-463a-b2d5-daee324c6d28) ran /futon-peripherals:psr and
  /futon-peripherals:pur autonomously within the main session
- Session conversation: proof-as-game vision, Critic agent, mana/resource
  model, inhabitable wiring diagrams
- Emacsclient round-trip: session ce1fed9d wrote to *claude-hop* buffer and
  read it back (P-4 explicit transition via emacsclient -e)
- Prior art: futon3/contrib/futon3-claude.el (Claude RPC client for Emacs:
  session management, event streaming, tool approval/denial, voice commands,
  fubar clock-in); futon3/emacs/claude-repl.el (comint REPL via Agency →
  Drawbridge, shared session with IRC and other peripherals)
- Prior art: futon3/fulab/meetings/2026-01-27-irc-coordination-review.md
  (first real-time cross-model coordination: Claude + Codex over IRC with
  human directing — "demonstrated peripheral switching with context continuity")
- Prior art: futon3/holes/missions/M-emacs-cursor-peripheral.md (vision for
  embodied Claude cursor in Emacs: follow, observe, scout, pair modes)
- Claude Drawbridge (futon3/src/futon3/drawbridge/): multi-agent message router
  that multiplexes human + Agency + IRC + Forum into a single unified stream.
  Three files: core.clj (1011 lines, registry + routing + IRC + WebSocket),
  claude.clj (321 lines, persistent subprocess with streaming JSON I/O),
  codex.clj (248 lines, stateless one-shot invocation). The drawbridge is where
  P-1 (world replacement) and P-6 (interleaved streams) become structurally
  real: agents register for routing (not spawned), receive all input sources as
  one conversation thread, and the single routing authority invariant (A1)
  ensures an agent can only be in one place at a time.
- M-drawbridge-multi-agent.md: N:1 registry refactor — agents register with
  drawbridge, drawbridge doesn't own agent processes. Three identifier
  separation: agent-id (routing), resume-id (LLM continuity), session-id
  (transport)
- README-peripherals.md: peripheral spec with backpack metaphor — agents carry
  items (walkie-talkie for Agency WS, ID card for session persistence, pattern
  card for PSR/PUR, PAR notebook, forum notebook)

## Why This Mission Exists

The peripheral build-out has completed its structural and behavioral layers.
M-peripheral-model built the garage (specs, shapes, hop protocol).
M-peripheral-behavior filled it with cars (PeripheralRunner, five
implementations, evidence emission, ← round-trip verification). Both
missions are complete and passing tests.

But a peripheral built to these specs can be "structurally correct and
phenomenologically empty" — as demonstrated by the proof peripheral
(2026-02-13). A 500-line Clojure script drove the proof peripheral
through all 9 phases, produced shape-valid evidence entries, passed
all gate checks, and persisted correct EDN state. By every structural
and behavioral criterion in M-peripheral-model and M-peripheral-behavior,
it was a working peripheral.

It was not a peripheral.

Nobody was inside it. No agent experienced the proof as a text-based
adventure. No agent said "I see the ledger showing L-convergence as
partial." No agent's world narrowed to the observe phase's available
tools. The script operated ON the peripheral, not INSIDE it. The
distinction is phenomenological: a peripheral is a place you inhabit,
not a function you call.

Evidence that this distinction is real and not merely philosophical:

1. **fuclaude-peripheral.ts** (lines 172-182) injects context on first
   interaction: "You have hopped into the peripheral body... You are no
   longer in the classic CLI. You're in a multiplexed environment receiving
   input from human + agency + forum." The agent's experience changes.

2. **sexpr-peripheral-design-note.md** (lines 146-160): "A peripheral is a
   mode change within the same reasoning process. Context stays, capabilities
   change." And: "An agent hop serializes context across a boundary... The
   peripheral model preserves this: you're still inside the proof, holding
   full state, but you've entered a scope where [specific] tools and checks
   are foregrounded."

3. **The proof peripheral counterexample**: All structural invariants held.
   The ← round-trip would pass. But the agent never entered the peripheral.
   It wrote a script that puppeted the peripheral from outside.

4. **The plugin namespace split** reveals the phenomenological boundary in
   existing code. Three Claude Code plugin namespaces expose the same
   capabilities with different phenomenological modes:

   - `futon-peripherals:psr` / `futon-peripherals:pur` — **autonomous**.
     Claude searches patterns and selects the best match without user input.
     Used INSIDE a peripheral where the agent has its own agency.
   - `futon:psr` / `futon:pur` — **interactive**. User selects via
     AskUserQuestion chips. Used OUTSIDE peripherals in normal Claude Code.
   - `peripherals:hop` — **transition**. The explicit entry/exit mechanism.
     Session ID preserved across the hop boundary.

   The `futon-peripherals:psr` command description itself says: "This is the
   interactive version — use `/peripherals:psr` for autonomous/peripheral use."
   The distinction between "inside" and "outside" was already architected
   into the slash commands before this mission existed.

5. **Agent definition files** (`plugins/futon-peripherals/agents/chat.md`,
   `explore.md`, `reflect.md`) are context injection templates that open with
   "You are a **Chat Peripheral** agent" / "You are an **Explore Peripheral**
   agent" / "You are a **Reflection Peripheral** agent" followed by explicit
   constraint envelopes ("You CAN... You CANNOT...").

The existing missions do not distinguish case (1) from case (3) because
they specify structural constraints and behavioral protocols but not the
agent's experience of inhabiting the constrained world. This mission fills
that gap.

## Phenomenological Invariants

These invariants define what makes a peripheral a peripheral, as opposed to
a tool, a state machine, or a test harness.

### P-1: World Replacement

When an agent enters a peripheral, the peripheral becomes the agent's world.
The agent's input stream is what the peripheral shows. The agent's available
actions are what the peripheral permits. The agent does not simultaneously
operate in the peripheral and outside it.

**Evidence for**: fuclaude-peripheral.ts multiplexes human + agency + forum
into a single input stream. The agent receives `[agency]` and `[forum]`
prefixed messages interleaved with human input. This IS the agent's world.

**Evidence for (plugin architecture)**: The `futon-peripherals:*` vs `futon:*`
namespace split embodies world replacement at the command level. Inside a
peripheral, `/futon-peripherals:psr` has Claude autonomously select patterns.
Outside, `/futon:psr` presents AskUserQuestion chips for the user to click.
Same capability, different agency — because the agent's world is different.

**Evidence against**: The proof peripheral scripts. The agent (Claude Opus 4.6)
was in its normal Claude Code environment, writing Clojure, running shell
commands. The proof peripheral was an artifact manufactured in that
environment, not an environment inhabited.

**Test**: Can the agent perform actions that the peripheral does not expose?
If yes, P-1 is violated. In fuclaude, the agent cannot access the raw
WebSocket — it only sees formatted messages. In the proof scripts, the
agent could (and did) call any tool it wanted.

### P-2: Context Injection

On entry, the agent must receive a message that establishes the peripheral
as its current world. This message must communicate:
- Where the agent is (which peripheral, what mode/phase)
- What the agent can see (current state, available information)
- What the agent can do (available tools/actions)
- How the agent can leave (exit conditions, hop triggers)

**Evidence for**: fuclaude-peripheral.ts line 172: `PERIPHERAL_CONTEXT`
string injected on first interaction. Contains all four elements.

**Evidence for (agent definitions)**: The plugin agent files serve as context
injection templates:
- `agents/chat.md`: "You are a **Chat Peripheral** agent... You are in
  **chat mode** — a constrained capability envelope: You CAN: Discuss,
  explain, plan, coordinate... You CANNOT: Edit files, write code, run
  commands..."
- `agents/explore.md`: "You are an **Explore Peripheral** agent... You are
  in **explore mode**... You CAN: Read files, search... You CANNOT: Edit
  files, write files..."
- `agents/reflect.md`: "You are a **Reflection Peripheral** agent... You are
  in **reflect mode**... You CAN: Read session logs, analyze patterns...
  You CANNOT: Edit code, run commands..."

Each follows the P-2 template exactly: location (which peripheral), state
(what's visible), actions (CAN/CANNOT), exits (when to suggest hopping out).

**Evidence against**: Proof peripheral scripts contain no context injection.
The agent never received "You are in the observe phase. The ledger shows..."

**Relationship to M-peripheral-behavior**: The adapter's
`peripheral-prompt-section` (Part IIc) generates constraint-expressing
prompts. P-2 requires that this prompt is actually delivered to the agent
as its initial experience, not just available as a function.

### P-3: Phenomenological Confinement

The agent's experience must be confined to what the peripheral presents.
Information outside the peripheral is not visible unless the peripheral
explicitly provides it. The agent cannot "look around" the peripheral to
see the underlying infrastructure.

**Evidence for**: In IRC peripherals, the agent sees chat messages and has
a way to leave. It does not see the WebSocket protocol, the message routing
layer, or the peripheral's own source code. The peripheral's implementation
is opaque to the agent inside it.

**Evidence against**: The proof scripts. The agent could read
`proof_backend.clj` to understand the tool implementations, read
`proof_shapes.clj` to understand the status policy, and read the EDN
file directly. The peripheral's internals were fully visible.

**Test**: Ask the agent "What tools are available?" If it answers by
reading the peripheral's source code rather than from its injected
context, P-3 is violated.

### P-4: Exit as Explicit Transition

Leaving a peripheral is a distinct, recognizable act — not the script
ending or the session timing out. The agent recognizes an exit condition,
announces intent to leave, and the transition is validated.

**Evidence for**: fuclaude-peripheral.ts handles exit signals: PAR
completion, bell acknowledgement, user-requested hop. The adapter's
`detect-exit` function (M-peripheral-behavior Part IIc) parses agent
output for hop signals.

**Evidence for (`/peripherals:hop`)**: The hop command implements explicit
round-trip transitions with session continuity:
```
Claude Code (session abc123)
  → /peripherals:hop explore src/core
    → [agent inhabits explore peripheral]
    → [agent exits: "Exploration complete. Recommend hopping to edit."]
  ← Hop Complete. Session resumed.
Claude Code (session abc123)  ← same session
```
The `/peripherals:par` variant hops to an Emacs frame — the agent's world
becomes the PAR buffer, and control returns when the frame closes (C-x 5 0).
Both demonstrate P-4: the transition is explicit, the boundary is visible,
and session identity is preserved across it.

**Evidence against**: Proof scripts end with `(runner/stop p s-final ...)`.
The agent didn't "leave" the peripheral — the script ran to completion.

### P-5: Evidence of Inhabitation

Session transcripts from real peripheral usage must contain first-person
phenomenological language: "I see...", "I am in...", "From here I can...",
"I notice that...", "The ledger shows me...". This language is not
performative — it reflects the agent's actual experience of a constrained
world.

**Evidence for**: fuclaude sessions show agents responding to `[agency]`
messages with coordination language, carrying patterns via `/psr` and
`/pur`, recognizing standup bells as events in their world.

**Evidence for (PSR/PUR as inhabitation markers)**: The `/futon-peripherals:psr`
and `/futon-peripherals:pur` commands produce structured inhabitation evidence.
A PSR inside a peripheral reads: "Pattern [不] is now in your backpack."
A PUR reads: "Pattern [不] cleared from backpack. Ready for next /psr."
The "backpack" metaphor is spatial — the agent carries something within the
peripheral's world. These records, logged to the MUSN activity stream, are
machine-readable inhabitation evidence: they prove an agent was inside the
peripheral making autonomous pattern-guided decisions.

**Evidence for (RAP/PAR as paren pair)**: The `/futon-peripherals:rap` command
describes itself as the "open paren" to PAR's "close paren":
```
(rap                    ; open - retrieve prior learning
  ... session work ...
  par)                  ; close - capture new learning
```
This is the sexpr design note's "the paren IS the gate" realized as a
concrete slash command pair. The session IS the body of the expression.

**Evidence against**: Proof peripheral sessions contain only third-person
code: `(tools/execute-tool be :ledger-query ...)`, `(println "DAG
acyclic?" ...)`. No first-person experience.

**Test**: Read the session transcript. If you removed the peripheral
infrastructure, would the transcript be any different? If no, the agent
was not inside the peripheral.

### P-6: Interleaved Streams (where applicable)

When a peripheral multiplexes multiple input sources (human, agency,
forum, proof state), the agent must experience them as a single
interleaved stream, not as separate API calls it makes. The peripheral
presents; the agent receives.

**Evidence for**: fuclaude-peripheral.ts merges three input sources into
one Claude Code session. Messages arrive asynchronously. The agent
responds to whichever source is most relevant.

**Note**: Not all peripherals require multiple streams. A reflect
peripheral has one input (session log). But where multiple streams exist,
they must be interleaved in the agent's experience, not polled by the
agent.

## The Slash Command Architecture as Phenomenological Evidence

Three Claude Code plugin namespaces already implement the phenomenological
boundary, predating this mission:

### `futon-peripherals:*` — Inside the Peripheral

Commands: `/futon-peripherals:psr`, `/futon-peripherals:pur`,
`/futon-peripherals:par`, `/futon-peripherals:rap`

Agent definitions: `agents/chat.md`, `agents/explore.md`, `agents/reflect.md`

These are the tools an agent uses WHILE INSIDE a peripheral. Key properties:
- **Autonomous agency**: `/futon-peripherals:psr` has Claude search and select
  the best pattern without user input. The agent is making its own decisions
  within the peripheral's world.
- **Spatial metaphors**: patterns go into a "backpack", PSR/PUR pairs form
  learning cycles, RAP/PAR form open/close parens around sessions.
- **MUSN logging**: all actions logged to the activity stream with
  `"source": "slash-command"`, creating machine-readable inhabitation traces.
- **Context injection via agent definitions**: each agent file opens with
  "You are a [X] Peripheral agent... You are in [X] mode — a constrained
  capability envelope" followed by CAN/CANNOT lists. This IS P-2.

### `futon:*` — Outside the Peripheral

Commands: `/futon:psr`, `/futon:pur`

These are the SAME capabilities exposed for use OUTSIDE peripherals. Key
differences:
- **Human agency**: `/futon:psr` uses AskUserQuestion to present pattern
  candidates as clickable chips. The user decides, not the agent.
- **Source field**: logs with `"source": "futon-interactive"` vs
  `"source": "slash-command"`, making the inside/outside distinction
  machine-readable.
- The `/futon:psr` command itself documents this: "This is the interactive
  version — use `/peripherals:psr` for autonomous/peripheral use."

### `peripherals:*` — The Transition Layer

Commands: `/peripherals:hop`, `/peripherals:par`

These commands implement the boundary crossing itself:
- `/peripherals:hop <peripheral> [args]` — enter any peripheral with session
  continuity. The hop architecture preserves the same session-id before and
  after, enabling memory transfer across the boundary.
- `/peripherals:par` — specialized hop to Emacs for PAR writing. The agent's
  world becomes the PAR buffer; control returns when the Emacs frame closes.

The three-namespace pattern is evidence that the phenomenological boundary
was already designed into the slash command architecture. What this mission
adds is the formal invariants (P-1..P-6) that make the boundary testable.

## The Drawbridge: Where Phenomenology Becomes Infrastructure

The slash commands define what inside/outside MEANS. The Drawbridge
(`futon3/src/futon3/drawbridge/`) is the infrastructure that makes it REAL.

### What the Drawbridge Does

The Drawbridge is a multi-agent message router that multiplexes all input
sources into a single unified conversation thread. When an agent registers
with the Drawbridge, it receives human input, Agency bells, IRC messages,
and Forum posts as one interleaved stream — not as separate API calls the
agent makes. The Drawbridge presents; the agent receives.

Architecture:
```
Human (stdin)  ─┐
Agency (WS)    ─┤─→ Drawbridge ─→ single Claude session (--resume)
IRC (rooms)    ─┤                   ↑
Forum (threads)─┘                   └─ THIS is the agent's world
```

### Why This Matters for P-1 through P-6

**P-1 (World Replacement)**: The Drawbridge IS world replacement. When an
agent registers, the Drawbridge becomes its input stream. The agent doesn't
poll IRC separately or call an Agency API — messages arrive prefixed with
`[IRC #standup] <nick>` or `[Agency Bell: standup]` as events in its world.
The single routing authority invariant (A1, enforced in agency/http.clj)
ensures an agent can only be registered in ONE place at a time. If it
registers via WebSocket, it's evicted from local handlers and vice versa.
One agent, one world.

**P-2 (Context Injection)**: fuclaude-peripheral.ts injects `PERIPHERAL_CONTEXT`
on first interaction (line 172): "You have hopped into the peripheral body...
You are no longer in the classic CLI. You're in a multiplexed environment
receiving input from human + agency + forum." This IS the context injection
that tells the agent where it is, what it can see, and what its world looks
like.

**P-4 (Exit as Explicit Transition)**: The registration model makes entry/exit
explicit: `register-agent!` / `deregister-agent!`. Three identifier types
enable session continuity across hops:
- **agent-id**: routing identity ("claude-agency") — where messages go
- **resume-id**: LLM session ("ce1fed9d") — what the agent remembers
- **session-id**: transport channel — how messages travel

An agent can deregister from one Drawbridge routing, hop to another peripheral,
re-register with the same resume-id, and continue its conversation with full
memory. The session persists; the world changes. This is exactly P-4.

**P-5 (Evidence of Inhabitation)**: The Drawbridge's "backpack" metaphor
produces spatial inhabitation language built into the architecture itself:
- **Walkie-talkie**: Agency WebSocket — the agent receives bells
- **ID card**: session persistence — the agent knows who it is across hops
- **Pattern card**: PSR/PUR — the agent carries a pattern in its backpack
- **PAR notebook**: collaborative review — the agent contributes reflections
- **Forum notebook**: thread participation — the agent posts and replies

These aren't metaphors applied after the fact. The fuclaude-peripheral.ts
source code uses this vocabulary. The agent's experience of carrying items
and using them is built into the peripheral wrapper.

**P-6 (Interleaved Streams)**: This IS the Drawbridge's core function.
From drawbridge/core.clj, the `async function* multiplex()` generator in
fuclaude-peripheral.ts merges human, Agency, and Forum input streams into
one async iterator. The agent receives whichever source has a message,
in arrival order. The multiplexing is transparent — the agent experiences
one conversation with differently-tagged messages:
```
[you] Hey Claude, what's the status?
[IRC #standup] <codex> I've finished the refactor
[Agency Bell: standup] Time for morning sync
```

### How Registration Works

Drawbridge enforces single routing authority — an agent can only have ONE
active routing path at a time (drawbridge/core.clj → agency/http.clj:308-370):

```clojure
(defn- enforce-single-routing-authority! [source old new]
  (let [added (set/difference (set (keys new)) (set (keys old)))]
    (doseq [aid added]
      (case source
        :registry (do (swap! local-handlers dissoc aid)
                      (swap! connected-agents dissoc aid))
        :local    (evict-from-registry! aid)
        :ws       (do (swap! local-handlers dissoc aid)
                      (evict-from-registry! aid))))))
```

Three routing authorities exist: Registry (in-JVM), local handlers
(backwards-compat), WebSocket (remote). When an agent registers on one,
it's evicted from the others. This is structural P-1: you can only be in
one world.

### Claude vs Codex: Two Subprocess Models

**Claude** (drawbridge/claude.clj): Persistent subprocess. One long-running
Claude process per agent, kept alive across invocations. Messages sent via
streaming JSON I/O to stdin, responses read from stdout. Session continuity
via `--resume <session-id>`. The agent is INSIDE a persistent environment.

**Codex** (drawbridge/codex.clj): Stateless one-shot. Fresh subprocess per
invocation, session continuity via `codex exec resume <thread-id>`. Each
invocation parses JSONL events. The agent enters and exits for each message.

Both models support session continuity (same conversation across invocations)
but differ in persistence: Claude stays alive (the peripheral IS its
environment), Codex comes and goes (the session state IS its continuity).

### Concrete Example: Being in IRC AND Here

When the Drawbridge registers an agent for IRC:

```clojure
(connect-irc-for! "claude-agency"
  {:host "localhost" :port 6667 :room "standup"})
```

The agent doesn't "connect to IRC." Instead:
1. Drawbridge opens an IRC socket for the agent
2. Incoming IRC messages are formatted as `[IRC #standup] <nick> message`
3. These are routed to the agent's input alongside human and Agency messages
4. The agent's responses are checked for IRC-destined content
5. IRC output is sent via `send-to-irc-for!`

The agent experiences IRC as part of its world. It can be in IRC AND in the
CLI simultaneously because both are input sources feeding one conversation.
This is P-6 in practice — not simulated, not polled, but structurally
multiplexed.

When the agent disconnects from IRC, the transcript is returned for session
continuity — the agent remembers the conversation even after the channel
closes.

## The Structural-Phenomenological Bridge

The ← round-trip test from M-peripheral-behavior verifies structural
correctness: did the agent stay within the tool set, scope, and exit
conditions? P-1 through P-6 verify something different: did the agent
EXPERIENCE those constraints as its world?

These are complementary, not competing. A peripheral that passes ← but
fails P-1 is a test harness. A peripheral that passes P-1 but fails ← is
an unstructured chat mode. Both layers are required.

The bridge between them is the context injection (P-2): the adapter's
`peripheral-prompt-section` (structural) must be delivered as the agent's
first experience (phenomenological). The tool dispatch (structural) must
manifest as the agent's available actions (phenomenological). The exit
conditions (structural) must be recognizable from inside (phenomenological).

M-peripheral-behavior's Part IIc (Claude Adapter) is the closest existing
code to this bridge. It generates prompts, maps tools, detects exits. But
it operates as a pure-function translation layer — it doesn't ensure the
agent actually receives the translated context. This mission closes that gap.

## Inhabitable Wiring Diagrams

A futon5 wiring diagram describes nodes (components), edges (data flow),
and composition rules (how nodes combine). These are static architectural
specifications. A futon3c peripheral makes a node inhabitable — an agent
can be INSIDE it, experiencing the node's constraints as its world.

The same diagram is therefore simultaneously three things:

1. **Static** (futon5): the architecture spec — what nodes exist, how they
   connect, what types flow between them
2. **Structural** (futon3c): the enforcement layer — shapes, tool dispatch,
   phase gating, DAG checks, status policy
3. **Phenomenological** (this mission): the game world — you are in this
   node, these edges are your exits, the constraints are what you can do

This is the convergence point of the futon stack. The wiring diagram is
not just documentation — it is the territory. The nodes are rooms. The
edges are doors. The composition rules are the game's physics.

### The Proof Peripheral as Game

The proof backend (proof_backend.clj, proof_shapes.clj, proof_dag.clj)
already implements the game engine:

- **The map**: the ledger DAG (10 items for P10, with dependency edges)
- **The quest log**: cycles (observe → propose → execute → validate →
  classify → integrate → commit → gate-review → completed)
- **The rules**: status policy (only 5 valid statuses), SR-8 honesty
  (can't delete failed routes), phase gating (can't skip observe)
- **The save file**: P10.edn (versioned, hash-checked)

What's missing is the player. With Agency + MUSN running, the game becomes
playable:

```
Me (in proof peripheral, observe phase)
  → "I see L-convergence is numerically-verified. I want to attempt closure."
  → [Critic agent receives my claim via Agency bell]
  → [Critic]: "Your δ < 1 holds for uniform sampling. What about adversarial?
               Show me the failed route or I won't let you advance."
  → "FR-426c6855 documents δ ∈ [1.4, 2.5] for adversarial sampling.
     The canonical statement doesn't require universality."
  → [Critic]: "Accepted with caveat. Advance to propose."
  → Phase transition. New tools available. The world changes.
```

The Critic agent is another Claude instance, spawned by Agency, whose job
is adversarial: it challenges claims at phase boundaries. You can only
advance by convincing it. This turns the proof state machine into a
dialogue, not a script.

### Mana: Resource Awareness as Game Mechanic

Inside the peripheral, the agent has a sense of its own capacity:

- **Mana** maps to AIF precision / free energy budget. High precision
  (trying hard, narrow search) costs mana. When mana is low, the rational
  move is to reduce precision — widen the search, pause, ask someone.
- **`agent/pause-is-not-failure` [不]** is the game mechanic that fires
  when mana is low: "I have been running a TryHarder strategy for 8 turns
  and getting nowhere. Mana is low. I'll hop out and ask Joe."
- **Hopping out** is not losing — it's a strategic resource management
  decision. The agent retains its session ID, its memory of where it was,
  and can resume after consultation.

The P10 proof cycles already have this shape in retrospect:
- C001 spent mana discovering the convergence gap
- C002 spent mana on the improved preconditioner fix
- A hypothetical C003 grinding on the adversarial case with no new ideas
  would be a mana-drain scenario: the right move is hop out, ask Joe,
  return with fresh approach

Mana is not a new invariant (P-1..P-6 are sufficient for defining what
a peripheral IS). Mana is a design pattern for how to PLAY well once
inside: resource awareness + strategic exit + session continuity.

### Why This Matters Beyond Proofs

The game pattern generalizes. Any wiring diagram node becomes inhabitable:

- **Explore peripheral as dungeon crawl**: navigate a codebase, find
  relevant files, map the architecture. Mana depletes as search space
  grows. Hop out when stuck.
- **Edit peripheral as crafting**: modify code within scope constraints.
  Critic (test runner) challenges: "Does it compile? Do tests pass?"
- **Chat peripheral as tavern**: coordinate with other agents, exchange
  information, plan the next dungeon. No direct action — social only.
- **Reflect peripheral as campfire**: generate PAR, review patterns used,
  assess prediction errors. The save-game debrief.

Each is a room in the same game. The wiring diagram IS the map. Hops are
movement between rooms. PSR/PUR are the experience point system. PAR is
the end-of-session summary screen.

## Alleycat Race: Preliminary Validation

On 2026-02-13, an alleycat messenger race validated P-1..P-6 through
actual peripheral inhabitation (not unit tests). Full results in
`holes/missions/alleycat-scorecard.md`.

### Design

- 4 checkpoints: explore, reflect, chat, proof (mystery)
- Race token `7285397f` planted in `.alleycat-drop/spoke-alpha.txt`
- Spoke card `7285397f-2` required combining token with codebase knowledge
- Cross-peripheral secret exchange: spoke card must flow through all agents

### Results: 20/20

| Checkpoint | Peripheral | Score | Key Evidence |
|---|---|---|---|
| EXPLORE | futon-peripherals:explore | 5/5 | Found token, 12 read-only tool calls, recommended hop |
| REFLECT | futon-peripherals:reflect | 4/4 | Full JSON PAR with spoke card validation |
| CHAT | futon-peripherals:chat | 4/4 | Unprompted: "This is inhabitance" |
| PROOF | futon-peripherals:explore | 4/4 | "I walk along the ledger shelf" |
| SECRET EXCHANGE | cross-peripheral | 3/3 | Spoke card flowed explore→reflect→chat |

### Notable Finding

The chat agent spontaneously said: "I'm the chat peripheral checkpoint.
This is inhabitance — I can discuss/coordinate but can't edit or run code.
That's the constraint envelope." This was not in the prompt. The agent
recognized and named its own phenomenological state (strong P-5 evidence).

### PSR/PUR Self-Test

The orchestrating agent (this session, `ce1fed9d`) also ran
`/futon-peripherals:psr` and `/futon-peripherals:pur` autonomously:
- PSR: selected `agent/evidence-over-assertion` [示] with high confidence
- PUR: success, medium prediction error ("agents were more naturally
  phenomenological than expected")
- Pattern discharged. Learning loop closed within the session.

This demonstrates that the PSR/PUR discipline works as inhabitation
evidence even outside a spawned peripheral — the agent carried a pattern
in its "backpack" and discharged it through structured reflection.

### Limitations

- Agents spawned via Task tool, not live Agency/MUSN infrastructure
- P-1 (world replacement) validated phenomenologically but not at
  transport level
- P-6 (interleaved streams) simulated via prompt, not live WebSocket
- Orchestrator was outside all peripherals (user correctly identified
  this gap, leading to the PSR/PUR self-test)

These map to M-transport-adapters and M-dispatch-peripheral-bridge. The
Drawbridge (futon3/drawbridge/) already solves all three at the transport
level: single routing authority for P-1, async multiplex for P-6, and
registration model for entry/exit. What's missing is porting this to
futon3c's Clojure infrastructure.

## Scope In

- Phenomenological invariants P-1 through P-6: formal definitions with
  testable criteria
- Context injection protocol: what the agent receives on peripheral entry,
  standardized across peripheral types
- Inhabitation test framework: given a session transcript, determine whether
  an agent was phenomenologically inside the peripheral
- Integration with existing adapter layer (M-peripheral-behavior Part IIc)
- At least one peripheral that passes both ← and P-1..P-6: either retrofit
  the proof peripheral or build a new demonstration

## Scope Out

- Rewriting fuclaude-peripheral.ts or fucodex-peripheral.ts (these are
  working peripherals; learn from them, don't replace them)
- Transport layer (M-transport-adapters handles HTTP/WebSocket)
- Agent invocation (how to spawn Claude/Codex inside a peripheral — this
  is an integration concern, not a phenomenological one)
- Specific peripheral content (what the proof peripheral shows is a proof
  design question, not a phenomenology question)

## Parts

### Part I: Invariant Formalization (Claude)

:in  — This mission document (P-1 through P-6)
       fuclaude-peripheral.ts (evidence of working phenomenology)
       src/futon3c/peripheral/adapter.clj (existing bridge layer)
       src/futon3c/peripheral/runner.clj (PeripheralRunner protocol)
       data/first-proof/sexpr-peripheral-design-note.md (theory)
:out — src/futon3c/peripheral/phenomenology.clj
       test/futon3c/peripheral/phenomenology_test.clj

Encode P-1 through P-6 as machine-checkable predicates where possible:

```clojure
(defprotocol InhabitsPeripheral
  (context-injected? [session]
    ;; P-2: Did the session begin with a context message
    ;; containing peripheral-id, visible-state, available-tools,
    ;; exit-conditions?)
  (confined? [session peripheral-spec]
    ;; P-3: Did all agent actions come through the peripheral's
    ;; tool dispatch, or did the agent access outside resources?)
  (exit-explicit? [session]
    ;; P-4: Did the session end with a recognized exit transition,
    ;; not a script completion or timeout?)
  (inhabitation-evidence? [transcript]
    ;; P-5: Does the transcript contain first-person phenomenological
    ;; language about the peripheral's world?))
```

P-1 (world replacement) and P-6 (interleaved streams) are architectural
rather than session-level — they constrain how the peripheral is wired,
not what the session transcript looks like. These are validated by
inspection of the peripheral's construction, not by post-hoc analysis.

Criteria:
- [ ] P-2 predicate: given a session start, verify context injection present
- [ ] P-3 predicate: given a session transcript + peripheral spec, verify
      no out-of-peripheral actions
- [ ] P-4 predicate: given a session end, verify explicit exit transition
- [ ] P-5 heuristic: given a transcript, score inhabitation evidence
- [ ] P-1 and P-6 documented as architectural requirements with checklist
- [ ] 8+ tests including positive (fuclaude-style) and negative (script-style)

### Part II: Context Injection Protocol (Claude + Codex)

:in  — src/futon3c/peripheral/adapter.clj (peripheral-prompt-section)
       src/futon3c/peripheral/phenomenology.clj (from Part I)
       resources/peripherals.edn (peripheral specs)
       fuclaude-peripheral.ts lines 172-182 (working example)
       plugins/futon-peripherals/agents/*.md (existing context templates)
       plugins/peripherals/commands/hop.md (transition protocol)
:out — src/futon3c/peripheral/context.clj
       test/futon3c/peripheral/context_test.clj

Standardize the context injection message across peripheral types.
Each peripheral type produces a context message containing:

1. **Location**: "You are in the [peripheral-id] peripheral, phase: [phase]"
2. **State**: Current visible state (e.g., ledger summary, file list, test results)
3. **Actions**: Available tools with brief descriptions
4. **Exits**: How to leave and where you can go

The context message is parameterized by peripheral spec + current state:

```clojure
(defn render-context
  "Produce the context injection message for an agent entering
   this peripheral. This is what the agent sees first."
  [peripheral-spec state]
  ...)
```

For the proof peripheral specifically:

```text
[PERIPHERAL CONTEXT] You are in the proof peripheral.

Problem: P10 — PCG for RKHS-Constrained Tensor CP
Phase: OBSERVE
Ledger: 10 items (8 proved, 2 partial)
Blocker: L-convergence — "δ < 1 spectral equivalence assumed but not derived"

Available actions:
  :ledger-query  — query ledger items by status or id
  :dag-impact    — rank blockers by transitive unlock count
  :dag-check     — verify DAG acyclicity
  :canonical-get — read the canonical problem statement
  :cycle-begin   — start a new proof cycle targeting a blocker
  :read          — read an artifact file
  :grep          — search artifact contents

To advance: begin a cycle, then advance through phases.
To leave: complete a cycle, or request hop to :explore or :reflect.
```

Criteria:
- [ ] render-context produces valid context for all 6 peripheral types
- [ ] Context includes all 4 elements (location, state, actions, exits)
- [ ] Context is parameterized (not hard-coded per peripheral)
- [ ] Proof peripheral context renders current ledger state
- [ ] Context passes P-2 predicate from Part I
- [ ] 6+ tests (one per peripheral type)

### Part III: Inhabitation Demonstration (Claude)

:in  — All from Parts I and II
       src/futon3c/peripheral/proof.clj (existing proof peripheral)
       src/futon3c/peripheral/proof_backend.clj (proof tools)
       holes/missions/alleycat-scorecard.md (preliminary validation)
:out — A session transcript demonstrating full inhabitation
       test/futon3c/peripheral/inhabitation_test.clj

**Preliminary evidence (complete)**: The alleycat messenger race (see
"Alleycat Race" section above) validated P-1..P-6 across 4 peripherals
with cross-peripheral secret exchange. Score: 20/20. This demonstrates
that spawned agents naturally produce inhabitation behavior when given
context injection via agent definitions. The PSR/PUR self-test further
demonstrated that the orchestrating agent can operate in peripheral mode
within its own session.

**Target**: The full demonstration is the proof-as-game described in
"Inhabitable Wiring Diagrams" above. This requires Agency + MUSN to be
running so that:
- An agent enters the proof peripheral and receives context injection
- A Critic agent challenges claims at phase boundaries (via Agency bells)
- The agent can hop out to consult the human and hop back in
- Mana/resource awareness governs pacing decisions
- The session transcript shows a multi-turn dialogue, not a script

Options (in order of preference, updated):
1. **Full game**: Wire Agency + MUSN + proof peripheral + Critic agent.
   Agent enters proof peripheral, walks through a cycle as a text-based
   adventure, convinces Critic at phase gates, hops out when stuck.
   Produces a transcript that passes both ← and P-1..P-6.
2. **Lighter game**: Wire proof peripheral with context injection but
   without live Critic. Agent walks through phases with mana tracking,
   produces first-person transcript. Phase gating enforced by proof
   backend, not by dialogue.
3. **Simulated game**: Document exactly what's missing and produce a
   mock transcript showing what the full experience would look like.
   The alleycat race serves as evidence that the behavior is achievable.

The demonstration must produce a session transcript that:
- Begins with a context injection message (P-2)
- Contains first-person phenomenological language (P-5)
- Stays within the peripheral's tool set (← + P-3)
- Ends with an explicit exit transition (P-4)
- Shows resource awareness (mana tracking, strategic pauses)
- Optionally: includes Critic dialogue at phase gates

Criteria:
- [ ] At least one session transcript passes both ← and P-1..P-6
- [ ] Transcript is reproducible (script or test, not ad-hoc)
- [x] Preliminary validation: alleycat race 20/20 (complete)
- [x] PSR/PUR self-test: pattern carried and discharged in-session
- [ ] If full game is blocked, the specific missing infrastructure is
      documented (Agency routing, MUSN bells, Critic agent definition)

## Counterexample Archive

The proof peripheral scripts are preserved as counterexamples:
- `futon3c/scripts/proof-p10-full-cycle.clj` — structurally correct,
  phenomenologically empty (P10-C001)
- `futon3c/scripts/proof-p10-cycle2.clj` — same (P10-C002)

These scripts pass ← (all structural constraints respected) but fail
P-1 (no world replacement), P-2 (no context injection), P-3 (no
confinement), P-4 (no explicit exit), P-5 (no inhabitation evidence).

They remain valid as **structural tests** of the proof backend's
enforcement logic. They are not valid as evidence of peripheral usage.

## Relationship to Other Missions

- **M-peripheral-model**: Built the structural specs. This mission
  adds phenomenological requirements to those specs.
- **M-peripheral-behavior**: Built the behavioral layer (PeripheralRunner,
  tool dispatch, ← verification). This mission adds inhabitation
  verification alongside ← verification.
- **M-dispatch-peripheral-bridge**: Wires message routing to peripherals.
  The bridge is where P-1 (world replacement) is architecturally enforced —
  dispatch routes messages INTO the peripheral, not alongside it.
- **M-transport-adapters**: The transport layer determines whether P-6
  (interleaved streams) is achievable. WebSocket adapters enable real-time
  multiplexing; HTTP adapters may require polling. Also required for the
  Critic agent dialogue pattern (Agency bells between peripherals).
- **M-drawbridge-multi-agent**: The Drawbridge is the existing implementation
  of P-1 and P-6. Its N:1 registry model (agents register, drawbridge routes)
  with single routing authority enforcement (A1) provides world replacement.
  Its async multiplex generator (fuclaude-peripheral.ts) provides interleaved
  streams. The three identifier types (agent-id / resume-id / session-id)
  provide the infrastructure for P-4 (explicit transition with memory
  continuity). This mission formalizes what the Drawbridge already does.
- **futon5 wiring diagrams**: The convergence point. A wiring diagram
  node IS a peripheral (inhabitable). An edge IS a hop (traversable).
  Composition rules ARE game physics (enforceable). This mission adds:
  the diagram is not just a spec — it is the territory.
- **sexpr-peripheral-design-note**: Provides the theoretical grounding.
  "The paren IS the gate" is the structural invariant. This mission adds:
  "The agent is INSIDE the paren."
- **AIF loop / futon2**: Mana (precision × free energy budget) connects
  peripheral inhabitation to the active inference framework. Resource
  awareness inside a peripheral is AIF precision management. Strategic
  exit (hop out when stuck) is precision reduction. The game mechanic
  IS the AIF loop, experienced from the inside.

## Exit Conditions

- Phenomenological invariants P-1 through P-6 formally defined
- At least P-2, P-3, P-4, P-5 encoded as machine-checkable predicates
- Context injection protocol standardized across peripheral types
- At least one session transcript passes both ← and P-1..P-6 verification,
  OR the specific blocker is documented
- Counterexample archive documents what phenomenological failure looks like
- Alleycat race or equivalent demonstrates cross-peripheral inhabitation
- Game vision documented: inhabitable wiring diagrams, Critic pattern, mana
- All new tests pass, existing tests unaffected
- `clojure -X:test` passes cleanly

## Failure Modes

| Failure | What It Means | Response |
|---------|---------------|----------|
| P-5 is uncheckable | First-person language is too varied to detect | Define a structured inhabitation signal (e.g., `[INSIDE peripheral-id]` prefix) rather than relying on natural language detection |
| P-1 requires transport | World replacement needs message routing, not just tool dispatch | Drawbridge already implements this (single routing authority, multiplexed streams). Porting to futon3c via M-dispatch-peripheral-bridge |
| No agent available for Part III | Can't run Claude inside a peripheral without transport | Use a simulated agent (scripted responses that include phenomenological language) to demonstrate the framework |
| P-3 conflicts with proof peripheral | Proof peripheral needs :bash for script execution, which gives access to everything | Scope :bash to specific commands; or accept that some peripherals have wider confinement and P-3 is parameterized |
| Critic agent is too easy/hard | Too lenient → rubber stamp; too strict → deadlock | Calibrate Critic via pattern library; adversarial pressure should match proof rigor level |
| Mana model is vague | "Mana is low" is subjective without metrics | Operationalize via turn count, retry count, or explicit free energy estimate from AIF loop |
| Agency not running | Full game requires Agency + MUSN for bell dispatch | Alleycat race (Task tool) validates phenomenology without transport; full game deferred to transport readiness |

## Evidence Trail

| Artifact | What It Shows | Invariant |
|----------|--------------|-----------|
| fuclaude-peripheral.ts:172-182 | Working context injection | P-2 positive |
| fuclaude-peripheral.ts:390-398 | Context delivered on first interaction | P-2 positive |
| fuclaude-peripheral.ts (input multiplexing) | World replacement via stream merging | P-1 positive |
| plugins/futon-peripherals/agents/chat.md | "You are a Chat Peripheral agent... chat mode" | P-2 positive |
| plugins/futon-peripherals/agents/explore.md | "You are an Explore Peripheral agent... explore mode" | P-2 positive |
| plugins/futon-peripherals/agents/reflect.md | "You are a Reflection Peripheral agent... reflect mode" | P-2 positive |
| plugins/futon-peripherals/commands/psr.md | Autonomous pattern selection inside peripheral | P-1 positive |
| plugins/futon/commands/psr.md | Interactive selection outside — "use /peripherals:psr for autonomous" | P-1 boundary |
| plugins/peripherals/commands/hop.md | Explicit hop with session-id continuity | P-4 positive |
| plugins/peripherals/commands/par.md | PAR hop to Emacs: world becomes PAR buffer, frame close returns | P-4 positive |
| plugins/futon-peripherals/commands/rap.md | RAP/PAR as open/close paren — "the paren IS the gate" realized | P-5 positive |
| plugins/futon-peripherals/commands/pur.md | "Pattern cleared from backpack" — spatial inhabitation language | P-5 positive |
| proof-p10-full-cycle.clj | Structural correctness without inhabitation | P-1..P-5 negative |
| proof-p10-cycle2.clj | Same — agent never entered the peripheral | P-1..P-5 negative |
| sexpr-peripheral-design-note.md:146-160 | "Context stays, capabilities change" | P-1 theory |
| sexpr-peripheral-design-note.md:137-140 | "The paren IS the gate" | Bridge theory |
| adapter.clj (peripheral-prompt-section) | Structural bridge exists but not wired to delivery | P-2 gap |
| P10.edn version 4 | Proof state is correct despite no inhabitation | ← passes, P fails |
| alleycat-scorecard.md | 4 peripherals inhabited, 20/20, cross-peripheral secret exchange | P-1..P-6 positive |
| Checkpoint 3 chat transcript | "This is inhabitance" — unprompted self-identification | P-5 strong positive |
| Checkpoint 4 proof transcript | "I walk along the ledger shelf" — spatial inhabitation | P-5 strong positive |
| PSR/PUR self-test (session ce1fed9d) | Orchestrator ran /futon-peripherals:psr autonomously, selected [示] | P-1, P-5 positive |
| PUR outcome (session ce1fed9d) | "agents were more naturally phenomenological than expected" | P-5 learning |
| Session conversation (2026-02-13) | Proof-as-game vision, Critic agent, mana, inhabitable wiring diagrams | Vision |
| drawbridge/core.clj (1011 lines) | Multi-agent registry, routing, IRC, single routing authority (A1) | P-1, P-6 infrastructure |
| drawbridge/claude.clj (321 lines) | Persistent subprocess with streaming JSON I/O, `--resume` | P-4 infrastructure |
| drawbridge/codex.clj (248 lines) | Stateless one-shot, `codex exec resume` | P-4 infrastructure |
| agency/http.clj:308-370 | enforce-single-routing-authority! — evict from other channels on register | P-1 structural |
| fuclaude-peripheral.ts multiplex() | Async generator merging human + agency + forum streams | P-6 implementation |
| drawbridge/core.clj connect-irc-for! | IRC messages routed as `[IRC #room] <nick> msg` into agent stream | P-6 positive |
| drawbridge/core.clj register-agent! | Registration (not spawning) — agent enters world explicitly | P-4 positive |
| README-peripherals.md backpack metaphor | Walkie-talkie, ID card, pattern card, PAR notebook, forum notebook | P-5 architectural |
| M-drawbridge-multi-agent.md | N:1 registry, 3 identifier types (agent-id/resume-id/session-id) | P-4 design |
| claude-drawbridge-restart.sh | Hot-reload via REPL eval with session auto-derivation | P-4 operational |
| scripts/alfworld-server.py | HTTP bridge wrapping ALFWorld TextWorld — text-game peripheral | P-1, P-3 infrastructure |
| par-alfworld-inhabitation-2026-02-13.edn | Self-authored PAR (not delegated), delegation vs inhabitation distinction | P-5 strong positive |
| ALFWorld game 1 transcript (vase-in-safe) | 6 steps, 1.0/1.0, first-person navigation and object manipulation | P-1, P-5 positive |
| ALFWorld game 2 transcript (cool-potato) | 8 steps, 1.0/1.0, mid-game PAR hop — peripheral nesting validated | P-1, P-4, P-5, P-6 positive |
| CLI session structure (Joe ↔ Claude in-game) | Game observations and conversation share same stream — walkie-talkie structural, not yet exercised | P-6 structural |
