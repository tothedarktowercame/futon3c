# P6a — conductor surface discovery

Date: 2026-08-16  
Scope: read-only map and wiring proposal; no production implementation.

## Finding in one sentence

The required mechanism is mostly present: Agency already binds a `claude-N`
identity and session to a turn, the WebSocket transport already binds a live
connection to a `PeripheralRunner` and turns each `tool_action` into a serialized
`runner/step`, and the problem peripheral already supplies the phase/tool gate.
The missing piece is a problem-specific surface binding that selects the live
problem handle for the conductor session and makes conductor effects enter only
through that serialized step seam.

## 1. IRC: transport and output contract

The IRC relay joins an agent/nick/channel and receives `irc_message` frames in
`dev/futon3c/dev.clj:5004-5017`. It mention-gates the turn
(`dev/futon3c/dev.clj:5034-5043`), wraps the user text with
`irc-invoke-prompt` (`dev/futon3c/dev.clj:4966-4993`), and invokes the registered
agent through `coordination/invoke-with-edge!` with `:from`, `:to`, `:surface
"irc"`, and the contracted prompt (`dev/futon3c/dev.clj:5052-5073`). The result
is normalized and posted by the server as the IRC nick
(`dev/futon3c/dev.clj:5091-5101`). Thus the agent is not asked to perform the
delivery; it is told the factual delivery contract and the server performs it.

The contract itself identifies surface, channel, sender, and the fact that the
returned text will be posted by the server (`dev/futon3c/dev.clj:4973-4978`).
This is the implementation introduced by reference commit `f5c3e25f` (`git show
f5c3e25 --stat`: “Enforce explicit surface contracts for Codex replies”). The
governing distinction is stated in `CLAUDE.md:199-218`: a surface contract is
transport metadata, not a tool/capability restriction.

## 2. Emacs REPL surfaces and `claude-N` continuity

### Codex

`codex-repl--surface-contract` constructs the explicit
`emacs-codex-repl` contract and says the response is shown only in that buffer
(`emacs/codex-repl.el:3528-3548`). The streaming call posts to
`/api/alpha/invoke-stream`, naming the Agency agent, surface `emacs-repl`, and
caller (`emacs/codex-repl.el:3860-3871`). `codex-repl-send-input` delegates the
buffer turn to the shared `agent-chat-send-input` machinery
(`emacs/codex-repl.el:4442-4456`). Buffer initialization binds the displayed
session and Agency agent identity (`emacs/codex-repl.el:4471-4489`).

### Claude

Claude uses the same shared chat turn boundary: `claude-repl-send-input` calls
`agent-chat-send-input`, with evidence/frame hooks around the turn
(`emacs/claude-repl.el:1227-1241`). Its transport posts the agent id, prompt,
turn id, `:surface "emacs-repl"`, and caller to `/api/alpha/invoke-stream`
(`emacs/claude-repl.el:903-938`). Initialization auto-registers a `claude-N`,
derives a per-agent session file, and restores the existing session
(`emacs/claude-repl.el:1425-1451`). The chat buffer records both the agent id and
resolved session id (`emacs/claude-repl.el:1397-1423`).

On the server, configured Claude seats are registered with distinct agent ids,
session files, and Emacs sockets; the concrete `claude-1` and `claude-2`
registrations are at `dev/futon3c/dev/agents.clj:336-359`. The invoke function
uses the persisted session on subsequent turns (`dev/futon3c/dev.clj:3506-3528`).
With the warm-pouch route, `agent-pouch/feed-turn!` receives that agent id and
session (`dev/futon3c/dev.clj:3864-3877`), streams events back through the
registered invoke event sink (`dev/futon3c/dev.clj:3904-3915`), and persists any
returned session id (`dev/futon3c/dev.clj:3928-3936`). The outer invocation is
serialized either through the per-agent turn queue or a lock
(`dev/futon3c/dev.clj:3997-4020`). This is the relevant kangaroo/warm-pouch fact:
turn continuity is already keyed by agent/session; a conductor binding need not
invent another agent identity mechanism.

There is no Claude-specific equivalent of the Codex textual helper named
`--surface-contract`. Claude nevertheless transmits the structured surface
field, while the shared server-side turn prompt is the natural seam for adding
the problem-conductor contract.

## 3. Existing agent/peripheral bindings

### What agent-chat does and does not do

`agent-chat.el` is the shared Emacs UI/turn coordinator. It does not itself call
`runner/start` or `runner/step`; the Claude and Codex front ends hand its turns
to `/api/alpha/invoke-stream` as shown above. Therefore agent-chat currently
binds an Emacs buffer to an Agency agent/session, but it does **not** make agent
actions peripheral steps.

The older social-dispatch route can choose a peripheral for an action-mode
message (`src/futon3c/social/dispatch.clj:122-134`) and execute an action list
through `preg/run-chain` (`src/futon3c/social/dispatch.clj:175-216`). This is a
one-shot chain: it mints a session and starts/runs/stops a peripheral rather than
binding an already-open problem cycle. It is useful precedent for typed action
routing, but not the correct state-continuity seam for a conductor.

### The direct transport precedent

The WebSocket transport already implements the desired structural property.
On `peripheral_start` it resolves a peripheral, creates it, calls
`runner/start`, and stores both the peripheral and an atom containing its state
on the authenticated connection (`src/futon3c/transport/ws.clj:289-333`). On
each `tool_action`, it constructs `{:tool ... :args ...}`, calls `runner/step`,
and atomically replaces the state under a lock
(`src/futon3c/transport/ws.clj:335-360`). It refuses action without an active
peripheral (`src/futon3c/transport/ws.clj:341-344`). This is the closest
implementation of “the agent's effects are peripheral steps.” Its current
registry does not include `:problem` (`src/futon3c/peripheral/registry.clj:36-64`),
so it is precedent rather than immediately usable wiring.

### War Machine pilot precedent

`README-pilot.md:43-58` makes the driving discipline explicit: READ, EVAL,
PRINT, and LOOP occur through named machine operations, and the resulting frame
is verified. It also requires consent-gate ids for substantive tools and honest
closure (`README-pilot.md:64-76`). The pertinent lesson is not its domain logic;
it is that a driving role inhabits a stateful machine and substantive acts are
machine transitions, while observation and judgement remain available.

## 4. The problem runner contract today

`PeripheralRunner` is the public lifecycle: `start`, `step`, and `stop`; a step
is exactly `{:tool keyword :args [...]}` and returns the new state and tool
result (`src/futon3c/peripheral/runner.clj:18-32`).

`EvidenceRequiredProblemPeripheral/start` requires `:session-id` and
`:evidence-store`, validates the store, then delegates; `step` and `stop`
delegate without a side channel (`src/futon3c/peripheral/problem.clj:1585-1600`).
`make-problem` composes the cycle engine with the state, ground-control,
checkout, and problem-cycle backends (`src/futon3c/peripheral/problem.clj:1602-1634`).

The phase chain is data, not prose:
`:register`, `:frame`, `:guided-solve`, `:intervene`, `:student-attempts`,
`:adjudicate`, `:promote`, `:close`, sentinel `:completed`
(`src/futon3c/peripheral/problem.clj:27-36`). The allowed tools per phase are
the `base-phase-tools` map (`src/futon3c/peripheral/problem.clj:40-61`), and the
cycle configuration enables required-output enforcement and state save/load
(`src/futon3c/peripheral/problem.clj:761-783`). This is already the authority
that must accept or refuse every effectful conductor act.

The callable conductor currently honors that authority internally:
`raw-step` calls `runner/step`, records the returned state, and converts refusal
or throw to a structured conductor error (`src/futon3c/apm/conductor.clj:25-43`);
`saved-step` follows every successful mutation with `:problem-save`
(`src/futon3c/apm/conductor.clj:45-55`). `open-frame!` constructs and starts the
problem peripheral and advances it through the opening phases
(`src/futon3c/apm/conductor.clj:85-153`). Its dispatch/record/deposit/adjudicate/
close functions are therefore good operation-level adapters, but today nothing
requires a live `claude-N` turn to use them.

Parking already carries the conductor's agent, session, and surface from cycle
context into `/api/alpha/park` (`src/futon3c/peripheral/problem.clj:1096-1123`).
That is evidence that cycle context already has the continuity keys needed for
park/resume; it is not yet an enforcement boundary.

## Proposal: the smallest conductor surface

### Shape

Add one server-owned binding table keyed by `[agent-id session-id]` whose value
is the live conductor handle (and cycle id/version metadata). Install the
binding when `open-frame!` successfully opens a problem cycle; remove it only
after terminal close or explicit, validated abandonment. A bound turn receives
an explicit `problem-conductor` surface contract naming problem, cycle, current
phase, and the rule that effectful conductor operations are submitted as typed
problem actions.

Expose one typed conductor-action transport route. It accepts an operation and
arguments, resolves the authenticated agent/session binding, and invokes the
corresponding conductor adapter. Each adapter ultimately executes
`runner/step`; the returned handle atomically replaces the bound handle. Unknown
operations, wrong phases, stale cycle ids, and unbound sessions are structured
refusals. Do not expose a generic eval function.

The LLM remains fully capable. Reads of state, files, evidence, and status stay
unrestricted. The enforcement claim is narrower: there is no transport route
for an **effectful conductor act** other than the typed surface action. This is
the surface-contract/capability distinction in `CLAUDE.md:199-218`.

### Existing seams to wire

1. **Turn identity and contract:** extend the invoke-stream prompt assembly in
   the server route used by `emacs/claude-repl.el:903-938`; look up the binding
   using the already authenticated/requested agent id and continuing session.
   The Emacs client needs, at most, to render the returned surface state; it
   should not own the binding.
2. **Persistent action serialization:** reuse the exact pattern from
   `src/futon3c/transport/ws.clj:335-354`: a server-owned atom/record, lock
   around action plus state replacement, and refusal when no binding exists.
3. **Problem construction and phase authority:** call only the public conductor
   operations in `src/futon3c/apm/conductor.clj`; they already funnel to
   `runner/step`. The problem phase gate remains solely in
   `src/futon3c/peripheral/problem.clj:40-72` and the cycle engine.
4. **Checkpoint/resume:** store cycle id and latest successful save version in
   the binding after each conductor operation. On process/agent takeover,
   construct the normal problem runtime and call `conductor/resume`; do not
   deserialize a peripheral object or accept caller-supplied state.
5. **Park/resume:** the existing conductor context and `park-dispatch` already
   carry agent/session/surface (`problem.clj:1100-1120`). Resume delivery must
   target the same binding key and re-enter through the typed action route; a
   resumed prose turn must not mutate the handle merely because it was awakened.

### What is genuinely new

- A small live binding registry for `(agent, session) -> problem conductor
  handle`, with compare/lock semantics and terminal cleanup.
- A typed conductor-action request/response route (or a problem-specific
  specialization of the existing WebSocket `tool_action` seam).
- Surface-contract prompt text derived from the bound handle.
- A takeover operation that rebuilds runtime seams and calls the existing
  `conductor/resume` path.

No new problem state machine, phase table, persistence format, agent identity
scheme, dispatch system, or capability restriction is required.

## Risks and acceptance probes for the implementation packet

1. **Two authorities for state.** If a WebSocket connection atom and a
   conductor binding both own state, they can diverge. There must be exactly one
   bound handle atom per cycle; transports resolve it rather than copy it.
2. **Session rebinding/takeover.** `claude-N` identity and CLI session can change
   on restore. Transfer must be explicit and conditional on successfully loading
   the named saved cycle version; never key by agent id alone.
3. **Parked continuation races.** A bell completion may arrive after manual
   takeover or close. Include cycle id/version in continuation data and reject a
   stale resume before action dispatch.
4. **Double execution on retry.** Network retries can replay an action. Require
   an action id and keep a per-cycle receipt ledger, or demonstrate that the
   underlying tool is idempotent; `problem-save` alone is not deduplication.
5. **Prompt-only enforcement.** Merely telling the conductor to use the surface
   is not transport enforcement. Tests must show that an unbound/off-surface
   conductor action has no server route and that every accepted effect appears
   as a problem `:steps` entry.
6. **Overreach into observation.** Do not disable shell, files, memory reads, or
   normal dialogue. Test that read-only observation still works while an
   effectful conductor operation without a binding is refused.
7. **Premature terminal cleanup.** Retain the binding across park and process
   reconnect; clear it only after sentinel completion (or an explicit governed
   abandonment), so the takeover path can resume a saved in-flight cycle.

The minimal end-to-end proof should bind a `claude-N` session, open a frame,
submit one valid and one out-of-phase conductor action through transport,
observe exactly one new problem step for the valid action and a structured
refusal for the other, park and resume on the same binding, then complete the
cycle and show that a post-sentinel action is unrouteable.
