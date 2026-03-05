# Mission: CYDER — Cybernetic Development Environment that Rocks

## Status: IDENTIFY

## Motivation

On 2026-03-01, stopping a Tickle conductor loop took 15 minutes of
shell-escaping archaeology: tmux `send-keys` mangled `!`, the port we
thought was nREPL was actually an HTTP eval endpoint, and we had to use
`resolve` + `deref` through a drawbridge eval to call a stop function
stored in an ad-hoc atom in dev.clj. This for the *simplest* orchestrator
— a 2-minute timer loop.

The Emacs `*futon-agents*` buffer shows agents beautifully: who's registered,
what they're doing, when they were last active. But agents are only one of
six categories of running thing in futon3c. The other five — servers,
bridges, daemons, peripherals, state machines — live in scattered atoms
with no uniform way to list, inspect, or stop them.

## The Core Insight: Every Peripheral Is a REPL

CIDER gives Clojure developers a unified interactive surface over what's
running: you can jack in to a REPL, inspect live objects, step through
evaluations, browse connections. Futon3c needs the same — not just for
Clojure, but for its own operational layer.

The analogy runs deeper than surface similarity. Every peripheral and
peripheral-like thing in futon3c *is* a REPL:

- The **proof peripheral** reads a goal state, evaluates a tactic, prints
  the updated proof DAG
- The **tickle conductor** reads IRC + GitHub state, evaluates an LLM
  decision, prints a page or PASS
- The **mission peripheral** reads a phase, evaluates a step, prints the
  updated mission

The `PeripheralRunner` protocol (`start/step/stop`) is literally
`init/eval/quit` with different names. The reason formal peripherals are
manageable and ad-hoc ones aren't is precisely that the formal ones expose
their loop structure. You can step them, inspect between steps, and exit
cleanly.

So the question isn't "how do we track processes?" — it's **"how do we
jack in to any running loop, look around, step it if needed, and stop it
when we're done?"** The process registry is infrastructure; the jack-in
protocol is the real deliverable.

## What "Cybernetic" Means Here

Cybernetics: the science of communication and control in complex systems.
The defining property of a cybernetic system is that it can observe and
regulate itself. Right now futon3c can observe its *agents* but not its
*infrastructure*. A conductor loop can run indefinitely with no way to
inspect or stop it from the observation surface. That's not cybernetic —
it's a blind spot.

CYDER closes the loop: every running process is observable, inspectable,
and stoppable from a single surface. REPL-like things can additionally
be jacked into and stepped.

## The Audit (2026-03-01)

A comprehensive audit found **32 runnable things** in 6 categories:

### A. Formal Peripherals (13)
chat, explore, edit, test, deploy, reflect, proof, mission,
mission-control, evidence, alfworld, discipline, cycle

**Lifecycle**: Clean. All implement `PeripheralRunner` protocol
(start/step/stop). Per-connection state in transport layer.

**Gap**: Not globally enumerable. No way to ask "which peripherals are
currently active across all connections?" The transport tracks them per-WS
connection, but there's no aggregate view.

### B. Pseudo-Peripherals / Orchestrators (4)
tickle-watchdog, tickle-conductor, tickle-llm, invoke-ticker

**Lifecycle**: Ad-hoc. Stop-fns stored in atoms (`!tickle`, `!tickle-conductor`).
You must know the atom name. Tickle-llm has no explicit stop.

**Gap**: Not registered anywhere. Not visible on any surface. The tickle
incident proved this: we couldn't stop the conductor without reverse-engineering
its shutdown path.

### C. Background Threads / Daemons (3)
evidence-store, codex-ws-bridge, evidence-replication

**Lifecycle**: Mixed. Codex WS bridge has a stop-fn via `stop-agents!`.
Evidence replication is internal to the bridge (hidden `:poll!` fn).
Evidence store (in-memory atom) has no stop concept.

**Gap**: Replication poller is invisible. No way to know it's running
or check its state without knowing the internal bridge handle.

### D. Servers / Listeners (5)
HTTP+WS (7070), IRC (6667), Drawbridge (6768), futon1a (7071), futon5 (7072)

**Lifecycle**: Mostly clean. Stop-fns stored in system atoms (`!f3c-sys`,
`!f1-sys`, `!f5-sys`). IRC server has no explicit stop-fn.

**Gap**: Not queryable as a group. Port information scattered across atoms.
No single "what's listening?" view.

### E. Bridges (3)
IRC-relay-bridge, dispatch-relay, ngircd-bridge (Python)

**Lifecycle**: No explicit stop for any of them. Callback-based; torn
down implicitly when their parent system stops. ngircd bridge is an
external process (SIGTERM only).

**Gap**: Completely invisible. No registration, no inspection, no stop.
The dispatch relay's gating state (`!ungated-nicks`) is an implementation
detail, not an observable property.

### F. State Machines with Lifecycle (4)
WS-connection-state, peripheral-session, agent-session, mission-control-sessions

**Lifecycle**: Clean for mission-control (start/stop/list/resume). Agent
sessions persist via files (`/tmp/futon-*-session-id`). WS connections
managed by transport. Peripheral sessions managed per-connection.

**Gap**: No aggregate view of active sessions across all types.

## Proposed Invariants

### I-6: Every Long-Running Process Registers Itself

**IF** a process will run for more than one request-response cycle
**HOWEVER** its implementation details vary (thread, future, server, bridge)
**THEN** it must register with a process registry on startup, providing:
name, type, start-time, and a stop-fn
**BECAUSE** unregistered processes are invisible, and invisible processes
are unstoppable (the tickle incident).

### I-7: Every Registered Process Is Stoppable

**IF** a process is registered (I-6)
**THEN** `DELETE /api/alpha/processes/:id` calls its stop-fn and
deregisters it
**BECAUSE** "for god's sake stop tickling Codex right now" must be a
one-liner, not a 15-minute archaeology expedition.

### I-8: Every Registered Process Is Inspectable

**IF** a process is registered (I-6)
**THEN** `GET /api/alpha/processes/:id` returns its current state as data
(at minimum: name, type, status, start-time, last-active; optionally:
custom state map)
**BECAUSE** you can't regulate what you can't observe (cybernetics 101).

### I-9: The Process Surface Is Browsable

**IF** processes are registered, stoppable, and inspectable (I-6/I-7/I-8)
**THEN** there exists a single surface (Emacs buffer, HTTP endpoint, REPL
command) that shows all of them
**BECAUSE** the `*futon-agents*` buffer proves this pattern works — agents
are manageable precisely because they're visible. Extend to everything.

### I-10: REPL-like Processes Are Jackable

**IF** a process has a loop structure (read/eval/print cycle)
**THEN** an observer can attach to it without disrupting it, see its
current state, optionally single-step it, and disconnect
**BECAUSE** every peripheral is a REPL, and the defining operation on
a REPL is jack-in. You should be able to watch a tickle conductor decide,
see a proof peripheral's DAG, or inspect a mission cycle — live, without
killing the process.

## CIDER Correspondence

| CIDER concept | CYDER equivalent |
|---------------|------------------|
| Connection (nREPL session) | Agent session (claude-1, codex-1) |
| Thread (background eval) | Background process (conductor, replication) |
| Middleware stack | Peripheral chain (explore → edit → test) |
| Inspector (data browser) | Process state viewer / jack-in |
| Stacktrace (error navigation) | Evidence trail (what happened) |
| `cider-debug` (step evaluator) | Peripheral stepping (step-fn) |
| `cider-quit` | `DELETE /api/alpha/processes/:id` |
| `cider-connections` buffer | `*futon-processes*` buffer |
| `cider-jack-in` | `(cyder/jack-in! id)` — attach to live process |

## The Two-Layer Model

Runnable things divide into two kinds based on whether they have a
meaningful loop structure:

### Layer 1: REPL-like (steppable)

Peripherals, conductors, state machines — anything with a read-eval-print
cycle. The `PeripheralRunner` protocol (`start/step/stop`) *is* a REPL
with different names (`init/eval/quit`).

| Runnable | Read | Eval | Print |
|----------|------|------|-------|
| Proof peripheral | goal state | apply tactic | proof DAG |
| Mission peripheral | phase + context | execute step | updated mission |
| Tickle conductor | IRC + GitHub + task state | LLM decision | page or PASS |
| Mission Control | portfolio observation | inference cycle | recommendation |
| Codex-code | issue prompt | codex invoke | PR or result |

For REPL-like things, CYDER should support **jack-in**: an observer
(human from Emacs, or another agent) attaches to a running loop
*without disrupting it*. Read-only observation of live state, with the
option to single-step if the loop supports it.

This is the `cider-inspector` move: you don't stop the JVM to look at
an object — you browse it live. Similarly, you should be able to watch
a tickle conductor make its decisions in real time, see the proof
peripheral's current DAG, or inspect what the mission cycle is doing —
without killing the process to read its atoms.

**Jack-in protocol** (sketch):
```clojure
(cyder/jack-in! "tickle-conductor")
;; => {:id "tickle-conductor"
;;     :type :daemon
;;     :state {:cycle-count 47
;;             :last-action :message
;;             :last-text "@codex-1 Batch-9 timed out..."
;;             :tasks {#22 :merged, #53 :merged, ...}
;;             :next-tick-at #inst "..."}
;;     :steppable? true}

(cyder/step! "tickle-conductor")
;; => runs one cycle, returns result

(cyder/disconnect! "tickle-conductor")
;; => detaches observer, loop continues
```

### Layer 2: Infrastructure (non-steppable)

Servers, bridges, listeners — things that accept connections or relay
messages. They don't have a meaningful "step." You can list them, check
health, and stop them, but you can't single-step an HTTP server.

| Runnable | Observable State |
|----------|-----------------|
| HTTP+WS server (7070) | open connections, request count, uptime |
| IRC server (6667) | connected clients, rooms, message rate |
| Drawbridge (6768) | active sessions, last eval |
| Codex WS bridge | connection state, replication queue |
| ngircd bridge | PID, connected, last message |

For infrastructure, CYDER provides **list/inspect/stop** but not step.
The Emacs buffer shows them as rows with status indicators.

### The Unifying Surface

Both layers appear in the same `*futon-processes*` buffer (and HTTP API),
distinguished by type. REPL-like things have a "jack-in" action;
infrastructure things have "inspect" only. The buffer is a live view,
updating as processes register/deregister — same pattern as
`*futon-agents*`.

## Architecture Sketch

```
┌─────────────────────────────────────────────────────────┐
│                   Process Registry                       │
│                                                          │
│  {:id "tickle-conductor"                                 │
│   :type :daemon            ; :daemon :server :peripheral │
│   :layer :repl             ; :repl or :infra             │
│   :started-at #inst "..."                                │
│   :stop-fn #fn             ; (stop-fn) → nil             │
│   :state-fn #fn            ; (state-fn) → data           │
│   :step-fn #fn             ; (step-fn) → step result     │
│   :last-active #inst "..."}                              │
│                                                          │
│  Layer :repl  → stoppable + inspectable + jackable       │
│  Layer :infra → stoppable + inspectable                  │
└────┬──────────────┬──────────────┬───────────────────────┘
     │              │              │
  HTTP API    Emacs buffer      REPL
  /api/alpha/ *futon-processes* (cyder/ls)
   processes/                   (cyder/inspect id)
   processes/:id/jack-in        (cyder/jack-in! id)
                                (cyder/step! id)
                                (cyder/stop! id)
```

The registry is a single atom. Each runnable thing calls
`(cyder/register! {:id ... :type ... :stop-fn ... :state-fn ...})` on
startup and `(cyder/deregister! id)` on shutdown. REPL-like things
additionally provide `:step-fn` and `:layer :repl`. The HTTP routes,
Emacs buffer, and REPL helpers are thin views over this atom.

Registration shape:
```clojure
;; Layer 1 (REPL-like): peripherals, conductors, state machines
(cyder/register!
 {:id       "tickle-conductor"
  :type     :daemon
  :layer    :repl
  :stop-fn  #(reset! running? false)
  :state-fn #(deref !tickle-tasks)
  :step-fn  #(tickle-conduct!)})

;; Layer 2 (infrastructure): servers, bridges, listeners
(cyder/register!
 {:id       "http-server"
  :type     :server
  :layer    :infra
  :stop-fn  server-stop-fn
  :state-fn #(do {:port 7070
                  :connections (count @ws-connections)
                  :uptime-ms (- (now) started-at)})})
```

## Phased Scope

### Phase 0: Mission Runner (proof of concept)

The simplest and most immediately useful case. At any point Joe might
have several missions in flight — M-cyder at IDENTIFY, M-self-representing-stack
paused at VERIFY, M-tickle-overnight just closed. Today the only way to
know this is to remember it or grep mission docs for `Status:` lines.

**Deliverable**: A live mission dashboard — glanceable from Emacs — that
shows every active mission with its current phase, patterns used (from
PSR/PUR records), and last activity. Jack-in to a mission shows its full
state: open questions, evidence collected, which derivation step is next.

```
Mission                       Phase     Patterns Used      Last Active
──────────────────────────────────────────────────────────────────────────
M-cyder                       IDENTIFY  —                  2 min ago
M-self-representing-stack     VERIFY    —                  2 days ago
M-tickle-overnight            CLOSED    learn-as-you-go    1 day ago
```

This is CYDER's proof of concept because missions are the most natural
REPL: each derivation phase (IDENTIFY → MAP → DERIVE → ARGUE → VERIFY →
INSTANTIATE) is a step, pattern selections are eval inputs, and evidence
entries are the print output. The mission peripheral already has this loop
structure. CYDER surfaces it.

**Concrete steps**:
1. Process registry (`cyder.clj`) with register/deregister/list/inspect/stop
2. Mission-aware state-fn that reads mission docs + evidence store for
   phase, patterns, last-active
3. HTTP endpoint: `GET /api/alpha/processes` returns all registered processes
4. Emacs buffer: `*futon-processes*` polls the endpoint (same pattern as
   `*futon-agents*`)
5. Jack-in: `GET /api/alpha/processes/:id` returns full mission state

### Phase 1: Infrastructure Layer

Extend the registry to servers, bridges, and daemons (Layer 2). These
don't need jack-in or stepping — just list/inspect/stop.

- Retrofit: HTTP+WS server, IRC server, Drawbridge, futon1a, futon5
- Retrofit: codex WS bridge, dispatch relay, IRC relay bridge
- Retrofit: tickle watchdog, tickle conductor
- `DELETE /api/alpha/processes/:id` to stop any process

### Phase 2: Full Jack-In Protocol

Extend jack-in to all REPL-like processes (Layer 1). Attach to a running
peripheral session, observe its state live, optionally step it.

- Jack-in protocol for REPL-like processes (attach/state/step/disconnect)
- `POST /api/alpha/processes/:id/jack-in` (attach observer)
- `POST /api/alpha/processes/:id/step` (single-step if steppable)
- Retrofit formal peripherals: wire `PeripheralRunner` instances into
  the registry so active peripheral sessions are visible
- Emacs jack-in buffer (one buffer per jacked-in process)

### Out of Scope (for now)
- Automatic restart / supervision trees
- Cross-futon process federation
- Historical process log (evidence store integration)
- Full Emacs IDE integration (cyder-mode, keybindings) — follow-on once
  the API surface is stable

## Relation to Existing Work

| Mission | Relationship |
|---------|-------------|
| M-peripheral-model | Defines the peripheral lifecycle; CYDER adds visibility |
| M-peripheral-gauntlet | Tests peripheral scenarios; CYDER tests process lifecycle |
| M-portfolio-inference | Observation surface for missions; CYDER adds observation for processes |
| M-mission-control | Portfolio-level control; CYDER adds infrastructure-level control |
| M-IRC-stability | IRC server lifecycle; CYDER registers it |
| M-tickle-overnight | The incident that triggered this mission |

## Open Questions

1. What does CYDER stand for? Working proposal: **Cybernetic Development
   Environment that Rocks**. Alternatives welcome.

2. Should the process registry be its own namespace (`futon3c.cyder`) or
   extend the existing agency registry (`futon3c.agency.registry`)?
   Argument for separate: processes ≠ agents — agents are identities,
   processes are running loops. Argument for combined: agents are a
   subset of processes; one registry is simpler and avoids two places
   to look.

3. How granular? Register every `future`? Or only things with names and
   intended lifetimes? The audit found 32 — that feels right. Per-request
   futures are too granular; per-connection peripheral instances are a
   judgement call (useful for "who's jacked in to what?" but could be noisy).

4. Should `state-fn` be required or optional? Required forces every
   registrant to think about observability upfront. Optional is easier
   to retrofit but lets invisible processes sneak back in.

5. Jack-in concurrency: can multiple observers jack in to the same
   process? CIDER allows multiple connections to one nREPL. The read-only
   case is straightforward (multiple readers of the same state-fn).
   Stepping is trickier — if two observers both call `step!`, who wins?
   Simplest answer: stepping is exclusive (one stepper, many observers).

6. How does jack-in relate to the existing Emacs chat surface? Today
   Joe interacts with agents via emacs-claude-repl. CYDER jack-in
   could use the same buffer model (one buffer per jacked-in process)
   or a dedicated inspector buffer. TBD based on what feels natural.

## Files (Anticipated)

| File | Purpose |
|------|---------|
| `src/futon3c/cyder.clj` | Process registry (register/deregister/list/inspect/stop) |
| `src/futon3c/transport/http.clj` | HTTP routes for `/api/alpha/processes` |
| `test/futon3c/cyder_test.clj` | Registry + lifecycle tests |
| `dev/futon3c/dev.clj` | Retrofit: register existing runnables on startup |
