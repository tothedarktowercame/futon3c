# Tickle v2: Mechanical Conductor

Tickle is the coordination agent in futon3c. This document specifies the
clean design — replacing the accumulated layers described in README-tickle.md.

## Core Principle

**Dispatch decisions are mechanical, not LLM-generated.**

An idle agent + an available obligation = PAGE. That's the entire decision.
No Haiku invoke, no prompt engineering, no "should I page or pass?" deliberation.
The conductor reads state from the registry and the proof ledger, and acts.

## Architecture

```
                    ┌─────────────────────┐
                    │   Agent Registry    │
                    │  (idle / invoking)  │
                    └────────┬────────────┘
                             │ reads status
                    ┌────────▼────────────┐
                    │  Mechanical         │
  60s tick ────────►│  Conductor          │────► IRC page (@codex T2-single: ...)
                    │                     │
  post-invoke ────►│  idle + obligation   │
  hook              │  = PAGE             │
                    └────────┬────────────┘
                             │ reads obligations
                    ┌────────▼────────────┐
                    │   Proof Ledger      │
                    │  (FM-001b state)    │
                    └─────────────────────┘
```

Two triggers fire the conductor:

1. **Clock** (60s tick): scan all agents in rotation, dispatch to any that are idle.
2. **Event** (post-invoke hook): when an agent finishes work, immediately try to
   dispatch new work to it. No waiting for the next tick.

## Components

### Mechanical Conductor (`start-fm-conductor!`)

The only dispatch mechanism. Replaces the watchdog, the LLM conductor, and
the bell-driven dispatch queue from v1.

**State**: one atom with:
- `:last-paged {agent-id → epoch-ms}` — cooldown tracking
- `:paged-obligations {agent-id → #{ob-id ...}}` — dedup (won't re-page same work)
- `:cycles-completed` — counter
- `:last-cycle` — last dispatch result for CYDER display

**Rotation**: worker agents only. Mentors and Lab Managers are never paged
with computation tasks. Default: `["codex-1" "claude-3" "codex-2" "codex-3"]`.
Agents not in the registry are silently skipped (no phantom dispatch).

**Cooldown**: 3 minutes per agent. After paging, the agent won't be re-paged
until the cooldown expires, even if they go idle again immediately.

**Obligation dedup**: once an obligation has been paged to an agent, it won't
be paged again to that same agent. If all obligations have been paged,
the conductor passes. The dedup set resets when the conductor restarts.

### Tickle-1 Agent (IRC conversational)

Tickle-1 remains registered as an agent with a Haiku invoke-fn. It responds
to direct `@tickle` mentions from humans on IRC. It does NOT:

- Self-initiate messages (no watchdog scan)
- Make dispatch decisions (no "should I page?" prompts)
- Get invoked by agent completion bells

If a human asks `@tickle status?` on IRC, tickle-1 responds. That's all.

### Post-Invoke Hook (`!post-invoke-hook`)

Set by `start-fm-conductor!`, cleared by `stop-fm-conductor!`.

When any agent finishes an IRC invoke (via the dispatch relay), the hook fires
with the agent-id. The hook calls `fm-dispatch-mechanical!` for that agent,
which checks idle status + available obligations and pages if appropriate.

This replaces `bell-tickle-available!` invoking tickle-1's Haiku, which caused
feedback loops (codex output contained `@tickle` → tickle invoke → more IRC
noise → codex sees noise → ...).

### CYDER Registration

The conductor registers as `fm-conductor` in CYDER:
- `state-fn`: shows per-agent status (`[idle] ready`, `[invoking] for Ns`,
  `[idle] cooldown Ns`) and last dispatch action
- `step-fn`: manual single-step (bypasses cooldown)
- `stop-fn`: stops the loop and clears the hook

Visible in `*processes*` Emacs buffer and via `/api/alpha/processes/fm-conductor`.

## What Was Removed (and Why)

| v1 Layer | Problem | v2 Replacement |
|----------|---------|----------------|
| Watchdog (`start-tickle!`) | Haiku scan every 60s → "you appear stalled" spam to all agents including invoking ones | Mechanical conductor reads registry status directly |
| LLM Conductor (`start-tickle-conductor!`) | Haiku deciding PAGE/PASS → slow, expensive, wrong decisions (paged claude-2 with SAT tasks) | Mechanical rule: idle + obligation = PAGE |
| Bell (`bell-tickle-available!`) | Invoked tickle-1 Haiku on every codex completion → feedback loop via @tickle in output | Post-invoke hook → mechanical dispatch |
| CT Work Queue | Stale, PlanetMath-specific | Not needed for FM-001 |
| Dispatch Queue (bell-driven) | Tangled with Haiku invokes | Post-invoke hook is the reactive trigger now |

## Configuration

```clojure
;; Start with defaults (60s tick, 3min cooldown, worker rotation)
(start-fm-conductor!)

;; Custom tick interval
(start-fm-conductor! {:step-ms 30000})

;; Stop
(stop-fm-conductor!)

;; Manual dispatch (REPL)
(fm-conduct!)                        ;; scan all idle workers
(fm-conduct-targeted! "codex-1")     ;; dispatch to specific agent
```

## Invariants

**T-1: No LLM for dispatch.** The conductor never invokes a language model
to decide whether to page. State + obligations → deterministic action.

**T-2: Workers only.** Mentors (claude-2) and Lab Managers (claude-1) are
never in the conductor rotation. They receive work through different channels
(whistles, direct requests from joe).

**T-3: No feedback loops.** The conductor posts to IRC as "tickle" but never
reads its own output. Agent completion triggers the hook, not an IRC mention
parse. No mechanism exists for conductor output to re-trigger the conductor.

**T-4: Obligation dedup.** An obligation is paged to an agent at most once
per conductor lifetime. If the agent needs the same work re-assigned (e.g.
after a failure), restart the conductor or use `fm-conduct-targeted!` manually.

**T-5: Registry is truth.** Agent idle/invoking status comes from the agency
registry, not from evidence timestamps or IRC message parsing. The registry
is updated by the invoke machinery in real time.

## Outstanding Issues

The mechanical-conductor direction is still correct, but the implementation is
not fully converged yet.

1. `futon3c.dev` decomposition is still in progress.
   The startup/orchestration code now delegates into extracted `dev.*`
   namespaces, but there are still compatibility wrappers in
   `dev/futon3c/dev.clj` and the facade can be simplified further.

2. `tickle_logic.clj` is now evidence-backed, but it is still only a diagnostic
   layer.
   It checks scan/page/escalation ordering and stall-evidence alignment against
   registry + evidence snapshots, but it is not yet wired into a shared
   invariant runner or surfaced through `check-invariants` / HTTP.

3. The watchdog and the mechanical conductor still coexist.
   `src/futon3c/agents/tickle.clj` now emits better scan/page/escalation
   evidence for invariant checking, but the README's intended end-state remains:
   the mechanical conductor should own dispatch, and watchdog behavior should be
   either clearly diagnostic or fully retired.

4. FM obligation sourcing is still specialized.
   The conductor works against the current FM obligation pipeline, but the work
   queue / obligation interface is not yet generalized enough to make Tickle v2
   a clean coordination substrate across domains.

5. Invariant coverage is still uneven.
   The new Tickle logic checks registry/evidence coherence, but we still do not
   have the full cross-domain invariant runner promised in
   `M-fulab-logic.md`.

## Files

| File | What |
|------|------|
| `dev/futon3c/dev.clj` | Top-level facade/orchestrator; delegates startup and conductor helpers into extracted modules |
| `dev/futon3c/dev/bootstrap.clj` | Startup/bootstrap orchestration extracted from `dev.clj` |
| `dev/futon3c/dev/agents.clj` | Core agent registration/startup extracted from `dev.clj` |
| `dev/futon3c/dev/peripheral_agents.clj` | Peripheral agent registration (`tickle-1`, mentor Claude) |
| `dev/futon3c/dev/fm.clj` | Mechanical conductor and FM dispatch helpers |
| `src/futon3c/agents/tickle.clj` | Watchdog scan/page/escalate runtime; now emits evidence-backed scan/page/escalation events |
| `src/futon3c/agents/tickle_logic.clj` | Core.logic invariant layer over registry + evidence snapshots |
| `src/futon3c/agents/tickle_orchestrate.clj` | `fm-assignable-obligations` (used by v2), LLM conductor (v1, not used) |
| `src/futon3c/agency/registry.clj` | Agent status tracking (`:agent/status`, `:agent/invoke-started-at`) |
