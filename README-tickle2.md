# Tickle v2: Mechanical Conductor

Tickle is the coordination agent in futon3c. This document specifies the
clean design — replacing the accumulated layers described in README-tickle.md.

## Core Principle

**Normal coordination is pull-driven; watchdog behavior is secondary.**

The main loop is:

1. an agent finishes work
2. it signals availability / asks for more work
3. the FM regulator assigns the next obligation if one exists

Mechanical dispatch is still the right rule for assignment, but it should be
entered through the agent's own completion/availability signal in the normal
case. The watchdog exists for silence, drift, and repair, not as the primary
scheduler.

## Architecture

```
                    ┌─────────────────────┐
                    │   Agent Registry    │
                    │  (idle / invoking)  │
                    └────────┬────────────┘
                             │ reads status
                    ┌────────▼────────────┐
                    │  FM Regulator       │
 availability bell ─►│  (mechanical       │────► IRC page / whistle /
 post-invoke hook    │   assignment)      │      direct next obligation
                    │                     │
 audit timer  ─────►│  idle + obligation  │
 (secondary)        │  = ASSIGN           │
                    └────────┬────────────┘
                             │ reads obligations
                    ┌────────▼────────────┐
                    │   Proof Ledger      │
                    │  (FM-001b state)    │
                    └─────────────────────┘
```

Two triggers may reach the regulator:

1. **Primary**: availability / completion signal from the agent.
2. **Secondary**: low-rate audit timer or watchdog probe when the normal loop
   appears broken.

## Components

### FM Regulator (`start-fm-conductor!`)

This is the sole assignment mechanism for FM work. Despite the current symbol
name, it behaves more like a regulator/scheduler than an orchestral conductor.
It replaces the LLM conductor and should eventually own all normal dispatch.

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

### Tickle-1 Agent (stateful coordination memory + IRC conversational shell)

Tickle-1 remains useful because it can keep state about recent coordination:
who asked for work, who said "back off", who was just paged, and what repair
attempts have already happened. It also responds to direct `@tickle` mentions
from humans on IRC.

In the intended end-state, it does NOT own normal assignment. It does:

- record / remember coordination context
- accept availability bells from agents
- mediate backoff / "I'm already working on it" style signals
- participate in repair when the normal loop goes silent

It does NOT:

- decide FM assignment policy by LLM prompt
- replace the FM regulator as the assignment source

If a human asks `@tickle status?` on IRC, tickle-1 responds. That's all.

### Availability Bells / Post-Invoke Hook (`!post-invoke-hook`)

Set by `start-fm-conductor!`, cleared by `stop-fm-conductor!`.

When any agent finishes work, the normal path should be:

1. the agent signals `"I'm available"`
2. Tickle/regulator records that availability
3. the FM regulator checks idle status + available obligations and assigns if appropriate

Today the existing post-invoke hook is the closest implementation of that
model. It is the right seam to keep.

This replaces `bell-tickle-available!` invoking tickle-1's Haiku, which caused
feedback loops (codex output contained `@tickle` → tickle invoke → more IRC
noise → codex sees noise → ...).

### Watchdog (audit / repair only)

The watchdog is not meant to be the primary dispatcher.

Its role is:

- detect silence or contradictory state
- ask "are you OK?" after a context-sensitive threshold
- initiate a bounded repair sequence or escalate if the pull loop appears broken

This means watchdog timing has to be interpreted in context:
- an agent actively invoking is different from an idle agent
- an explicit backoff is different from unexplained silence
- a missing availability bell is different from a crash

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

**T-1: No LLM for dispatch.** The regulator never invokes a language model
to decide whether to page. State + obligations → deterministic action.

**T-2: Pull-first.** Normal work assignment is triggered by agent completion /
availability, not by a periodic polling loop.

**T-3: Workers only.** Mentors (claude-2) and Lab Managers (claude-1) are
never in the regulator rotation. They receive work through different channels
(whistles, direct requests from joe).

**T-4: No feedback loops.** The regulator posts to IRC as "tickle" but never
reads its own output. Agent completion triggers the hook, not an IRC mention
parse. No mechanism exists for conductor output to re-trigger the conductor.

**T-5: Stateful backoff.** If an agent says "back off" or "I'm working on it",
Tickle's coordination state must remember that and suppress redundant probing or
reassignment for an appropriate window.

**T-6: Registry is truth.** Agent idle/invoking status comes from the agency
registry, not from evidence timestamps or IRC message parsing. The registry
is updated by the invoke machinery in real time.

**T-7: Watchdog is audit-only.** The watchdog may probe, repair, or escalate
when the normal pull loop fails, but it does not become a second primary
assignment mechanism.

## Outstanding Issues

The mechanical-conductor direction is still correct, but the implementation is
not fully converged yet.

1. `futon3c.dev` decomposition is still in progress.
   The startup/orchestration code now delegates into extracted `dev.*`
   namespaces, but there are still compatibility wrappers in
   `dev/futon3c/dev.clj` and the facade can be simplified further.

2. `tickle_logic.clj` is now evidence-backed, but it is still only a diagnostic
   layer.
   It checks scan/page/escalation ordering, availability-bell context, and
   stall-evidence alignment against registry + evidence snapshots, but it is
   not yet wired into a shared invariant runner or surfaced through
   `check-invariants` / HTTP.

3. The watchdog and the regulator still coexist in code.
   The intended end-state is not "delete watchdog", but "demote watchdog to
   audit/repair." The boot defaults and runtime behavior should reflect that.

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
| `dev/futon3c/dev/fm.clj` | FM regulator / mechanical assignment helpers |
| `src/futon3c/agents/tickle.clj` | Watchdog + availability-bell runtime; emits evidence-backed scan/page/escalation/availability events |
| `src/futon3c/agents/tickle_logic.clj` | Core.logic invariant layer over registry + evidence snapshots for pull-loop + watchdog checks |
| `src/futon3c/agents/tickle_orchestrate.clj` | `fm-assignable-obligations` (used by v2), LLM conductor (v1, not used) |
| `src/futon3c/agency/registry.clj` | Agent status tracking (`:agent/status`, `:agent/invoke-started-at`) |
