# Technote: Decomposing `dev/futon3c/dev.clj`

Date: 2026-03-10

## Problem

`dev/futon3c/dev.clj` is now large enough to be a real coordination hazard for
both humans and agents.

At the time of writing it is roughly 4700 lines and mixes:

- environment/config parsing
- runtime atoms exposed through Drawbridge
- invoke evidence emission
- persistent IRC transport state
- Tickle state machine and conductor logic
- FM-001 proof dispatch
- mentor peripheral logic
- work-queue runners
- Claude invoke factory
- Codex invoke factory
- IRC relay / dispatch bridge
- system boot and agent lifecycle

The issue is no longer "the file is long." The issue is that it no longer has a
single coherent responsibility.

## Constraint

We should not break the stable `futon3c.dev` surface casually.

In practice, several workflows depend on:

- Drawbridge expressions that reference `futon3c.dev/*` vars directly
- REPL usage of `futon3c.dev/start-*`, `stop-*`, `restart-*`
- live hot-reload expectations on the current namespace

So the safe decomposition rule is:

1. keep `futon3c.dev` as the stable façade namespace
2. move implementations behind that façade
3. preserve public vars/entrypoints until call sites have been audited

## Current section map

The file already contains rough section boundaries, which are the main
extraction candidates:

- config + deployment helpers
  `env`, `read-admin-token`, `configured-codex-*`, `deployment-role*`

- runtime atoms
  `!f1-sys`, `!f3c-sys`, `!tickle`, `!codex-status`, etc.

- invoke evidence + delivery helpers
  invoke artifact writing, delivery recording

- IRC sender / transport utilities
  persistent ngircd connection, `send-irc!`, bridge send fns

- Tickle state machine + conductor
  task sync, done-signal processing, LLM-backed conductor

- FM-001 conductor
  proof-obligation dispatch and status

- mentor peripheral
  math-channel read/send/evaluate/intervene

- work queues
  CT queue, ArSE queue, other issue runners

- invoke factories
  Claude invoke path, Codex invoke path, IRC Codex path

- dispatch relay + Drawbridge boot

- system boot / lifecycle
  `start-*`, `stop-*`, `start-agents!`, `restart-agents!`, `-main`

## Recommended extraction order

The extraction order should optimize for two things:

1. lowest risk to live operational behavior
2. highest reduction in cognitive load

### Phase 1: pure or near-pure helpers

Move these first:

- config/deployment helpers
- invoke evidence helper functions
- IRC transport helper functions that do not own the top-level atoms

Suggested namespaces:

- `futon3c.dev.config`
- `futon3c.dev.invoke`
- `futon3c.dev.irc`

Why first:

- low dependency density
- easy to test
- minimal Drawbridge surface risk

### Phase 2: peripheral subsystems

Move these next:

- mentor peripheral
- FM-001 conductor
- CT / ArSE queue runners

Suggested namespaces:

- `futon3c.dev.mentor`
- `futon3c.dev.fm`
- `futon3c.dev.ct`
- `futon3c.dev.arse`

Why next:

- these are logically separate domains already
- they contribute significant bulk without needing to own process boot

### Phase 3: Tickle

Extract the Tickle state machine, done-signal handling, and conductor into a
dedicated namespace, but keep the public `start-tickle!` / `stop-tickle!`
wrappers in `futon3c.dev` initially.

Suggested namespace:

- `futon3c.dev.tickle`

Why later:

- higher live-operational risk
- more shared state and more hot-reload sensitivity

### Phase 4: invoke factories and relay machinery

Move the large invoke/relay builders last:

- Claude invoke factory
- Codex invoke factory
- IRC Codex invoke path
- dispatch relay

Suggested namespaces:

- `futon3c.dev.invoke.claude`
- `futon3c.dev.invoke.codex`
- `futon3c.dev.relay`

Why last:

- these are the most operationally sensitive
- they are easiest to destabilize with namespace churn
- they are currently entangled with delivery/evidence/state assumptions

## What should stay in `futon3c.dev`

For now, the façade namespace should keep:

- runtime atoms referenced externally
- top-level lifecycle entrypoints
- boot orchestration
- thin wrappers that delegate into extracted namespaces

This gives us decomposition without sacrificing live reload and Drawbridge
compatibility.

## Concrete first cuts

If we start doing this work, the best initial cuts are:

1. extract config/deployment helpers
2. extract invoke evidence + delivery helpers
3. extract mentor peripheral

That sequence should materially shorten `dev.clj` without touching the most
fragile relay/Codex/Tickle paths first.

## Anti-goal

Do not "solve" the size problem by moving random code until the file is shorter.

That would preserve the confusion while making operational reasoning worse.
The target is not a shorter file by itself; the target is clearer subsystem
ownership with a stable operational façade.

## Current verdict

The concern is valid.

`dev.clj` has crossed the line where it is acting as:

- system bootstrap
- transport layer
- orchestration layer
- agent factory
- live operations console

all at once.

The correct response is staged extraction behind a stable façade, not a
one-shot rewrite.
