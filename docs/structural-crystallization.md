# How the Stack Becomes Law

*A narrative complement to the holistic-argument-sketch.
Where that document describes what the FUTON stack **is**, this one
describes how it is **coming into being** — specifically, how running
code becomes structural law through progressive crystallization.*

---

## The Shape of the Problem

The futon stack has a lot of code. Eight repos. 84 missions. 853
patterns. 7,400+ evidence entries. Dozens of processes, peripherals,
bridges. It works — agents coordinate, evidence accrues, missions
complete.

But "works" and "works correctly by construction" are different things.
Most of the stack runs on convention, not constraint. An agent *should*
bell when idle, but nothing *forces* it to. A peripheral *should* accept
hops from any peripheral that lists it as an exit target, but nothing
checks. A proof obligation *should* have its dependencies resolved before
it can be marked proved, but this is validated by eyeball, not by machine.

The difference between convention and constraint is the difference
between a liquid and a crystal. Liquid code flows and adapts but has no
structural memory. Crystallized code has hard edges — invariants that
hold by construction and break loudly when violated.

**M-structural-law** is the mission that manages this crystallization.
This document narrates what's crystallized so far, what's still liquid,
and how the phase boundary is advancing.

---

## Layer 0: The Code (Liquid)

Everything starts as code. Files get written, functions get called,
tests pass. This is the liquid layer — it works, but its structural
properties are implicit. You have to read the code to know what
invariants hold, and you have to trust the developer's discipline to
know they're maintained.

At this layer, the stack already has substantial substance:

- **Agency**: 5 registered agent types, WS + HTTP + local invoke
  routing, session management, capability declarations
- **Peripherals**: 11 peripheral types, hop protocol, entry/exit
  conditions, tool dispatch
- **Forum**: Evidence threads, event streaming, thread projection
- **Drawbridge**: Message routing, IRC relay, peripheral wrappers
- **Coordination**: Bell-driven task queue, conductor dispatch,
  safety-net timers, Tickle escalation
- **Portfolio**: AIF observe → perceive → affect → policy loop
- **Mission Control**: Cross-repo inventory, coverage tracking,
  devmap alignment

None of this has structural law yet. It has tests — 614 of them across
futon3c alone — but tests verify behavior, not structure. A test says
"this function returns the right value." An invariant says "this
*relationship* holds across the entire system at all times."

---

## Layer 1: Domain Invariants (Candidate Crystal)

The first crystallization happened when core.logic invariant layers
were written for specific domains. Each one follows the same pattern:

```
snapshot live state → build logic database → run goals → report violations
```

Five domains now have this treatment:

### 1. Portfolio Logic (`portfolio/logic.clj`)

The predecessor — the first invariant layer written. Checks that
portfolio state (beliefs, observations, policy selections) maintains
internal consistency: precision is non-negative, modes transition
legally, candidate rankings are well-ordered.

**Status**: Operational. The original proof of concept.

### 2. Tickle Logic (`agents/tickle_logic.clj`)

Checks Tickle conductor invariants: escalation chains are backed by
pages (you can't escalate without paging first), pages are backed by
scans (you can't page without scanning first), stall evidence aligns
with scan results.

**Status**: Operational, 0 violations on live system. This domain
crystallized cleanly — the invariants match the implementation.

### 3. Agency Logic (`agency/logic.clj`)

Checks agent registration invariants: no untyped agents, no duplicate
sessions, routing consistency. Also checks peripheral hop topology:
if peripheral A declares `:hop-B` as an exit, does peripheral B's
entry set include `:from-A`?

**Status**: Operational, **5 violations cataloged** (V-1 through V-5).
All five are entry/exit asymmetries on `:explore` and `:edit` — they
accept hops *from* fewer peripherals than can hop *to* them.

These violations are interesting precisely because they're not bugs in
the usual sense. The code works fine — hops succeed at runtime via
`:user-request` overrides. What the invariant layer revealed is a
*design ambiguity*: should `:explore` be the universal entry peripheral
(accept hops from anywhere) or a restricted one (only from `:reflect`
and `:default`)? The invariant layer surfaced the question. The answer
requires architectural judgment.

### 4. Proof Logic (`peripheral/proof_logic.clj`)

Checks proof ledger invariants: DAG acyclicity (no circular
dependencies between obligations), status-evidence consistency (a
"proved" obligation should have its dependencies also proved), phase
output completeness (each proof phase should have its required
artifacts).

**Status**: Operational, tested. Applied to FM-001 and FM-001b ledgers.

### 5. Codex Code Logic (`agents/codex_code_logic.clj`)

Checks invariants specific to Codex agent code submissions: PR
structure, test expectations, scope boundaries.

**Status**: Operational.

---

## Layer 2: Meta-Invariants (Emerging Crystal)

Looking across the five domain layers, a pattern emerges. The same
structural properties appear in different vocabularies:

| Meta-invariant | Tickle | Agency | Proof | Portfolio |
|---|---|---|---|---|
| **Graph symmetry** | escalation ↔ page | hop exit ↔ entry | depends-on ↔ unlocks | — |
| **Status discipline** | scan → page → escalate | idle ↔ invoking | open → partial → proved | BUILD ↔ MAINTAIN ↔ CONSOLIDATE |
| **Phase ordering** | scan before page before escalate | entry before work before exit | observe → ... → completed | observe → perceive → affect → policy |
| **Required outputs** | page needs scan evidence | — | each phase needs outputs | step needs observation |
| **Existence** | scanned agents registered | hop targets exist | deps exist in ledger | candidates in inventory |
| **Dependency satisfaction** | — | — | proved needs proved deps | policy needs perception |

Six meta-invariants, projected differently onto each domain. This is
the structural law of the stack — not the domain-specific rules, but
the *shape* of the rules.

**Status**: Identified but not yet extracted as reusable code. This is
M-structural-law Phase 2 (DERIVE) — factoring these into parameterized
core.logic goals that all domains can call.

---

## Layer 3: Structural Law Combinators (Not Yet Built)

The vision: a small set of reusable goals in `logic/structural_law.clj`:

- `graph-symmetry-checko` — if relation A holds, relation B must hold
- `phase-ordering-checko` — phases must advance in defined order
- `required-outputs-checko` — outputs must exist for each phase
- `existence-checko` — referenced entities must exist
- `dependency-satisfaction-checko` — dependencies must meet threshold

Each domain logic file becomes a thin projection: snapshot the domain's
state, map it onto the universal vocabulary, call the combinators.
New domains (missions, namespaces, evidence threads) crystallize by
writing only the projection, not by reimplementing structural logic.

**Status**: Not yet built. Pending M-structural-law Phase 2 handoffs.

---

## Layer 4: Violations as Obligations (Not Yet Built)

The crystallization becomes operational when violations drive work.
Today, violations are listed in a ledger (`M-invariant-violations.md`).
Tomorrow, they feed the conductor's task queue:

```
invariant runner → detect violations → classify by actionability
  → :auto-fixable → queue for agent dispatch
  → :needs-review → flag for human
  → :informational → log for awareness
```

This closes the loop: the stack checks itself, discovers structural
issues, and assigns them as work. The conductor doesn't need a manually
seeded task pool — the invariant layers *are* the task source.

**Status**: Not yet built. M-structural-law Phase 3 (ARGUE).

---

## Layer 5: Self-Representing Stack Integration (Not Yet Built)

The final crystallization: structural law violations become navigable
in the Arxana hypergraph (Column 2 of the three-column stack). You
can click from a mission to its invariants to its violations to the
code that would fix them. The system's structural integrity becomes
a first-class, queryable object — not just a property of the code,
but a thing you can point at, discuss, and trace.

**Status**: Not yet built. M-structural-law Phase 3, Handoff 3.4.

---

## The Crystallization Timeline

What's solid, what's setting, what's still liquid:

```
                    SOLID                   SETTING              LIQUID
                    ─────                   ───────              ──────
Layer 0 (code)      ████████████████████    ██████████           ████████
Layer 1 (domains)   █████ (5 domains)       ██ (missions next)   ████████
Layer 2 (meta)      ██ (identified)         ████ (not extracted) ████████
Layer 3 (combin.)                           █ (designed)         ████████
Layer 4 (obligations)                                            ████████
Layer 5 (self-rep)                                               ████████

Time →              ← done                  ← in progress        ← future →
```

The base is wide and solid. The crystal is growing from the bottom up.
Each layer depends on the one below: you can't extract meta-invariants
without domain invariants, you can't build combinators without
meta-invariants, you can't drive obligations without combinators.

---

## What the Holistic Argument Doesn't Say

The holistic-argument-sketch (`futon3/holes/holistic-argument-sketch.md`)
makes five support claims for the stack:

- S1: The evidence discipline works
- S2: The Active Inference framing is generative
- S3: Pattern transfer is real
- S4: Commercial demand exists
- S5: The reflexive architecture is rare

What it doesn't address — because the infrastructure didn't exist when
it was written — is the **internal structural integrity** of the stack
itself. S1 says evidence is collected; it doesn't say the collection is
*complete* or *guaranteed*. S2 says AIF operates at three scales; it
doesn't say the scales are *consistent* with each other. S5 says the
reflexive architecture is rare; it doesn't say the reflexion is *sound*.

Structural law is the missing support claim:

**S6: The stack checks its own structural integrity.** Five invariant
domains enforce six meta-invariant classes. Violations are cataloged,
classified, and (soon) dispatched as work. The same structural law
applies to proofs, agents, missions, coordination, and portfolio
management. This is not just reflexion — it's reflexion with teeth.

S6 is partially instantiated. The domain layers exist and run. The
meta-invariant extraction and obligation dispatch are the current
growth edge. When Layer 4 is operational, S6 will be fully grounded:
the stack doesn't just observe itself, it *checks* itself and *fixes*
what it finds.

---

## Relationship to the Mission Landscape

The mission landscape poster shows 84 missions across 10 repos,
grouped by concern and colored by completion status. This document
zooms in on one specific cross-cutting concern: the crystallization
of structural law across those missions.

The missions that participate in crystallization:

| Mission | Layer | Status |
|---------|-------|--------|
| M-fulab-logic | 1 (domain) | Not started |
| M-invariant-violations | 1 (domain) | MAP — 5 violations cataloged |
| M-structural-law | 2–5 (meta through self-rep) | IDENTIFY |
| M-self-representing-stack | 5 (integration) | Complete (infrastructure) |
| M-three-column-stack | 5 (integration) | Complete (framework) |

The completed missions (self-representing-stack, three-column-stack)
provide the *infrastructure* for structural law to be represented.
The active missions (invariant-violations, structural-law) are doing
the actual crystallization work. The not-yet-started mission
(fulab-logic) would add domain layers for peripheral topology and
other coordination invariants.

---

## In Sum

The stack is transitioning from liquid (convention-based) to
crystalline (invariant-enforced). The transition is happening bottom-up:
code first, then domain invariants, then meta-invariants, then
combinators, then obligation dispatch, then self-representation.

Five layers, three currently populated (code, domain invariants,
identified meta-invariants), two under construction (combinators,
obligations). The crystal is about 40% formed. The base is solid.
The growth edge is M-structural-law.

The story is not "we have structural law." The story is "structural
law is crystallizing, and we can see the crystal growing."
