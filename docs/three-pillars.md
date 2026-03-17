# The Three Pillars of the Futon Stack

*How a software system learns to maintain itself*

**Companion documents**: [The Holistic
Argument](https://github.com/tothedarktowercame/futon3/blob/main/holes/holistic-argument.md)
(the full technical argument with support claims, attack relations,
and falsifiability conditions) and its [machine-readable
form](https://github.com/tothedarktowercame/futon3/blob/main/holes/holistic-argument.sexp).

---

## The problem

Most software systems are built by people who hold the design in their
heads. The code does what it's told but has no opinion about whether
what it's told makes sense. When the people leave or forget, the
system drifts — what was intentional becomes accidental, what was
tested becomes assumed, what was principled becomes "that's just how
it works."

The futon stack is an experiment in building software that maintains
its own design rationale. Not artificial intelligence in the popular
sense — it doesn't chat or generate images — but a system that can
answer three questions about itself:

1. **What do we believe this system is?** (The Argument)
2. **What are we sure enough about to enforce?** (The Invariants)
3. **What do we do to make progress?** (The Missions)

These three questions correspond to three documents, three bodies of
code, and three conceptual pillars. Together they form something like
an immune system for a codebase: the ability to detect when something
is going wrong and respond before it becomes a crisis.

---

## Pillar 1: The Argument (what we believe)

Every system has a design philosophy, but it's usually implicit —
scattered across README files, Slack conversations, and one
developer's memory. The Argument makes it explicit.

The Argument is a written document (the "holistic argument sketch")
that says: *here is why this architecture works, here is the evidence
for each claim, and here is what would prove us wrong.* It has five
support claims:

- The evidence discipline works (we have records of what was tried and
  what succeeded)
- The theoretical framing is productive (the same formalism applies at
  multiple scales)
- Patterns transfer between domains (what works in mathematics also
  works in software coordination)
- There is real demand for what this produces
- The reflexive architecture is rare and valuable

Each claim has evidence — not vague assertions, but specific counts,
file paths, and test results. And each claim has attack relations:
named objections that the argument must survive. The Argument doesn't
claim to be right; it claims to be *falsifiable* and *currently
surviving its own objections*.

**In Active Inference terms**, the Argument is the *generative model*:
the system's beliefs about what it is and how it works. When reality
diverges from these beliefs, that divergence is a signal — something
needs attention.

---

## Pillar 2: The Invariants (what we're sure of)

Not everything in the Argument is equally certain. Some parts have
been tested thousands of times and have never failed. Other parts are
educated guesses that seem right but haven't been stressed. The
Invariants track this difference.

The system maintains a structural law inventory: a machine-readable
catalogue of every discovered constraint, classified by how confident
we are in it. There are two tiers:

**Operational invariants** (nine families) are laws that are always
enforced in running code. For example:

- *Phase ordering*: states advance in a valid sequence (you can't skip
  from "planning" to "done" without executing)
- *Required outputs*: each phase must produce declared artifacts (you
  can't claim you built something without the files)
- *Dependency satisfaction*: completed work must be backed by its
  prerequisites (you can't mark a task done if its inputs aren't ready)
- *Graph symmetry*: if A depends on B, B knows about A (no invisible
  dependencies)

**Candidate invariants** (eight families) are patterns that keep
appearing as pressures but haven't yet been promoted to law. For
example:

- *Failure locality*: when something breaks, the error surfaces near
  where it happened (not three layers away)
- *Budgeted action selection*: choosing what to do next should
  consider cost, not just priority
- *Human-visible inspectability*: the system's state should be
  readable without tribal knowledge

The metaphor is **crystallization**. Code starts liquid — it works,
but its structure is implicit. Over time, conventions harden into law:
first as patterns people follow, then as tests that enforce them, then
as structural constraints the system checks automatically. The
crystallization narrative tracks what's solid, what's setting, and
what's still liquid.

**In Active Inference terms**, the Invariants are *precision-weighted
priors*. High precision (operational invariant) means the system is
very confident this constraint holds and will refuse to violate it.
Low precision (candidate) means the system suspects this constraint
matters but isn't certain enough to enforce it yet. Precision
increases as evidence accumulates — the same way a scientist becomes
more confident in a result after many replications.

---

## Pillar 3: The Missions (what we do)

Having beliefs (the Argument) and constraints (the Invariants) isn't
enough. You also need a way of working that produces evidence,
respects constraints, and advances the beliefs. The Missions are that
way of working.

A futonic mission is a structured process with seven phases:

1. **IDENTIFY** — Survey the territory. What exists? What's been tried?
   What's the gap?
2. **MAP** — Cross-reference with theory and prior work. What patterns
   apply? What has already been solved elsewhere?
3. **DERIVE** — Make explicit decisions. For each design choice:
   IF (the situation), HOWEVER (the tension), THEN (the decision),
   BECAUSE (the reasoning). Every decision is numbered and grounded.
4. **ARGUE** — Weave the decisions into a coherent argument. This is
   where the individual choices become a design.
5. **VERIFY** — Check the design against its own claims. Do the
   structural laws permit it? Does the Argument survive it?
6. **INSTANTIATE** — Build it. If this phase requires novel decisions,
   earlier phases were incomplete.
7. **DOCUMENT** — Make what was built discoverable by someone who
   doesn't know the mission exists.

Each phase is a different kind of uncertainty reduction:

- IDENTIFY reduces uncertainty about *what* to do
- MAP reduces uncertainty about *what's already known*
- DERIVE reduces uncertainty about *how* to do it
- ARGUE reduces uncertainty about *whether it's coherent*
- VERIFY reduces uncertainty about *whether it's correct*
- INSTANTIATE reduces uncertainty about *whether it works*
- DOCUMENT reduces uncertainty about *whether others can find it*

The process produces evidence at every step: pattern selection records
(which pattern was chosen and why), pattern use records (whether it
worked), and post-action reviews (what was learned). This evidence
feeds back into the Argument and the Invariants — successful patterns
increase confidence, failures trigger revision.

**In Active Inference terms**, the Missions are the *action policy*:
the behavioural repertoire an agent uses to minimize uncertainty. Each
mission phase is a different action type, and the sequence is designed
so that each phase reduces the uncertainty that the next phase needs
resolved.

---

## How the three pillars connect

The pillars aren't independent. They form a loop:

```
    The Argument
    (what we believe)
         │
         │  The Argument says what to observe.
         │  Observations update or challenge the Argument.
         ▼
    The Invariants ◄────────────────────────────────────┐
    (what we're sure of)                                │
         │                                              │
         │  The Invariants constrain what                │
         │  actions are permitted.                       │
         ▼                                              │
    The Missions                                        │
    (what we do)                                        │
         │                                              │
         │  Missions produce evidence.                  │
         │  Evidence feeds back into beliefs             │
         │  and crystallizes new invariants.             │
         └──────────────────────────────────────────────┘
```

A concrete example from recent work (M-aif-head, March 2026):

- The **Argument** claimed that every peripheral (workspace where
  agents operate) should have an AIF head — a sensory-motor interface
  that lets it observe its environment, check constraints, and decide
  what to do between tasks.
- The **Invariants** specified which structural laws the AIF head must
  consult: phase ordering, required outputs, dependency satisfaction,
  and nine others.
- The **Mission** walked through all seven phases to design, verify,
  and build the implementation: 9 handoff tasks, 1,072 lines of code,
  6 documentation entries.

The mission's evidence now feeds back: the Argument is strengthened
(the AIF framing produced working code), new invariants crystallized
(the coverage invariant: "every peripheral has an AIF head"), and the
methodology itself was validated (the mission lifecycle was verified
against its own framework).

---

## What this means in practice

The futon stack is not a product. It's an investigation into whether
software systems can be built that maintain their own coherence — not
through artificial general intelligence, but through the simpler
mechanism of *keeping track of what you believe, what you're sure of,
and what you're doing about it*.

The three pillars are the minimum viable version of this idea:

- A **written argument** that can be challenged and updated
- A **catalogue of constraints** that distinguishes certainty from
  speculation
- A **structured process** that produces evidence at every step

These are not exotic concepts. Scientists maintain hypotheses
(arguments), distinguish established results from conjectures
(invariants), and follow methodologies that produce replicable evidence
(missions). The novelty is applying this discipline to a software
system and connecting the three so that the system can, to a limited
but real degree, participate in its own maintenance.

The AIF (Active Inference) framing gives mathematical precision to
these ideas — generative models, precision-weighted priors, free energy
minimization — but the ideas themselves are older than the mathematics.
They are, at root, about building things that know what they are.

---

## Appendix: This isn't just theory

The three-pillar structure described above isn't a plan — it's
running. The futon stack currently tracks 84 missions across 10
repositories. Here's what that looks like in practice.

### ~30 completed missions form the platform

These are finished: designed through all seven phases, implemented,
documented, and producing evidence. They include:

- **The peripheral stack** — the workspace model where agents operate.
  Six missions built it layer by layer: from the abstract peripheral
  model, through dispatch bridging, phenomenology (how peripherals
  appear to agents), the gauntlet (capability envelopes), transport
  adapters, and IRC stability.

- **Mission infrastructure** — the mission peripheral itself (where
  missions are tracked), mission control (the operational dashboard),
  and portfolio inference (the slow-timescale AIF loop that recommends
  what to work on next).

- **Coordination** — pattern selection records, walkie-talkie
  (agent-to-agent messaging), Codex agent behaviour, overnight
  automation, and the coordination rewrite that unified the gate
  pipeline.

- **Evidence and self-representation** — the evidence viewer, the
  self-representing stack (where the system can inspect its own
  structure), and the three-column stack (code, evidence, and
  documentation side by side).

Each completed mission left behind not just working code but a
permanent evidence trail: what patterns were selected, whether they
worked, and what was learned. This trail feeds back into the Argument
and the Invariants.

### ~10 missions are in progress, building out the thesis

The current growth edge includes:

- **M-structural-law** — extracting the structural law inventory from
  documentation into enforceable code. This is the Invariants pillar
  becoming self-enforcing.
- **M-cyder** — cybernetic operations, the economics layer that
  manages resource budgets across missions.
- **M-stepper-calibration** — tuning the cycle engine that drives
  mission phases.
- **M-tpg-coupling-evolution** and **M-xor-coupling-probe** — formal
  methods work in futon5, investigating how components couple and
  decouple.
- **M-distributed-frontiermath** and **M-artificial-stack-exchange** —
  knowledge infrastructure in futon6.

These aren't isolated features. Each one is building out a specific
aspect of the three-pillar architecture: making the Argument more
precise, crystallizing new Invariants, or improving the Mission
process itself.

### Working this way is fast, not slow

A natural worry is that seven-phase missions with evidence trails and
structural law checking must be enormously slow. In practice, the
opposite is true. The structure *removes* overhead by eliminating the
most common time sinks in software development:

- **No "what should I build?" paralysis** — IDENTIFY and MAP answer
  this before any code is written.
- **No design arguments mid-implementation** — DERIVE and ARGUE
  resolve design decisions before INSTANTIATE begins.
- **No "does this actually work?" anxiety** — VERIFY catches structural
  problems before code exists.
- **No lost context** — every decision is recorded, so returning to a
  mission after weeks away takes minutes, not hours.

A recent example: **M-aif-head** went from conception to completion
(all seven phases plus a pre-instantiation review) in a single
evening session — about two hours. That mission produced:

- 9 handoff tasks designed and implemented
- 1,072 lines of code across 8 files in two repositories
- 6 documentation entries in the docbook
- A reusable protocol (AifHead) that every future peripheral will
  implement
- A complete evidence trail, including the mission's own
  self-verification against its AIF framework

The full mission document is public:
[M-aif-head](https://github.com/tothedarktowercame/futon2/blob/main/holes/M-aif-head.md)

### The landscape at a glance

```
SOLIDIFIED — the platform stands on these         ████████████████████  ~30
  Peripheral stack, mission infrastructure,
  coordination, evidence, self-representation

CURING — current print layer                      ████████░░░░░░░░░░░░  ~10
  Structural law, cybernetics, calibration,
  coupling probes, knowledge infrastructure

POWDER BED — ready to solidify next               ████░░░░░░░░░░░░░░░░   ~6
  Autonomous pattern lifecycle, last-mile
  porting, sliding blackboard

SUPERSEDED — absorbed into current repos                                 ~8
  Original futon3 missions, now ported

GRAY ZONE — pre-split, needs triage                                     ~26
  Mostly futon3 missions awaiting review
                                                  ─────────────────────
                                                  Total: ~84 missions
```

This is a 3D printing metaphor: completed missions are the solid
platform, in-progress missions are the layer currently being printed,
and ready missions are the powder waiting to be fused. The platform
grows one layer at a time, and each layer is structurally sound before
the next begins — because the Invariants enforce it.
