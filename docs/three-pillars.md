# The Three Pillars of the Futon Stack

*How a software system learns to maintain itself*

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
