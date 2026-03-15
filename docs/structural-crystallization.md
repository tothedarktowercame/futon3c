# How the Stack Becomes Law

*A narrative complement to the holistic-argument-sketch.
Where that document describes what the FUTON stack **is**, this one
describes how it is **coming into being** — specifically, how running
code becomes structural law through progressive crystallization.*

*The machine-readable companion is `docs/structural-law-inventory.sexp`.*

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
it can be marked proved, but some of this is validated by eyeball, not
by machine.

The difference between convention and constraint is the difference
between a liquid and a crystal. Liquid code flows and adapts but has no
structural memory. Crystallized code has hard edges — invariants that
hold by construction and break loudly when violated.

**M-structural-law** is the mission that manages this crystallization.
This document narrates what's crystallized so far, what's still liquid,
and how the phase boundary is advancing.

---

## The Inventory

The `structural-law-inventory.sexp` is the canonical registry seed. It
classifies every discovered invariant along five dimensions:

| Dimension | Values |
|-----------|--------|
| **Scope** | local · repo · cross-repo · stack |
| **Status** | operational · operational-but-bypassable · operational-when-enabled · candidate · violated |
| **Kind** | foundational · gate · mission-law · reporting-law · process-law · structural-law |
| **Home** | repo · mission · peripheral · subsystem |
| **Evidence** | implemented-in, enforced-at, evidenced-by |

This classification matters because not everything called "invariant"
is the same kind of thing. A foundational invariant that rejects bad
writes at the storage layer is categorically different from a gate
check you can disable in configuration, or a candidate invariant that
shows up as a recurring pressure across cleanup passes but has no
enforcement code yet.

---

## Layer 0: The Code (Liquid)

Everything starts as code. Files get written, functions get called,
tests pass. This is the liquid layer — it works, but its structural
properties are implicit.

At this layer, the stack already has substantial substance. But tests
verify *behavior*, not *structure*. A test says "this function returns
the right value." An invariant says "this *relationship* holds across
the entire system at all times."

---

## Layer 1: Operational Invariants (Hard Crystal)

Nine families of invariant are currently operational — enforced in
running code, breaking loudly when violated:

### The Original Six (futon3c core.logic layers)

These were discovered by writing `core.logic` projections over live
system state. Each follows the same pattern:
`snapshot → build-db → goals → query-violations`.

1. **Graph symmetry** — if A points to B, the inverse structural
   relation should be present. Checked across portfolio, tickle, agency,
   and proof domains. The entry/exit asymmetry violations (V-1 through
   V-5) are instances of this family — they're the crystal's growth
   edge, not its failure.

2. **Status discipline** — state labels must be legal and
   evidence-consistent. An obligation can't be marked `:done` with only
   assertion evidence. An agent can't be `:invoking` and `:idle`
   simultaneously. Enforced in tickle, agency, proof, and mission
   backends.

3. **Phase ordering** — phases advance in a valid, declared order. You
   can't skip from `:observe` to `:completed` without traversing the
   intermediate phases. Enforced in tickle escalation, proof
   obligations, and mission cycles (observe → propose → execute →
   validate → classify → integrate → commit → gate-review → completed).

4. **Required outputs** — each phase must produce its declared artifacts
   before advancing. A proof phase needs its outputs. A mission phase
   needs its deliverables. You can't just wave through.

5. **Existence** — referenced entities must actually exist. Hop targets
   must be real peripherals. Proof dependencies must be real obligations.
   Mission blockers must be real items.

6. **Dependency satisfaction** — completed things must be backed by
   completed prerequisites. You can't mark a proof obligation "proved"
   if its dependencies aren't proved. Portfolio policy needs perception
   before it can select actions.

### The Recovered Three (futon1a substrate)

These were discovered not by writing new logic layers but by auditing
futon1a's existing enforcement. They were always there — they just
hadn't been named as invariant families:

7. **Startup contracts** — system startup requires explicit policy
   (e.g., allowed penholders) and fails loudly when the policy is
   underspecified. You can't just start the system and hope. Enforced
   at `futon1a.system/start!`.

8. **Layered error hierarchy** — failures surface at the layer that
   caused them, with stable status and context. Writes run in strict
   L4→L3→L2→L1→L0 order; lower layers don't run when higher layers
   fail. You can't get a confusing error from the wrong layer. Enforced
   in `futon1a.core.pipeline`.

9. **Authorization and identity discipline** — writes require an
   allowed penholder, tooling identities must pass an explicit allowlist,
   and external identity mappings are canonicalized. Conflicting
   source/external-id pairs are rejected. Enforced in
   `futon1a.auth.penholder` and `futon1a.core.identity`.

---

## Layer 1b: Gate Surfaces (Conditional Crystal)

Some invariants are real but not always-on. They're gates — checks
that become active under explicit configuration:

- **futon4 window constraints** — Arxana browser validates window
  layout, focal management, and data shapes using Reazon relations.
  These are genuine structural checks, but they're configurable. You
  can run Arxana without them.

- **futon3b gate pipeline** — all coordinated task work traverses
  G5→G0 in order, and the first failing gate determines the result.
  But ordinary live entry points (Emacs agent-chat, IRC) can still
  bypass the gate boundary entirely.

The distinction matters: a gate is evidence of useful structural
discipline. It's not (yet) foundational law. The inventory marks these
as `:operational-when-enabled` or `:operational-but-bypassable` to
keep this difference visible.

---

## Layer 2: Candidate Invariants (Soft Crystal)

Eight families of invariant recur across the stack as pressures —
things that break when violated, patterns that show up in cleanup
passes and debugging sessions — but don't yet have enforcement code:

1. **Atomic inspectable units** — work should happen in bounded,
   inspectable units with clear custody. Every work item should have
   one canonical repo. Active work should start from an explicit
   checkout. Endings should be committed, parked, abandoned-with-reason,
   or escalated — not left floating. *Evidence*: the 2026-03 cleanup
   pass found mixed work across repos, unowned files, unbounded stashes.

2. **Artifact custody** — outputs should land where the stack expects
   them. Derived artifacts belong in the canonical repo. Scratch is
   marked as scratch. Generated files don't masquerade as source.
   *Evidence*: FM-001 solver code landed in futon5 instead of futon6.

3. **Repo role clarity** — a repo should say what kind of thing it is.
   Top-level layout should match the declared role. Main branch
   shouldn't mix canon, experiments, generated output, and runtime
   clutter. *Evidence*: futon3 root was cluttered; futon1a README
   conflated active substrate with aspirational devmap.

4. **Archaeology control** — latent work shouldn't accumulate as
   invisible debt. Stash count stays bounded. Old stashes get
   recovered, parked, or dropped. *Evidence*: futon3c had heavy
   peripheral stashes; futon4 had lingering Arxana stashes.

5. **Peripheral custody** — peripheral sessions should carry enough
   structure that work can't silently drift across domains. Each
   session should carry a domain ID. Hops should preserve mission
   identity. Exits should produce fruit. *Evidence*: mission and proof
   peripherals already show this shape, but it's not first-class.

6. **Human-visible inspectability** — state should be visible enough
   that the operator can tell what's going on without folklore. Key
   outputs should be inspectable in human-readable form. State should
   project to evidence or blackboards. *Evidence*: typed proof-path
   boundaries (futon3b) and mission snapshots (futon3c) are firm
   exemplars; much of the rest is still folklore-dependent.

7. **Failure locality** — failures should surface near the layer that
   caused them. Errors should report the right layer. Higher-layer
   failures should prevent lower-layer mutation. Defects should be
   localizable without archaeology. *Evidence*: futon1a's layered
   pipeline is the strong exemplar; futon6's first-proof
   retrospectives surfaced falsification-before-commitment as a
   mathematical variant.

8. **Budgeted action selection** — action selection should be
   constrained by available budget, not priority alone. No costly
   action without budget. Spend must be recorded. Depletion forces
   deferral. *Evidence*: discussed and partially sketched; no fully
   operational exemplar yet.

---

## Layer 3: Meta-Invariants (Emerging Crystal)

Looking across the operational and candidate families, the structural
law inventory maps each family to its known domains and identifies
gaps. The Handoff 1.2 working set (lines 635–722 of the sexp) tracks
this explicitly:

| Family | Domains | Firm exemplar | Gap |
|--------|---------|---------------|-----|
| Graph symmetry | portfolio, tickle, agency, proof | proof_logic.clj | Mission lacks logic-layer projection |
| Status discipline | tickle, agency, proof, mission, futon1a | mission_backend.clj | Mission logic file not yet extracted |
| Phase ordering | tickle, proof, mission | mission_backend.clj | Logic-layer form is next step |
| Required outputs | proof, mission, futon1a | mission_shapes.clj | Shared combinator layer doesn't exist |
| Existence | agency, proof, mission, futon1a | mission_backend.clj | Not full dependency surface yet |
| Dependency satisfaction | portfolio, proof | proof_logic.clj | Mission has DAG but no dep-sat law |
| Startup contracts | futon1a | allowed-penholders | One strong exemplar, no second repo |
| Layered error hierarchy | futon1a | strict-layer-order | Strong, no second repo exemplar |
| Auth/identity discipline | futon1a | penholder-allowlists | Well-evidenced, not mirrored elsewhere |

The candidate families have a parallel watchlist tracking their
strongest exemplars and readiness for promotion.

---

## Layer 4: Structural Law Combinators (Not Yet Built)

The vision: a small set of reusable goals in `logic/structural_law.clj`.
Each domain logic file becomes a thin projection — snapshot the domain's
state, map it onto the universal vocabulary, call the combinators. New
domains crystallize by writing only the projection.

**Status**: Designed in M-structural-law Phase 3 (DERIVE). Not yet
implemented. Pending the mission_logic.clj extraction and meta-invariant
completeness check (Handoffs 1.1 and 1.2).

---

## Layer 5: Violations as Obligations (Not Yet Built)

The crystallization becomes operational when violations drive work:

```
invariant runner → detect violations → classify by actionability
  → :auto-fixable → queue for agent dispatch
  → :needs-review → flag for human
  → :informational → log for awareness
```

This closes the loop: the stack checks itself, discovers structural
issues, and assigns them as work. The conductor doesn't need a manually
seeded task pool — the invariant layers *are* the task source.

**Status**: Not yet built. M-structural-law Phase 4 (ARGUE).

---

## Layer 6: Self-Representing Stack Integration (Not Yet Built)

Structural law violations become navigable in the Arxana hypergraph
(Column 2 of the three-column stack). The system's structural integrity
becomes a first-class, queryable object.

**Status**: Not yet built. M-structural-law Phase 4, Handoff 3.4.

---

## What the Holistic Argument Doesn't Say

The holistic-argument-sketch makes five support claims (S1–S5).
Structural law is the missing sixth:

**S6: The stack checks its own structural integrity.** Nine operational
families enforce invariants across six repos. Eight candidate families
recur as pressures across cleanup passes and debugging sessions.
Violations are cataloged, classified by scope/status/kind, and (soon)
dispatched as work. This is not just reflexion — it's reflexion with
teeth.

S6 is partially instantiated. The operational families run. The
candidate families are named and evidenced. The meta-invariant mapping
is explicit. What remains: combinators (Layer 4), obligation dispatch
(Layer 5), and self-representation (Layer 6).

---

## The Five Questions

The inventory's immediate questions (from the sexp) are the
crystallization's decision points:

1. Should atomic inspectable units become a first-class structural-law
   family? *(Pressure from cleanup evidence says yes; no enforcement
   code yet.)*

2. Should every mission/problem/peripheral session carry a canonical
   home-repo? *(Peripheral custody family; already partly implemented
   in mission/proof peripherals.)*

3. Should cleanup conditions (stash debt, main-branch coherence) become
   structural-law checks? *(Archaeology control family; currently
   folklore.)*

4. Should peripheral stop events normalize into explicit check-in
   outcomes? *(Connected to artifact custody and atomic units.)*

5. Which candidates deserve promotion to foundational always-on
   invariants, and which should remain optional gates? *(The central
   classification question for Phase 2.)*

---

## The Crystallization Timeline

```
                    SOLID                   SETTING              LIQUID
                    ─────                   ───────              ──────
Layer 0 (code)      ████████████████████    ██████████           ████████
Layer 1 (9 families)█████████               ██ (mission logic)   ████
Layer 1b (gates)    ████                    ██                   ████
Layer 2 (8 cands.)  ██ (identified)         ████ (evidenced)     ████████
Layer 3 (meta-map)  ██ (mapped)             ████ (not extracted) ████████
Layer 4 (combin.)                           █ (designed)         ████████
Layer 5 (obligat.)                                               ████████
Layer 6 (self-rep)                                               ████████

Time →              ← done                  ← in progress        ← future →
```

The base is wide and solid. Nine operational families across six repos.
The growth edge is M-structural-law: extracting mission_logic.clj,
verifying meta-invariant completeness, and deciding which of the eight
candidate families have earned promotion.

The story is not "we have structural law." The story is "structural
law is crystallizing, and we can see the crystal growing."
