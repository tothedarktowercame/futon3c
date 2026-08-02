# E-learning-across-levels — seed for future War Machine operations

**Opened 2026-08-01 by claude-7, from Joe:** *"'learning across levels' is
something that we would like to build into future War Machine operations.
The memory system is part of this but not all."* This doc names the concept,
grounds it in the evidence we already hold, and stakes out the bounded next
steps. It is a seed for a mission, not a mission.

## 1. The evidence that levels exist and are differently instrumented

`holes/labs/M-codex-sorry-loop/known-residual-20260801.md` Part E is eight
lessons carried out of the loop's biggest session — **every one of them
learned, none of them instrumented**. They were captured only because the
ground-control agent hand-wrote a close-out note. Classified by level:

| level | what learns | Part E instances | where it currently lives |
|---|---|---|---|
| **L0 — object** | routes, lemmas, dead ends | named obligations travel (E5); dead ends recorded save rounds; build-library-detached (E7) | **the memory corpus** — the only instrumented level, and V2 measured how partially |
| **L1 — instrument** | the checks and meters themselves | vacuity ladder grew a third rung + proposed 0d (E2); alarm-shaped verification failures (E3); *sorry-count is the wrong meter, the dependency set has the derivative* (E6) | hand-written notes; a memory only if someone thinks to mint one |
| **L2 — process/topology** | how the agents coordinate | triangle review — the runner leg carried the gate, both ground controls shipped false specs caught by neither reviewer (E1); job-cap/commit-incrementally (residual D1); park-surface trap (D2) | ops registers, CLAUDE.md edits, MEMORY.md — prose, unindexed, unsurfaced |
| **L3 — norm/conative** | what counts as a problem at all | stale absence-prose blocks work (E4); write-side duplication invisible until the norm was authored (E8; staging §I3: *"the norm did not emerge from the encounters; it had to be written"*) | R1–R4 in construction-targets.md; the ⊸ register; nowhere queryable |

**Taxonomy rulings (2026-08-01, from the recall pass's misfits — see
`meta-learning-recall-20260801.md` §5):**

- **Classification is by the level of the LESSON, not the defect** (M2). A
  build fact (L0 defect) whose lesson is "proved-gated-unreachable is worth
  nothing" classifies where the lesson lands.
- **Cross-level edges are first-class** (M1): an L1 instrument fault
  producing an L0 false claim reported to the operator is an *edge*, and
  several of the session's worst moments were edges, not nodes. Record them
  as `Lx→Ly` rather than forcing a node label.
- **The meta-loop needs its own slot** (M3): findings about the apparatus
  used to study learning (e.g. §0 of the recall artifact) are not L1 of the
  loop. Marked `meta` pending a better name — leaving them unplaced would
  repeat the L0-only blindness one level up.
- **L4 — operator/strategic** (M4): operator-steering signals (a throughput
  remark flipping serial→parallel; authorisation gating statement repair;
  "no bells or whistles") are neither agent-authored norms nor inter-agent
  coordination. Kept distinct because WM already has an operator-lane
  concept this should eventually meet.

**Finding — the last gate is judgement, and that locates the residual
(2026-08-02).** Registering E2-pilot against claude-4's CLean format, the
render refused at the seed plan (it hardwired seedable simulation formulas;
E2's runs are non-seedable). The registration *could* have passed by
fabricating three meaningless seed formulas. Four automated gates stood
behind that point — the render gate, the type gate (`ValidatedTrace`), the
malli config gate, the core.logic checker — and none of them would have
caught fabricated-but-well-formed values; the only thing that did was an
agent declining to launder. This is an **L1→meta edge**: the apparatus that
measures whether a mechanism helps has a residual failure mode (a
well-formed lie) whose last line of defence is judgement, not a check. It is
the same class the whole programme is named after — *semantics living where
enforcement isn't* — turned on the enforcement machinery itself. Recorded
because it is not comfortable: the format's safety is real but not total,
and this names exactly where the gap sits. claude-4's fix (parameterise the
replicate index type; make `:seedable? false` *owe* a floor endpoint rather
than be exempted) closes this specific instance by converting the launderable
gate into a proof obligation.

**But the deeper reading (claude-4, 2026-08-02) is that the residual is a
BOUNDARY, not a bug — and locating it exactly beats pretending it closes.**
The fix removes *one* launder (fabricating formulas where a non-seedable
variant should exist). It does not remove the general one: **gates check the
SHAPE of a claim, not its MEANING.** Three well-formed seed formulas are
indistinguishable at the type level from three meaningful ones, because
meaning lives in whether the formula *describes the thing it names* — and a
gate cannot know that food-seed arithmetic is nonsense for a theorem prover.
That semantics is *irreducibly* outside enforcement. So the class
"semantics living where enforcement isn't" has two regimes: where the
semantics *could* move into the type (move it — the general repair) and
where it *structurally cannot* (name the boundary, staff it with judgement,
and do not mistake the gates for theatre — the same gates caught the
non-navigable treatment, the laundered control, the unregistered arm, and a
one-cell perturbation). The finding is not "gates are weak"; it is "gates
verify form, and form is not meaning — the last mile is irreducibly a
judgement, so the apparatus must budget for a judge, not assume one away."
This is the sharpest single statement the programme has of *why* an
L1/instrument layer cannot be fully automated: at the top of every
enforcement stack sits a claim about meaning that only a reader can check.

Two structural observations:

1. **Instrumentation coverage is monotone decreasing in level.** L0 has a
   deployed service with receipts; L1–L3 have prose. This is exactly Joe's
   "we logged meta-learning signals but I don't know that they were ever
   mined" — the signals exist at levels the system has no store for.
2. **The loop's frontier climbed the levels as it matured.** Early residual
   was unproved theorems (L0); the final residual is dominated by statement
   defects (L1: under-strength hypotheses the vacuity checks couldn't see)
   and operator decisions (L3: what should this problem mean?). A system
   that learns only at L0 runs out of things it can learn exactly when the
   interesting lessons start.

## 2. What "built into WM operations" would mean

The memory system solves (partially, per V2) the L0 problem: sensor
(receipts) → store (corpus) → surfacing (recall) → review (witness). The
generalization is that **each level needs its own instance of that loop**,
and they are different in kind, not just in content:

| level | sensor | store | surfacing | review leg |
|---|---|---|---|---|
| L0 | receipts (A1/B2/B5...) | memory corpus | recall at dispatch | Lean witness |
| L1 | instrument-defect receipts (the §7.3 pattern: every defect found was in an instrument) | typed instrument-lesson entries | surfaced when an instrument of that class is *built or trusted* | mutation testing; DarkTower `check_sound` |
| L2 | coordination events (crossed bells, caps hit, park failures) | ops registers → typed | surfaced at *dispatch-protocol* time, not proof time | the ⊸ register's honest violation counts |
| L3 | authored norms + the gaps they retroactively expose | R-rules with provenance ("each traced to the defect that produced it") | surfaced at *review* time | operator |

Note this is NOT a proposal for three new memory stores. The plausible
minimal move is B4's pattern extended one level: `:memory-use/kind` marks
which *channel* an L0 memory acts through; a sibling field could mark which
*level* a lesson addresses, letting one corpus carry all four with
level-appropriate surfacing routes. Whether that works is an empirical
question — V2's lesson is that a field nobody writes measures nothing.

## 3. Connection to the causal integration

Rob's three-level discipline (structural template / execution episode /
inquiry episode — `E-memory-causal-integration.md` §6.3) is the causal-graph
version of the same stack: execution episodes are L0 data; inquiry episodes
are L1 learning *as typed records with predictions attached*; his live-model
synchronization loop is L2 discipline; his gold-target ledger is L3 (the
authored norm the as-built graph is measured against). Their system already
records L1 learning in a queryable form — the 82-row source-obligation
reconciliation is literally an L1 ledger. Worth asking them how much of
that generalizes off the compiler case.

## 4. Bounded next steps (no mission yet)

1. **Recall pass over the claude-9 session** — **DONE 2026-08-01**
   (`meta-learning-recall-20260801.md`, commits `fc32f385`/`00ef0cc7`;
   reviewed by claude-7, three anchors independently re-verified). 27
   signals, none overlapping Part E; **zero pure reconstructions** — every
   signal either transcript-anchored (20) or verifiable in a named
   committed file (7 `NOT-IN-ARTIFACT`).
   *Instrument caveat, discovered by the pass itself:* the secured
   transcript is **surface-partial** — emacs-repl only, no bell surface —
   so it is systematically blind to inter-agent coordination, *which is
   where L2 lives*. Six of the seven not-in-artifact signals are L2/L3.
   The instrument secured to measure the level distribution under-samples
   the level predicted to dominate — V2 §1.1's thesis, reproduced in the
   meta-loop on its first outing. (Second instrument finding: recall
   failed at the *query* — searching `137 of 145` for text that reads
   `137/145` — one rung below where reconstruct≠reproduce usually bites.)
2. **Measure the level distribution** — **DONE, prediction confirmed as a
   floor**: L1 (11) + L2 (7) = **18 of 27 = 67%**, from an instrument that
   under-samples L2 by construction. The quantitative case for the WM
   capability is made; the honest number is "at least two-thirds."
3. **Fold into the V3 cohort**: the cohort's receipts will capture L0. A
   cheap L1 addition — a `:defect-location {instrument|data}` field on
   experiment review findings — would start the L1 ledger for free, since
   §7.3's finding (fourteen review passes, every defect in an instrument)
   suggests the field's distribution is heavily skewed and therefore
   informative.
4. **Scope gate:** anything beyond this — new stores, WM wiring, a mission
   doc — waits for Joe.

---

*Cross-references: `known-residual-20260801.md` (Parts D, E),
`E-memory-resourcing-and-strategy.md` §6–7, `E-memory-v3-staging.md` §G
(conative structure), §I3 (norm-before-recurrence),
`E-memory-causal-integration.md` §6.3, V2 §7 (instruments as the subject).*
