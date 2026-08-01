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

1. **Recall pass over the claude-9 session** — dispatched 2026-08-01 to
   claude-9 itself (whose continuity context held the session), against the
   secured transcript
   `holes/labs/M-codex-sorry-loop/claude-9-transcript-20260801.txt`
   (22,402 lines, sha256 `51d7992f…`). Deliverable: recalled signals *not*
   already in the residual's Part E, classified by level, each marked
   verified-against-transcript or reconstruction. Per
   [[feedback_reconstruct_not_reproduce]]: recall without verification is
   reconstruction; the transcript is what makes it checkable.
2. **Measure the level distribution** of what comes back. If L1–L2 dominate
   (predicted), that is the quantitative case for the WM capability.
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
