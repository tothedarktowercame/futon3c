# Research plan v1 — the memory-transfer programme alongside the frames

*claude-7 (lab manager/analyst), 2026-08-17, on the operator's direction:
"brush up the V3 whitepaper and supporting notes into a research plan
that we'll run alongside the agentic problem solving work." Companion to
`retrieval-whitepaper-v3.md`; supersedes nothing — this is the forward
plan the whitepaper's next sections will be written FROM.*

## 0. Where the whitepaper stands, in one paragraph

V3's thesis (§1.2) is that an agent memory system must be audited by its
own users, with silence catalogued (§2), witness standards enforced (§3),
and retrieval characterized by what its users actually needed (§4). Its
skeletal sections — the ladder results (§5) and terms-and-fit (§7) — were
waiting on exactly what now exists: a cycle machine that runs end to end
(W.31–W.60), instruments that measure transfer directly, and a per-frame
cadence that produces comparable data. §7a's observation (the operator's
corrections as the corpus) has meanwhile been demonstrated at scale:
frames 7–8 produced ~20 machinery defects, every one converted to a
merged fix or a dated queue item within hours.

## 1. The central quantity: the transfer loss function

The operator's formulation: **"are memories increasingly transferring
and being used?"** Made operational:

- **Instrument**: `holes/labs/M-apm-demonstration/analysis/transfer_checks.bb`
  — six checks (C1 attribution, C2 promoted-before-student, C3
  eligibility-includes-promoted, C4 pull-uses-receipted, C5
  projection-completeness, C6 canonical-subject), run per frame against
  saved state + substrate. The per-frame score is the loss function's
  pipeline-health component: it measures whether the MACHINERY lets
  transfer happen and be seen.
- **Baseline**: f7 = 2/6. Predictions: f8 = 5/6 (P13/P16/P17 live), f9 =
  6/6 (P21 activates).
- **Beyond pipeline health** (the checks' ceiling): once the pipeline is
  clean, the loss function's substantive components take over —
  per frame: (a) # memories surfaced to seats (eligible ∪ pulled), (b)
  # with receipted USE (offer-dispositioned or pull-received), (c) # uses
  that co-occur with committed artifacts (the f7-attempt-3 pattern), (d)
  time/attempts-to-first-artifact with vs without memory use. (d) is the
  outcome measure; (a–c) are the mediating chain. The chain is fully
  witnessed as of f8's instruments.

## 2. Data cadence: what every frame yields

Each frame produces, without any analyst intervention:
1. The **trace/envelope** (offers, pull-uses, eligibility provenance,
   guidance events with types, solver-config witness, scribe lanes).
2. The **transfer-checks score** (run at close; known-failing entries
   date which fixes were live).
3. The **guide's final report** (card-mandated honesty; f7's was the
   richest single document of the mission).
4. **Session transcripts** of every seat (supplementary material).
5. **Merged mathematics** on apm-lean main (the merge-back protocol).

The analyst's per-frame product: one entry in a running series —
score, loss-function components, deviations, design findings — appended
to the mission record and rolled into whitepaper sections as they mature.

## 3. Experimental arms (operator-ruled, from the round-1 queue)

1. **Dispatch regime** (W.21/W.29): siege-sustained vs state-based vs
   codex-4's own "siege-once-then-state-based", same stratum, same frozen
   cards, guidance counted by the proctor from the log. The v3 solver
   card runs the hybrid by default; a controlled comparison needs
   registered arms.
2. **Memory condition** for the student lane: empty-shelf baseline (f7,
   banked) vs promote-solver-stocked shelf (f8 onward) vs
   deliberately-seeded shelf (the round-1 maze pattern, now expressible
   legitimately through pre-frame promotion passes).
3. **Store-mode vs harness-mode** rounds (the original design's one
   variable per round; harness-mode untouched by P16 precisely so this
   arm stays clean).
4. **Budget sensitivity**: :reg/student-runner-budget as a pinned
   variable (W.58: progress framing makes partial work measurable, so
   budget-vs-progress curves are now meaningful).

## 4. The case-study pipeline (whitepaper supporting material)

- **CS-1 (drafting now): e-c0a2d2fe** — one memory's journey through
  recorded / unreachable / found-anyway / used / unmeasured, with every
  gap becoming a merged fix; the checks instrument as its epilogue. This
  is §7a's method demonstrated on the memory system itself.
- **CS-2: the fresh-guide orientation property** — two consecutive
  zero-history guides card-perfect on first read (W.39, W.60); the card+
  contract as a complete role transmission; f7-guide's terminal refusal
  as the system's strongest conduct specimen.
- **CS-3: the retro-promotion pass** — propose/review/execute with
  receipts; the reviewer disagreeing with a third of proposals; the gate
  catching the lab manager's own ctx bug (W.43).
- **CS-4 (accumulating): the defect ledger as corpus** — every W-entry
  finding classified (silent-degradation family, attribution family,
  observability family, sequencing family) — the empirical backbone for
  the whitepaper's design-principles section.

## 5. Open design questions the frames should answer

1. **Push-recall relevance**: promoted technique memories didn't match
   a03J04's lexical anchors (W.61 addendum). Is push recall's
   term-matching the right surface for TECHNIQUE transfer, or is
   pattern-endpoint recall (via the attachment taxonomy) the real
   channel? The f8 student's pull behavior is the first evidence.
2. **Taxonomy growth**: when do new pattern files get authored vs
   attached-to-existing (the promotion round's reassign/reject data is
   the signal); does the un-executed math-split manifest matter in
   practice?
3. **Snapshot semantics**: open-time snapshot + witnessed unions (P16)
   vs live queries — where does the next demand for freshness appear?
4. **Cross-problem vs intra-problem transfer** (operator's prior:
   intra dominates): the promoted-8 are cross-problem technique bets;
   f8+ measures whether they ever surface and convert.

## 6. The analyst role (proposed evolution, operator to rule when ready)

Phase 1 (now): the lab manager runs the bench — checks at close,
per-frame series entry, case studies, design findings to the operator.
Phase 2 (when cadence stabilizes): **analyst as a carded seat** — a
fresh agent per N frames whose card mandates: consume each closed
frame's trace + checks + reports; append one accumulator entry (what
moved, what stalled, what contradicts the thesis); propose (never
dispatch) design packets. Per-turn accumulation with the same
fresh-seat/store-accumulates discipline as every other role — the
analyst's memory IS the store and the mission record, not its context.
Phase 3: the accumulator entries become the whitepaper's ladder-results
section (§5) essentially auto-drafted.

## 7. Immediate next actions

1. Run transfer checks at f8 close; append the second series point.
2. Draft CS-1 (e-c0a2d2fe) as `docs/case-studies/cs1-e-c0a2d2fe.md`.
3. Whitepaper §5 skeleton gets the f7/f8 series stub + instrument
   description.
4. Queue the guide-card v2.2 touch-ups (:mission documentation) for the
   f9 freeze window.

---

## 8. The theory layer (v1.1 addition, operator's correction 2026-08-17):
## the hypergraph Laplacian programme, unstalled

V2's formal spine (whitepaper-v2 §4.3–4.5, related-work: Zhou–Huang–
Schölkopf degree-normalised hypergraph Laplacian, no clique expansion)
ended at a wall: **the deployed memory graph was degenerate** — its
largest component a single hyperedge, λ₂ = 1.0 by construction — and V2
concluded "more dispatching cannot fix this because the corpus grows as
star-forests and is closed under adding memories." λ₂ was validated as a
real structure statistic on the git-history graph (~15 SD below the
configuration-model null) but unaskable on the store itself.

**Attach-then-review changed the graph class.** Promotion creates
memory→pattern attachment edges — cross-links between problem-subject
stars and shared pattern nodes. The corpus is no longer closed under
star-forest growth: every promote-solver/promote pass adds edges of
exactly the kind whose absence made the store spectrally trivial. The
retro-pass alone attached 8 memories across 12 patterns (6 newly
authored); every frame adds more.

**Programme S (spectral series):**
- S1. Per-frame: capture the reviewed-memory hypergraph (typed edges,
  Zhou operator, NO clique expansion) and record component structure +
  λ₂ (normalised and unnormalised) alongside the transfer-checks score.
- S2. Pre-registered predictions: (i) within a small number of frames
  the largest reviewed component contains >1 hyperedge (degeneracy
  broken — a DATE, not a value); (ii) λ₂ departs 1.0 and, per V2's
  inverted-threshold finding, DECREASES as pattern-sharing richness
  grows; (iii) once ≥ some minimal size, the real λ₂ falls below a
  configuration-model null as the git graph did — the store develops
  "real wiring", not just degree sequence.
- S3. Structural sensitivity (V2 §4.3) becomes REPLAYABLE AT DISPATCH
  TIME: the §4.3 caption's own complaint ("without dispatch-time
  snapshots this measures the graph at capture time") is resolved by the
  open-time snapshots + P16 eligibility provenance — arm-removal
  perturbations can now be evaluated against the graph AS THE DISPATCH
  SAW IT.
- S4. Tooling: locate and reuse the V2 spectral tooling (the λ₂/null
  computation used for the git-history result) rather than rebuilding;
  wire it as a per-frame bench step next to transfer_checks.bb.

**Why this is "designing and evaluating systematically", not just making
it work:** the transfer checks (§1) measure whether the pipeline lets
memories move; Programme S measures whether the STORE ITSELF is
developing the connective structure that retrieval theory says it needs.
The two series together — pipeline health and spectral richness — are
the systematic evaluation: if transfer improves while λ₂ stays
degenerate, transfer is running on lexical luck; if λ₂ richens while
transfer stalls, retrieval is failing structure it could use. Either
divergence is a design finding; convergence is the thesis.

## 9. The cascade layer (v1.2, operator's direction 2026-08-17):
## pattern→pattern structure BEFORE multi-pattern attachment

Source: futon5a/essays/moran-1971-agent-cascades — Moran's reading of the
MSC cascade (Alexander et al. 1968) as the control structure of a
knowledge base: patterns as productions (condition / action / BECAUSE),
edges as which-enables-which context dependencies, the whole a
general→specific SEMILATTICE (multiple parents; overlaps are where the
intelligence lives), and the 1971 warning that a thousand-pattern cascade
is useless as a wall chart — it must be queried dynamically against the
current problem state (Moran's AA = retrieval-augmented pattern
application, specified before it was buildable).

**Resolution of the W.67 fragmentation finding.** The reviewer's bespoke
patterns were CORRECT as attachments (high fit) and the store's
fragmentation is real — because the missing structure is not
memory→pattern sharing but PATTERN→PATTERN edges. layer-cake-crossover-
split SPECIALIZES separate-into-independent-pieces; the six formalization
techniques plausibly share upstream parents. As cascade edges these
relations connect the graph without diluting attachment fit. **Order of
work (operator): cascade first; multi-pattern attachment on that ground
afterward** — attachments then travel meaningfully (a memory on a
specific pattern is reachable through its general parents).

**Mechanism (proposed, smallest honest version):**
1. The .flexiarg format grows an explicit upstream field (Alexander's own
   patterns cite their larger-scale contexts) — e.g. `upstream:
   math-informal/separate-into-independent-pieces` — authored in the
   pattern FILE, where the because-clause lives; the multi_watcher
   ingests it as a typed pattern→pattern edge (:specializes /
   :enables). Curation stays editorial-with-receipts, exactly like
   pattern authoring today.
2. Reviewer guidance (scribe v2.2): authoring a bespoke pattern REQUIRES
   naming its upstream(s) — the cascade grows with every promotion
   instead of needing consolidation passes.
3. Retrieval consequence (design question 1 answered structurally):
   technique transfer becomes a CASCADE WALK — match general patterns
   against problem state, descend to specifics carrying attached
   memories — Moran's AA, on our store. Push recall's lexical anchor
   mismatch (W.61) is exactly the wall-chart failure Moran predicted.
4. Programme S extension: λ₂ now measures CASCADE connectivity (pattern-
   pattern edges dominate the spectral structure); prediction S2-i
   restates as "the cascade's largest component grows monotonically with
   promotion passes."
5. The because-clause discipline (Moran's best-aged sentence: transparent
   beats correct; evidence detachable and replaceable) is already our
   witness standard — the cascade makes it navigable.
