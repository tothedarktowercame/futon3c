# M-zai-learning-loop — a laptop-based, self-improving learning loop in mathematics

**Status:** CHARTERED 2026-07-25 (Joe's direction; this doc claude-4).
**Parents:** M-typed-memories (the gap, the prototype ladder),
M-shared-memory-control-build-test (recall/curation/receipts machinery),
E-dynamic-queries + E-retrieval-flows (the dynamical retrieval theory),
Zaif (the runner formalism this loop completes with a memory axis).

## HEAD (the gap this closes)

M-typed-memories names the gap: agents do the work but the *learning* is not
captured, stored, or fed back. This mission exists to produce **real
evidence** of two claims, in mathematics, on a laptop:

1. **A learning loop runs**: a runner does mathematics; a scribe distills its
   learning into typed, subject-attached memories; retrieval feeds those
   memories into later work; receipts and challenges evaluate them. Every
   stage store-native, witnessed where possible.
2. **The loop is self-improving**: new patterns — and new *levels* of
   patterns — accrete in the store (patterns exist at any abstraction level),
   and this accretion shows up later as **improved mathematical capability on
   held-out problems**.

The second claim gets teeth from a held-out set (below). The first claim gets
teeth from the four-level observables (below). Neither claim is allowed to
lean on the other: a cohort can demonstrate the loop runs while capability is
flat, and must say so.

## The loop (a reduced War Machine loop)

| Role | Agent | Analogue | Notes |
|---|---|---|---|
| Opportunity | one problem-attempt session (apm-lean) | click | durée-triggered by ground control in v0 |
| Recall at dispatch | pattern-conditioned recall injected into the runner prompt | selection | P1 recall machinery; neighborhood projection, not just leaf rules |
| Runner | zai (zai-2 proposed) | author | works the problem; own `memory_record` calls are `self-asserted` |
| Scribe | codex (codex-2 proposed) | reviewer | v0 batch, per-session: mines the session's turn-round evidence, distills scoped rewrite rules, records them with evidence-id provenance; the memory author is distinct from the acting agent by construction |
| Mechanical witness | `lake env lean` exit code + sorry count + commit sha | build gate | free, incorruptible |
| Delivery QA | operator reviews new rules/patterns (the six-rule review is the template) | Field Desk QA | |
| Evaluation | use-receipts, challenge edges, the four-level observables | Phase 6 outcome model | feeds θ (E-dynamic-queries Ψ) when Rung 4 lands |

Separation of powers: the scribe authors memories *about* the runner's work —
witnessable distillation — while the runner's own realizations stay
self-asserted. Promotion of any memory to `:reviewed` remains an operator or
independent-reviewer act (M-shared-memory Phase 3 discipline).

**Corpus discipline**: the substrate is the Evidence Landscape, not buffer
scrapes — zai turn-rounds persist as `:coordination`/`:turn-round` entries
(verified 2026-07-25: coverage back to 07-18 across zai-3/4/5, dense for
zai-1). Rules cite turn-round evidence ids in `:rule/evidence`.

## Observables (four levels, each honestly labeled)

1. **Leaf** (witnessed when it occurs; NOT required for success): a recalled
   rule cited in a use-receipt with the trigger class resolving faster.
   Absence is not failure — dynamical memory can be useful without ever
   reaching leaf nodes.
2. **Pattern** (witnessed-structural): "retrieval anticipated the lesson" —
   the scribe files a session's new rule under a pattern that recall had
   already activated at dispatch. Diffuse-influence variants (behavior aligns
   with surfaced pattern without citation) are **exploratory only**, never
   promotion evidence.
3. **Field** (exploratory at small n): error-rounds-per-trigger-class across
   sessions; triage-ladder adoption; sorry burndown shape. Problem difficulty
   is heterogeneous; no trend claim without saying so.
4. **Structure-formation** (witnessed events): pattern-formation as
   first-class records — two single-instance rules merging into an n≥2
   pattern; a new pattern *level* appearing (pattern-of-patterns); challenge
   edges landing on a misleading rule. The graph growing the connectivity
   that makes Δ_θ non-degenerate (E-retrieval-flows: scribe output is
   operator food) is itself a success dimension.

## Held-out capability evaluation (the self-improvement meter)

**Berkeley Problems in Mathematics (BPM) is the held-out set.** The APM
corpus contains it; the loop trains (accretes memory) on non-BPM problem
sessions; capability is then measured on BPM attempts as:

> **more problems solved with zero sorries, in fewer turns** — both counted
> from the Evidence Landscape: solved = final turn-round summary with Exit 0
> / 0 sorries / commit sha (mechanically witnessed); turns = count of
> turn-round entries for that problem's sessions.

Design rules, preregistered per evaluation cohort:
- BPM problems are never mined by the scribe and never appear as
  `:mined-from` subjects — held-out means held out of *memory formation*,
  not just of tuning.
- Attempts are counted in dispatch order with no relabeling or replacement
  (the WM cohort counting discipline verbatim).
- The comparison is capability-over-accumulation: BPM attempt batches
  interleaved at intervals as the memory graph grows (batch 0 = near-empty
  store baseline; batch k = after k training cohorts). Small n; trends
  reported with their n, never laundered.
- CPM, DPM, and sibling collections, when acquired, become successive
  held-out sets — each new set is pristine exactly once.
- The indexed PDF is the **2nd edition** (exams through Sp98, 741
  problems). The 3rd edition (2004) extends past Sp98; its increment over
  the 2nd-edition index is a naturally delimited fresh tranche when
  acquired — treat the edition diff as its own pristine set.

Baseline already in the landscape (2026-07-25): a95A07 complete (0 sorries,
commit 1d622b0); a95A04 partial (symmetric_split proven, 2 sorries);
turn-round counts recoverable per problem.

### The memorization contrast (Joe, 2026-07-25)

BPM is a published book (Fa77–Sp98 exams) and plausibly in the runner
model's training data; APM problems probably are not. Neither containment
status is certain, and the charter treats both as unknown. Consequences:

- **BPM absolute performance is confounded by pretraining recall** — in both
  directions: memorization can inflate the baseline, and can also leave less
  headroom for memory-driven improvement.
- **The cleaner learning signal is the differential**: as the store accretes,
  compare the improvement *slope* on probably-novel APM problems against the
  slope on possibly-memorized BPM problems. Memory-driven capability should
  show more strongly where pretraining recall helps least. Beating the
  track record on APM specifically is the interesting result.
- Both slopes are reported with the containment caveat stated; no claim of
  "novel problems" without the "probably" attached.

### BPM corpus status (2026-07-25)

`~/Downloads/berkeley-problems-in-mathematics.pdf` (440pp, 2001-era Acrobat
scan **with an OCR text layer**). The text layer reliably yields structure —
**741 unique problems indexed** (`holes/labs/M-zai-learning-loop/bpm-index.json`:
id, chapter, section, exam season; chapters 1–7 = real analysis 140,
multivariable 51, ODEs 56, metric spaces 34, complex 166, algebra 147,
linear algebra 148) — but math notation is badly garbled (`R + R` for
`ℝ → ℝ`), so faithful problem statements need a second pass:
vision-capable transcription over page images, with the garbled layer as a
cross-check. (Noting the irony: memorization of the published text *helps*
transcription while confounding evaluation — acceptable, since corpus
fidelity is checked against the page image, not trusted from the model.)
Formalization into Lean stubs is a third, quarantined step: the formalizing
agent's session is excluded from memory formation like any BPM contact.

## Preregistration discipline

Each training or evaluation cohort preregisters: target session count,
casting, counting rule (all typed outcomes in the denominator), allowed
triggers, and the observable levels it will report. Typed failure kinds
extend the WM set with at least: `recall-empty`, `rule-misleading`
(recalled memory made the arc worse — a challengeable outcome, and the
organic form of Rung 4's confirmation-collapse decoy), `scribe-lag`,
`memory-write-rejected`, `held-out-contamination` (a BPM subject reached
memory formation — voids the evaluation batch, recorded not hidden).

## Build slices

- **S0 (load-bearing first): recall-at-dispatch.** Ground-control dispatch
  script: given a problem id, call recall with the problem/trigger-class
  refs, render the surfaced neighborhood into the runner prompt, record
  what was surfaced (the use-receipt's "offered" half). Without S0 nothing
  upstream of the store ever reaches the runner.
- **S1: scribe packet (batch, per-session).** Mine the session's turn-round
  evidence (store-native), spot arcs, distill scoped rules in the
  `zai1-rewrite-rules-draft.edn` shape, `memory_record` with evidence-id
  provenance and pattern/problem subjects. Codex-sized; the six-rule
  hand-reshaping is the acceptance template.
- **S2: receipts + challenge edges for the zai loop** (reuse WM receipt
  shape; `rule-misleading` becomes recordable).
- **S3: pattern formation lane**: cluster proposals (e.g. the linarith
  triple → one n=3 pattern) recorded as typed events; operator promotes.
  New-level events (pattern-of-patterns) are first-class.
- **S4: BPM evaluation batch 0** (baseline before further memory accretion —
  should run EARLY, while the store is still small).
- Provisioning prerequisites: memory-domain stamps for zai-2/codex-2
  (mathematics); `memory_record` tool description spelling out required
  fields (name/body/subjects) so first calls don't bounce-learn.

## Theory link

E-retrieval-flows binds this loop to the evolving-operator frame (Rob's
Sturm–Liouville reading; MetaCA genotype/phenotype): the scribe grows the
graph connectivity that makes the retrieval operator family non-degenerate;
Rung 4's coupled θ-evolution turns receipts into coefficient updates; "new
levels of patterns" is the abstraction axis MetaCA's typed boxes formalize.
The self-improvement claim, in that language: **the coupled system (x, θ,
graph) moves into and stays in the sustaining band, and the structure it
sustains pays rent on held-out problems.**

## Open questions

- Recall-injection format: raw rules vs pattern-neighborhood summary — what
  does a zai runner actually *use*? (S0 should A/B this within sessions.)
- Scribe cadence at v1+: real-time following (agent-follow feed) once batch
  distillation is proven; the GPU-local annotator slots here later.
- Panopticon interop: can Rob's rewriting system consume/emit the rule
  shape directly? (Draft EDN is shaped to hand over as-is.)
- How BPM turn-counts normalize across problem difficulty — flat count vs
  per-collection strata.

## Learnings from the existing runs → S1 memory-creation design (2026-07-26)

### What the corpus actually taught

1. **Agents narrate their own rules at the moment of repair** (zai-1 mining):
   distillation is mostly *binding* — attach scope, before/after, and
   evidence ids to a diagnosis sentence the agent already wrote — not
   inference. The error→fix arc span is the natural scope; levels
   (tactic/plan/process) must be registered per rule.
2. **Failure types each want a different memory kind** (batch 0):
   round-budget death (1.7.1: 216 tool calls of API search) wants **API-map
   memories**, minable from *unsuccessful* trajectories ("this region is
   expensive; that lemma family is elsewhere"); self-termination (1.8.1)
   wants **process rules** with a lexical signature (zero edits + long
   prose); substrate kills want nothing — they are confounds to type, not
   learn from.
3. **Final summaries are near-ready memory bodies**: solved sessions state
   problem-class → key lemma in one paragraph (`integral_pos`,
   `lipschitzWith_of_nnnorm_deriv_le`, `exists_ratio_hasDerivAt_eq_ratio_slope`,
   Cauchy MVT). Lemma-location memories are the cheapest, highest-precision
   lane, and they are the connectivity food the retrieval operator needs
   (shared lemma/tactic/problem-class nodes — see E-retrieval-flows v1).
4. **The supervised-overrun machinery works; budgets are the constraint
   surface**: wall-clock extensions engage, but tool-round budgets bind, and
   quota contention between consumers can masquerade as capability failure.
   Memories should carry conditions; batch design should own the substrate.

### Boundary decision (flagged to operator)

Batch-0 (BPM) sessions are held out: their MATHEMATICAL content is never
mined into memories. Using their *failure shapes* to design the mechanism
(this section) is permitted meta-learning — the line is content vs
mechanism. The mineable corpus for actual memory formation = the APM
sessions (a95/a96 series: 2 complete + ~14 partials as of 2026-07-26
morning, all with turn-round evidence in the landscape).

### S1 scribe design: three extraction lanes

| Lane | Input | Output memory kind | Precision |
|---|---|---|---|
| **Solve-lane** | final summaries of solved/partial sessions | lemma-location + proof-pattern (problem-class → lemma/tactic) | high, near-mechanical |
| **Arc-lane** | error→fix spans in turn-round streams | scoped rewrite rules (the six-rule shape: scope/before/after/level/confidence/evidence-ids) | medium, needs distillation |
| **Trajectory-lane** | failed/expensive stretches (round burn, self-termination signatures) | negative/cost memories + process rules | medium, highest novelty |

Every memory: name/body/subjects (store-enforced), turn-round evidence ids
as provenance, level, confidence by instance count, authored by the scribe
seat (author ≠ runner by construction). First scribe batches land as
scribe-asserted; operator reviews in the store (the six-rule review is the
acceptance template); librarian/challenge machinery takes over later.

### Next steps (order)

1. **S1 pilot**: codex scribe packet over the a95/a96 corpus → first
   mathematics-domain memories recorded through the fixed memory_record
   path. Target: the three lanes each produce ≥1 reviewable memory.
2. **S0**: recall-at-dispatch, keyed on problem terrain (the bpm-starter
   README terrain labels seed the pattern vocabulary).
3. Provisioning: mathematics domain stamps for runner + scribe seats.
4. Training cohort 1 (APM problems, scribe following per-session), then BPM
   batch 1 against the accreted store.

## Beyond mathematics: WM memory mining and the HITL pattern economy (Joe, 2026-07-26)

Parked for after batch 1 — recorded so it is not lost; the mathematics-facing
sequence is unchanged.

- **WM mining round**: the same scribe pipeline pointed at the WM's own
  corpus (cohort attempt records, repair arcs like 056's :grounded-change,
  typed failure kinds like 057/058 selection-timeouts). Lanes map directly:
  solve = grounded operation sequences; arc = repair conversions;
  trajectory = what failures teach about the workflow.
- **Demarcation criterion** (the "mathematisization of programming"): a
  workflow is WM-able iff it can be theorised as a pattern cascade;
  human-sequenced build-outs are not. The frontier is dynamic and
  diagnostic: where the cascade cannot be written, the residue is a missing
  pattern — the work-list for pattern-supply HITL.
- **Two HITL forms, one store signature**: HITs (per-instance, O(n)) vs
  pattern-supply (per-class, amortized; e.g. patterns for reasoning about
  topology without vision — a pattern as prosthetic for a modality gap).
  Human-authored patterns enter the same typed graph and get the same
  use-receipts/challenges, so HITL quality is measurable with existing
  machinery; humans are another author class in the structure-formation
  lane.
- **Domain-differential capability mapping**: works-in-algebra/analysis vs
  not-in-topology/web is the memorization-contrast methodology generalized —
  the capability star-map across domains, with pattern-supply as the repair
  action where stars are dark.

## S5 (chartered 2026-07-26, from lane-B feasibility): semantic proposal lane

Verdict from the timeboxed investigation (codex-5): **feasible-later; MiniLM
is the right proposal tool.** The nightly futon3a pipeline (04:30 cron,
`index_patterns.sh --minilm`) indexes 1,521 file-backed patterns/missions but
zero :7073 store records. Two missing pieces before dispatch integration:

1. **Nightly exporter**: reviewed :7073 memory/pattern records → the MiniLM
   corpus (graph hydration measured at ~53s for 12 memories — must happen
   nightly, never at dispatch).
2. **Long-lived query boundary** reachable from futon3c (warm MiniLM query:
   46ms; cold Clojure path: 11.8s — a persistent process or service, not
   per-dispatch spawn).

Integration seam (precisely located): `propose-patterns-by-query` gains an
optional `:semantic-proposals` fn; candidates pass through the existing
`recall-batch-fn` and are retained ONLY where the domain projection proves a
reviewed memory/assert edge to that exact pattern. **Embeddings propose; the
reviewed graph remains the warrant.** Dispatch exposes it as
`--semantic-recall` (:recall-system :v2-semantic).

Evidence this matters: the feasibility overlay queried with the a93A01
statement surfaced candidates lexical FTS (v0 AND v1) missed —
math/proof-architecture 0.29, series-evaluation-api 0.19,
construction-before-estimates 0.17 — i.e. the a93A01 recall-empty rows in
cohort 1 are exactly the rows a v2 lane would have populated. The cohort's
recall-system column anticipates :v2-semantic.
