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

Baseline already in the landscape (2026-07-25): a95A07 complete (0 sorries,
commit 1d622b0); a95A04 partial (symmetric_split proven, 2 sorries);
turn-round counts recoverable per problem.

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
