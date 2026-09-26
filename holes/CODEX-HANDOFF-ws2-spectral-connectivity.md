# CODEX-HANDOFF — WS2: spectral classification + the connectivity meter

Mission: `holes/missions/M-memory-retrieval.md` WS2. Prepared 2026-07-27
by claude-6 (Claude owner). **Delivery: Agency bell from claude-6. When
done, bell claude-6 back with a summary + commit SHAs.** Owner reviews
after landing (author ≠ reviewer).

## Goal

Two lab instruments, both babashka, both read-only with respect to the
store:

1. **Spectral classification** (`retrieval_flow_sweep_v2.bb`): compute
   the spectrum of Δ_θ beside the v1 trajectory classes, answering
   E-retrieval-flows §Next steps ("spectral classification per θ beside
   trajectory classes").
2. **The connectivity meter** (`connectivity_meter.bb`): a standing
   diagnostic over the *current live* memory/pattern graph answering
   "is the graph dense enough for retrieval dynamics to beat direct
   lookup?" — the v0 finding ("component-limited") turned into a
   measurable threshold.

Background you must read first: `holes/excursions/E-retrieval-flows.md`
(v0/v1 findings and why connectivity gates operator expressiveness).

## Files

`:in` (READ-ONLY — do not modify):
- `holes/excursions/E-retrieval-flows.md` — intent and prior findings.
- `holes/labs/M-typed-memories/retrieval_flow_sweep.bb`,
  `retrieval_flow_sweep_v1.bb`, `retrieval-flow-sweep-results.edn`,
  `retrieval-flow-sweep-v1-results.edn` — v0/v1 stay FROZEN for
  comparability; v2 is a new script.
- `holes/labs/M-typed-memories/phase4-wm-corpus.edn`,
  `zai1-rewrite-rules-draft.edn` — the two graph sources.
- `scripts/wire_math_memory_patterns.clj`,
  `scripts/dispatch_with_recall.clj` — learn the LIVE edge shapes
  (memory→pattern attachments, review status) from these; do not guess
  the store schema.

`:out` (create, all under `holes/labs/M-typed-memories/` unless noted):
- `retrieval_flow_sweep_v2.bb`
- `retrieval-flow-sweep-v2-results.edn`
- `connectivity_meter.bb`
- `live-graph-export-20260727.edn` — frozen export the meter ran on
- `connectivity-meter-20260727.edn` — the meter's first reading
- `ws2-results-note.md` — short honest note (findings, not narrative)

## Part 1 — spectral classification (v2 sweep)

- Rebuild the v0 (phase-4) and v1 (combined rules) graphs exactly as the
  frozen scripts do (same edge construction; lift the code, cite the
  lineage in a comment).
- For each θ in the v1 grid (uniform / prescribe-heavy / uses-heavy /
  hub-off — reuse v1's grid) build Δ_θ = Σ_r θ_r Δ_r as a real symmetric
  matrix and compute its full eigenvalue spectrum.
- **Numerics**: cyclic Jacobi eigenvalue algorithm for symmetric
  matrices (pure Clojure/bb, no external deps); convergence tolerance
  1e-10 on off-diagonal norm, iteration cap stated and reported;
  deterministic (fixed sweep order, no randomness). n ≤ ~25, so cost is
  trivial.
- Report per θ-grid point: component census (from the unweighted union
  graph restricted to relations with θ_r > 0 — note hub-off changes the
  support), per-component λ₂ (second-smallest eigenvalue of that
  component's sub-Laplacian), spectral gap, and the v1 trajectory class
  for that grid point (join against the frozen v1 results).
- **Consistency check (falsifiable, in the results)**: across the grid,
  heat-run time-to-uniform (steps to reach participation within 5% of
  its tail value, computable from a rerun of the v1 heat dynamics inside
  v2) must anti-correlate with λ₂ of the seed's component — report the
  rank correlation. If the sign comes out wrong, report it as-is; that
  is a finding, not a bug to hide.
- v0 graph included to exhibit the degenerate case spectrally: tiny
  components, λ₂ values, and the statement of the food problem in
  spectral form.

## Part 2 — the connectivity meter

- **Export step**: bounded, read-only queries against the futon1b store
  (base `http://127.0.0.1:7073`) for the graph pattern-mediated recall
  actually runs over: `:memory/*` hyperedges and their pattern
  endpoints / attachment edges with review status (shapes per the two
  `:in` scripts). **Every query carries an explicit `&limit`** (heavy
  hyperedge type-queries; store discipline). If the store answers
  `:expensive-read-busy` or 503, back off once (≥5s), retry once, then
  degrade gracefully with the error recorded in the export file. NO
  writes of any kind; no unbounded scans.
- Export is stamped: date, exact query URLs + limits, row counts,
  truncation flags. The meter then runs on the frozen export only.
- **Meter output** (`connectivity-meter-20260727.edn`): node count, edge
  count by relation/attachment type, reviewed-vs-unreviewed edge counts,
  component count + size histogram, per-component λ₂ for the K largest
  components (K=5, same Jacobi code), and a **verdict**.
- **The verdict criterion is preregistered in the script header, not
  chosen after seeing the numbers.** Proposed criterion (adjust only
  with written justification in the header, before running):
  `:dynamics-informative` iff the largest reviewed-edge component has
  ≥ 10 nodes AND ≥ 2 distinct relation/attachment types AND λ₂ > 0.1;
  else `:component-limited`. Grounding: the v0 graph (3-node components)
  was degenerate; the v1 graph (23 nodes, connected, 4 relation types)
  differentiated. Whatever the live graph scores, report it honestly —
  `:component-limited` is the *expected* first reading and is the
  baseline the cohort loop's "operator food" gets measured against.
- The meter must be **rerunnable**: date-stamped output files, no
  overwriting of previous readings (filename carries the date).

## Acceptance checklist

- [ ] `bb retrieval_flow_sweep_v2.bb` runs end to end; results EDN
      parses; per-θ spectra + trajectory-class join + consistency
      correlation present.
- [ ] Jacobi implementation validated in-script against a known case
      (e.g. path graph P3: Laplacian eigenvalues {0, 1, 3}) — assert,
      don't eyeball.
- [ ] `bb connectivity_meter.bb` runs end to end; export + meter EDN
      written; all store queries bounded; zero store writes (script
      contains no POST/PUT/DELETE).
- [ ] Verdict criterion present in script header and predates the run
      (single commit is fine; the header text must state the criterion
      as preregistered).
- [ ] `ws2-results-note.md`: ≤ 60 lines; what the spectra say about the
      v1 trajectory classes; the live meter's first reading; honest
      statement of the consistency-check result.
- [ ] v0/v1 scripts and results untouched (`git diff --stat` shows only
      `:out` files).
- [ ] `clj-kondo` 0 errors on the two new .bb files;
      `futon4/dev/check-parens.el` clean on them.
- [ ] Bell claude-6 with summary + commit SHAs.

## Explicitly out of scope

Store writes; changes to live recall or ranking; changes to v0/v1
artifacts; interpreting the meter as a gate on anyone's work (it is a
meter — the reading is the deliverable); embedding/semantic lanes (WS4).
