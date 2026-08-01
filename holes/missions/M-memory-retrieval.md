# M-memory-retrieval — growing the retrieval subsystem toward the theory

Date: 2026-07-27
Status: **PROPOSED** (organised by claude-6 at Joe's handoff, 2026-07-27;
charter confirmation = Joe). WS1 (Rung 4) needs no confirmation — it was
chartered dark by Joe 2026-07-24 in `E-dynamic-queries` §Rung 4 and is
dispatched under that charter.
Owner: claude-6 (organisation, review, architecture). Coding handoffs to
idle Codex agents per the workspace handoff protocol.

## Position in the landscape

The theory→live mapping is now written down end to end
(`holes/excursions/E-retrieval-flows.md` §v2, 2026-07-27): the
M-zai-learning-loop recall stack is the first deployed shadow of the
evolving-operator frame `(x_t, θ_t, F_t, B_t)` with S6's receipt-informed
ranking as the first live Ψ. The honest delta: live Ψ is a **scalar
per-memory** multiplier, not the per-relation-type operator update the
theory draws; the dark rungs of `E-dynamic-queries` (Rungs 1–3 implemented
dark; Rung 4 chartered, unimplemented) are the growth target. This mission
organises the climb from the deployed shadow toward the drawn theory.

Boundary with **M-zai-learning-loop (claude-4)** — the interface contract:

- claude-4 owns the *live loop*: cohorts, dispatch_with_recall, receipts,
  the deployed S6 ranking, curriculum lane, prereg discipline.
- this mission owns *retrieval-subsystem growth*: the dark rungs, spectral
  and connectivity diagnostics, the Ψ-v2 design, the semantic lane design.
- **Interface 1 — receipts are read-only observations here.** This mission
  consumes frozen receipt snapshots; it never writes receipts and never
  mutates the live ranking. Promotion of anything into the live path is a
  coordinated act with claude-4 (and Joe where policy).
- **Interface 2 — the calibration gate stands.** Any θ-learning beyond the
  Rung 2 one-outcome ratio stays gated on the Phase 6 minimum (n ≥ 20
  independently witnessed outcomes; frozen sample was 13). Cohort exhaust
  from claude-4's loop is what raises n; this mission does not manufacture
  outcomes.
- **Interface 3 — dark-first.** Everything here ranks only inside frozen /
  admissible corpora (`:live-ordering-changed? false`) until its own
  acceptance bar is met AND live wiring is agreed at Interface 1.

### Live-side coordinates (claude-4, 2026-07-27 bell reply)

- **Receipt corpus** (our read-only observations): store `:7073`, author
  `ground-control`, type `:pattern-outcome`, body `:event :memory-use`
  with `:phase :offered|:outcome`. As of 2026-07-27: ~10 offered-halves,
  2 outcome-halves (one witnessed 2/2 use event, one reasoned non-use);
  growing several/day while cohorts run — the Phase 6 n≥20 gate is
  probably **days** away, not weeks (WS3 evaluation timing).
- **Deployed Ψ contract**: S6 = `1 + α·used/offered`, α=0.5, cold-start
  neutral. `E-retrieval-flows.md` §v2 (commit `07403f5`) is the
  current-state contract WS3 designs against.
- **Retrieval-miss dataset**:
  `holes/labs/M-zai-learning-loop/s1-pilot/meta-drafts.edn` — typed miss
  diagnoses (TeX-encoding term loss, description-vocabulary gaps) with
  actual receipt query terms; cohort-2 ops log accumulates more. Real
  failure cases for validating WS2/WS3/WS4 designs.
- **Versioning contract for promotion**: live recall changes carry a new
  `:recall-system` tag per receipt (…→ v1.2-normalized as of 07-27).
  Interface-1 proposals ship with the next version tag and land at a
  **cohort boundary**, never mid-cohort (claude-4 slots them).
- **Meter feedback loop closed (07-28)**: claude-4 runs
  `connectivity_meter.bb` at each cohort close and reports the reading
  in the cohort meta-meters (per-cohort operator-food series, live
  side). Scribe instruction adopted from next pass: cross-memory
  references (updates/resolves/contrasts) become **typed edges**, not
  prose. Verified 07-28: supersession currently lives in evidence
  bodies only (3 body-level refs; zero `:memory/retract` hyperedges) —
  latent food the scribe rule will lift into the measured graph.
  Calibration: current isolation is between *pattern families*
  (terrain clusters); shared patterns already span problems within a
  family. First expected bridge: S2's distills edges (one session,
  three terrains). **Backfill commitment (07-28)**: claude-4 backfills
  the 3 known body-level relations (2 supersedes + 1 resolves, liminf
  chain) as typed edges at cohort-2 close, *before* the meter run, and
  takes a pre/post-backfill reading pair — a clean calibration point
  for meter movement per edge-type added.
- **Receipt-protocol fix (07-28, after WS3's join audit)**: claude-4
  (a) backfills outcome halves for the 12 offered-only jobs at
  cohort-2 close (full ground truth held in cohort tallies/ops log —
  legitimate rows, not reconstructions); (b) protocol change recorded
  into the RESUME doc — outcome half at EVERY ground-truth
  verification unconditionally, recall-empty rows included. Expected:
  at/near n≥20 by Saturday evening. **Analysis discipline for the
  Saturday re-read**: backfilled halves are distinguishable by
  recorded-at vs job time — the Ψ-v2 re-read must segment metrics by
  receipt provenance (backfilled vs at-verification) and report
  strata separately if they differ.

## Workstreams

### WS1 — Rung 4: k-step coupled propagation — **LANDED + REVIEWED 2026-07-27**

Codex-4 `35f1fef`; owner review PASS with all gates re-run independently
(12/71/0 focused, kondo 0/0, parens OK, demo output == frozen results).
Battery result: floor-off collapses to the decoy (θ 1.0/0.0), floor-on
ε=0.2 recovers the planted target at step 2 — with both control arms
ranking the decoy first, so recovery is earned by the iterated dynamics.
Verification record: `E-dynamic-queries.md` §Rung 4 verification.

The spec is complete in `E-dynamic-queries` §Rung 4: iterate Φ/Ψ up to k
steps over frozen corpora with an exploration-mass floor; the
confirmation-collapse battery (floor-off must collapse, floor-on must
recover) is the falsifiability core; k=1 identity with Rung 1; fresh
namespace beside `dynamic_queries.clj`; control arms retained.
Codex packet: `holes/CODEX-HANDOFF-rung4-coupled-propagation.md`.
Acceptance = the charter's own acceptance block.

### WS2 — Spectral + connectivity diagnostics — **LANDED + REVIEWED 2026-07-28**

Codex-4 `07aa3af` (spectral sweep) + `be93d0f` (connectivity meter);
owner review PASS, gates re-run independently. Key outcomes: the food
problem in spectral form (v0's four components = four zero modes);
first live meter reading **`:component-limited`** (largest reviewed
component 6 nodes / 1 edge type / λ₂≈1.0 — the standing baseline for
cohort operator-food); the preregistered λ₂-vs-time check failed at
ε=0.3 (ρ=0.0) and the owner-review diagnosis (explicit-Euler stability
boundary) was CONFIRMED by a preregistered ε=0.1 re-test (ρ=−0.8):
**step size is part of the operator**. Full record: `ws2-results-note.md`
+ E-retrieval-flows §v3. WS4's gate reading: connectivity-starved —
food (WS5/cohorts) before lanes.

Original scope (retained for reference):

From `E-retrieval-flows` §Next steps: compute the spectrum of Δ_θ (λ₂ per
component, spectral gap) beside the trajectory classification in the sweep
scripts (`holes/labs/M-typed-memories/retrieval_flow_sweep*.bb`), and make
"is the graph dense enough for dynamics to beat lookup?" a standing meter
(component census + λ₂ over the *current* memory/pattern graph, run on a
frozen export). The v0 finding — dynamics were component-limited — becomes
a measurable connectivity threshold instead of an anecdote. This is also
the honest gate for WS4: operator sophistication is worthless below the
connectivity floor. Deliverable: extended sweep script + one diagnostics
script + a short results note. Candidate self-work or a small Codex packet.

### WS3 — Ψ v2 design: from per-memory scalar to per-edge-type θ (DERIVE) — **LANDED + REVIEWED 2026-07-28**

Codex-4 `965128d`; owner review PASS, gates re-run independently
(harness replays frozen export; kondo 0/0; parens OK; design
conformance verified in source: credit-assignment modes, α=0.5,
n-min=5, LOO no-self-scoring, θ_r `:inactive-degenerate`, ε=0.01
floored simplex, synthetic fixture asserts θ=1.5 → ranking flip).
First corpus numbers: 16 halves / 2 joined / **n=1 metric row** — all
arms MRR 1.0; no comparative claim, verdict
`:below-calibration-minimum` as predicted. **Binding constraint
identified: outcome-half completion** (12 of 14 jobs have offered
halves but no outcome half) — flagged to claude-4 for cohort-2's
remaining rows. Record: `ws3-results-note.md`.

Owner design: `holes/labs/M-memory-retrieval/psi-v2-design.md`
(pattern-level θ; deterministic fractional credit assignment through
reviewed attachments; α=0.5 for S6 comparability; per-coefficient
abstention at n<5 in-harness, n≥20 Phase 6 standard for any live
promotion; θ_r declared degenerate until the edge-type census exceeds
one). Replay harness → codex-4, packet
`holes/CODEX-HANDOFF-ws3-psi-v2-replay.md`. Expected verdict at current
receipt volume: `:below-calibration-minimum`, emitted honestly.

Original scope (retained for reference):

The key row of the v2 table, advanced one rung. Design (logic-model first,
paper before code): aggregate receipts per (pattern, relation-type) rather
than per memory; define the bounded multiplicative update, its floor
(exploration mass — the same ε as Rung 4), cold-start neutrality, and the
audit fields; specify what n the Phase 6 gate requires *per coefficient*
(the calibration burden multiplies with θ's dimension — this must be
stated, not discovered). Dark evaluation harness: replay frozen cohort
receipts through Ψ-v2 vs the live scalar Ψ vs no-Ψ, on held-out
surfacing precision. No live wiring inside this mission (Interface 1).
Timing: design now; evaluation as cohort receipts accumulate.

### WS4 — S5 semantic lane (design + dark evaluation)

Charter §S5 (MiniLM proposals, feasible-later) with live evidence already
in hand: the a93A01 recall-empty rows are exactly what the feasibility
overlay would have populated (charter §v2-semantic evidence). Scope here:
the proposal-only contract (embeddings PROPOSE, reviewed edges WARRANT —
the B_t discipline is untouched), a dark replay on cohort-1/2 rows
measuring would-have-surfaced precision, and a wiring proposal handed to
claude-4. Gated behind WS2's connectivity read: if recall-empty rows are
connectivity-starved, food beats lanes.

### WS6 — Whitepaper (claude-6 holds; carve-out; Joe's direction 2026-07-27)

The "show the correspondence table to Rob" item, promoted: a whitepaper at
patent-filing technical grade — claims-enumeration first, then the
correspondence tables (MetaCA/retrieval/Rob's frame; theory→live v2),
architecture, embodiments (the rungs), evidence, honest deltas. Written
**bit by bit by the Claude owner under carve-out (b)** — each WS result
lands as an evidence increment; no Codex handoff. Doubles as deliberate
defensive publication (dated, legible prior art) while leaving the US
12-month grace-period option open — measurable improvement is
demonstrable from the claude-4 learning loop (cohort-1 witnessed use
chain); the updating-Laplacian mechanism's *specific* contribution is
pending WS1/WS2 evidence and is marked as such, never claimed early.
Draft: `docs/retrieval-whitepaper.md`. Send-to-Rob gate = Joe.

### WS5 — Operator food (connectivity supply) — mostly not ours

Scribe output IS operator food (v1 sweep finding). The supply side lives
in claude-4's loop (mining lanes, pattern wiring, curriculum). This
mission's part is only the WS2 meter — publishing the connectivity
number per cohort so the loop can see whether its exhaust is feeding the
graph. Explicitly not owned here: mining, wiring scripts, curriculum.

## Sequencing

1. **WS1 dispatched now** (codex packet; independent of everything live).
2. **WS2 next** — cheap, and its meter gates WS4 and informs WS5.
3. **WS3 design in parallel** (paper artifact; no code dependency); its
   evaluation waits on receipt volume (Interface 2).
4. **WS4 after WS2's read**, wiring proposal to claude-4 at the end.

## Out of scope

- Any change to live ordering, live ranking, or dispatch_with_recall
  without Interface-1 coordination.
- Relaxing the Phase 6 calibration gate or the reviewed-edge warrant
  discipline (B_t).
- Retroactive bulk mining of historical transcripts (M-typed-memories
  out-of-scope item stands).
- ~~The Rob conversation (showing the correspondence table) — Joe's
  call.~~ Promoted into WS6 (2026-07-27): the whitepaper IS the Rob
  deliverable; sending it remains Joe's gate.

## Cross-references

`holes/excursions/E-dynamic-queries.md` (rungs; Rung 4 charter) ·
`holes/excursions/E-retrieval-flows.md` (frame → experiments → live, v2) ·
`holes/missions/M-typed-memories.md` (substrate) ·
`holes/missions/M-zai-learning-loop.md` (the live loop; §Second derivative)
· `algorithms/zai-learning-loop.md` (recipe).
