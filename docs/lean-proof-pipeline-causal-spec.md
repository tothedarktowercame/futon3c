# The Lean proof pipeline's causal graph — authored spec, v1

**2026-08-02.** Companion to `lean-proof-pipeline-causal-spec.json`, which
is the artifact. Example #2 for the diagram engine (M-diagramprover WS-C),
authored in the same discipline as the memory-system spec
(`memory-causal-graph-spec.json`): component-grain variables, mediators
preserved, arrows carrying mechanisms with evidence, time-indexed state
instead of cycles, measurement layer separate from causal structure. The
evidence corpus is the M-codex-sorry-loop campaign record — a completed
137/145 campaign, so unlike the memory spec every arrow is evidenced;
the one conjecture is an *exclusion restriction*, not an arrow.

**20 variables, 31 evidenced arrows, 1 flagged conjecture (an exclusion
restriction), 4 removable leak edges, 7 sensors with characterized
corruption, 2 regime axes, 3 interventions, 3 requested receipts.**

## How to read it

The causal spine, one dispatch round:

```
statement + queue row + library + docs (state at t)
    -> statement_audit -> dispatch_packet -> route_selection
    -> proof_search  <- library_state          [the search consumes the library]
    -> dependency_set                          [the variable with a derivative]
    -> build_verdict -> verification_readout   [corrupted by build_env_state]
    -> reported_outcome -> review_verdict
    -> outcome (holes at HEAD, audited)
    -> queue row / library state at t+1        [the loop, time-indexed]
```

Four things to notice, because they are where the spec does work:

1. **The statement has a direct edge to the outcome (P01 -> P16), and it
   is a regime axis.** A false or vacuous statement cannot close no
   matter how good the search — the campaign's endgame made this regime
   boundary visible: search-limited failures were exhausted, leaving 6
   statement-defect rows for Joe. Pooling sound and defective statements
   is invalid; the defect ceiling is structural, not statistical.

2. **The truth/readout split is drawn explicitly.** `build_verdict`
   (P11, Lean's actual verdict) and `verification_readout` (P13, what
   the runner *reads*, corrupted by stale oleans and namespace state,
   P12) are separate nodes. The alarm-shaped-failure discipline
   ("re-derive the name or rebuild before believing a negative") is an
   intervention on P13. Likewise `reported_outcome` (P14) is separate
   from `outcome` (P16): the 30-minute job cap kills reports *after*
   the work has landed, biased against exactly the hardest rounds.

3. **The conjecture is "sorry count is the wrong meter," made
   checkable.** CJ1: library interventions act on closure only through
   the dependency set (P10), and hole count carries no additional
   signal. The pipeline's ground-truth sensor (T01, queue_audit) is
   excellent, but its *progress* variable has no sensor at all (T05,
   absent) — CJ1 is the preregistered argument for building one.

4. **The leak edges make duplication debt causal.** K2 is the
   campaign-specific one: a copied-provenance module's content lives as
   byte-identical declarations inside problem files, so withholding the
   module does not withhold the content. LusinN (copied; consumer holds
   an independent copy) vs BanachZarecki (extracted; zero overlap, real
   import edge) is the characterized contrast pair, and it drives the
   falsifiable prediction in R2.

## The three receipts we request

Stated fully in `requested_receipts` in the JSON; in brief:

- **R1** — what `do(add ConstructionTarget)` identifies, given that the
  natural experiment (four same-day closures, residual E7) chose its
  module by need: the adjustment set for the controlled version, and
  what the uncontrolled observation does and does not license.
- **R2** — withholding validity by provenance regime, with the
  falsifiable divergence: withholding **LusinN** should have *no* effect
  on its non-importing copy-holder (K2 open), withholding
  **BanachZarecki** a *nonzero* effect on its real consumer. Confirmed
  asymmetry = duplication debt measured as a causal quantity: the
  difference between removing a module and removing its content.
- **R3** — sensor sufficiency: hole count (T04) is predicted NOT to be
  a sufficient statistic for progress (witness trajectory: a96A04's
  1→1→1 holes over a shrinking dependency set); under CJ1 a
  dependency-set sensor (T05) would screen T04 off entirely. Confirmed
  screening-off = the preregistered case for building T05.

## What is deliberately out of v1

Joe's statement-repair decisions (operator, not mechanism); the held-out
files (Part B, excluded by design); estimation and sensor-bias
weighting; and the cross-pipeline join to the memory-system graph
(recall during proof search) — v2 can join the two specs at
`proof_search <- surfaced_set` once both are stable.

## Revision contract

Same as the memory spec: this is versioned, it is wrong somewhere, and
encoding it in the engine is how the first errors surface. Corrections
land as v2 with deltas recorded, not silent edits. (v1 already ate one
such correction at authoring time: the within-round readout->search
feedback initially drew a cycle; it is now time-indexed inside the
trajectory, per the discipline.)
