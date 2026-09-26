# S4 scribe pass 34

- Mode: two new drafts plus resolution/amendment proposals; drafts only; no
  store-write endpoint was called.
- Solve-lane yield: 2.
- Arc-lane yield: 0.
- Frontier-lane yield: 1.
- Trajectory-lane yield: 0.
- Total yield: 3 drafts.

All five supplied receipt IDs resolved. The omitted a94A06 outcome receipt
appeared during the pass as `b832f130-765b-4759-90e9-29bebf8c159c`; it resolves
and names a94A06. The earlier
`e-638cb980-155f-42ed-a6b7-b34c8dc2c743` is only the offered/timeout record and
was not substituted for the outcome.

I regenerated both rollout harvests in dry-run mode and body-verified:

- a94A01: codex-7 turns 58–59;
- a93A04 closure: codex-7 turn 60;
- a94A03: codex-7 turns 61–62 and codex-6 turn 62;
- a93J07: codex-6 turn 60;
- a94A04: codex-6 turn 63;
- a94A06: codex-6 turn 64.

The a94A06 close now yields one solve memory: its improper-FTC theorem derives
integrability from a nonnegative derivative and finite antiderivative limit,
which is the structural opposite of the `integral_undef` defect shape. No
turn-round was written to the store by this pass.

The store—not only local promote scripts—was checked before choosing subjects:

- `e-codexpilot-order-a-disjoint-interval-family-into-a-variation-partition`
  resolves and its live assertion endpoints include `a93A04`,
  `math/measure-integration-api`, and `math/missing-dependency-protocol`;
- `e-codexpilot-preflight-domain-guards-and-inferred-bound-types-in-analysis-statements`
  resolves and its live endpoints include `math/corpus-trust-protocol` and
  `math-formalization/notation-semantics-traps`;
- `e-codexpilot-integral-minkowski-eLpNorm-bochner` resolves and its live
  endpoints include `young-convolution-L1-L2` and
  `math/missing-dependency-protocol`;
- both memories discussed in a94A03's use report resolve directly.

## Verdict 1: resolved frontier

Confirmed. Amend
`e-codexpilot-order-a-disjoint-interval-family-into-a-variation-partition`
from `:gap-open` to resolved by construction.

The construction is exactly the one requested by the frontier:

1. filter degenerate intervals, whose endpoint increments vanish;
2. move the active finite set through an arbitrary `Fin` equivalence;
3. sort by left endpoint with `Tuple.sort`;
4. obtain monotonicity from `Tuple.monotone_sort`;
5. use pairwise disjointness to separate successive intervals;
6. embed the selected endpoint increments in one monotone variation
   partition.

The resolution lemma is
`sum_edist_le_eVariationOn_of_mem_disjWithin`, axiom-clean at commit
`56c337538c4bd866874ba6a9db68c284f7470352`.

The broader lesson is capturable, but as a frontier-lifecycle rule rather than
a mathematics memory:

> Library absence names a construction target; it does not establish that the
> target is blocked. A frontier should record attempt history and should not be
> classified as durable merely because no packaged declaration exists.

This is not just an operator-error anecdote because it changes a repeatable
queue transition. However, one fast construction does not justify a universal
attempt threshold. Record `:construction-attempt-count` and
`:last-construction-obstruction`, and let review decide when evidence supports
`:durable-frontier`.

## Verdict 2: computable false conclusion

This belongs as step 4 of the promoted statement-preflight memory, not as a
distinct rule.

The preflight already asks for a mechanism-specific degenerate witness. a94A04
adds the missing second phase: after showing a hypothesis is vacuous, instantiate
the conclusion where its operation is genuinely defined and compute a
contradiction. This upgrades the diagnosis from “the premises do not constrain
the intended object” to “the theorem is false.”

Keep the evidence levels distinct:

- vacuity: the monomial products are non-integrable and their Bochner
  integrals totalize to zero;
- falsity: for `p(X)=1-X`, the product is integrable and its telescoping
  integral equals one.

The receipt is explicit that this is argued, not machine-checked.

## Verdict 3: memory-use modes

Both distinctions belong in the benchmark.

`prevented-a-misapplication` is a genuine use mode. It requires:

- the candidate action the runner was considering;
- the memory-derived boundary that rules it out;
- evidence that the decision changed;
- no implication that the memory carried a proof.

Here, the compact-support memory prevented applying a compact-bump theorem to a
full-support Gaussian.

`accurate-but-too-narrow` is distinct from a miss. Use
`:scope-boundary-confirmed` when a memory correctly names the proof family or
frontier but its quantified scope does not cover the target. If it changes the
plan or isolates the missing generalization, count it as a use; otherwise
record it as considered-and-declined with a scope reason. The p=2 Young memory
did change the diagnosis here: the target needs all finite `p`.

## Hand-applicable amendments

### A. Resolve the interval-ordering frontier

Target:
`e-codexpilot-order-a-disjoint-interval-family-into-a-variation-partition`

Append:

- `:frontier-status :resolved-by-construction`;
- resolution receipt `52a7728c-dc8e-45c1-a10a-04ab98047665`;
- resolution commit `56c337538c4bd866874ba6a9db68c284f7470352`;
- declaration `sum_edist_le_eVariationOn_of_mem_disjWithin`;
- construction steps: filter degenerate intervals, choose a `Fin` equivalence,
  sort the active subtype by left endpoint with `Tuple.sort`, obtain
  monotonicity with `Tuple.monotone_sort`, use pairwise disjointness for
  separation, and embed the increments in one variation partition;
- downstream status: `lipschitz_V_limit_implies_ac` and
  `ac_iff_lipschitz_V_approx` are both axiom-clean;
- use status `:frontier-resolved-on-next-attempt`.

Preserve the original gap history; do not replace it.

### B. Add attempt evidence to frontier lifecycle

Target: the frontier-drafting/lifecycle candidate register from passes 30–33.

Append:

- library search absence establishes `:unpackaged-construction-target`, not
  `:durable-frontier`;
- record `:construction-attempt-count`, attempted route, proved prefix, and
  exact last obstruction;
- a93A04 evidence:
  - initial receipt `2a8fa1b7-846d-4a71-b001-8f4fa244c5fa`;
  - resolution receipt `52a7728c-dc8e-45c1-a10a-04ab98047665`;
  - attempt count `2`;
  - resolution mode `:constructed-missing-lemma`.

Suggested confidence:
`:single-sharp-counterexample-to-library-absence-implies-durable-blocker`.

### C. Extend the statement preflight

Target:
`e-codexpilot-preflight-domain-guards-and-inferred-bound-types-in-analysis-statements`

Append after the degenerate-witness step:

4. If the hypothesis is vacuous, seek an instance of the conclusion where all
   operations are genuinely defined and compute its value. A nonzero computed
   value upgrades `:vacuous-premise` to `:statement-refuted`.

Add a94A04:

- receipt `782070a0-17b3-478c-a5e6-243c797e73d2`;
- mechanism `:integral-undef`;
- vacuity witness: the atomic measure at
  `xₘ=1-1/(m+2)` and `f(xₘ)=1/(m+1)`;
- defined counter-instance: `p(X)=1-X`;
- computed value: telescoping integral `1`, contradicting the conclusion `0`;
- status `:argued-not-machine-checked`.

### D. Extend the Bochner-integral diagnostic

Target:
`e-codexpilot-derive-integrable-from-nonzero-bochner-integral`

Append a94A04 as an argued reverse-direction instance, but preserve its sharper
two-stage classification:

- non-integrable monomial products make the hypotheses hold via
  `integral_undef`;
- an integrable polynomial product then gives a genuinely nonzero conclusion
  instance.

Do not flatten the second fact into “another vacuity case.”

### E. Add benchmark use modes

Target: the memory-use taxonomy/benchmark register.

Append:

- `:prevented-misapplication`
  - used memory:
    `e-codexpilot-separate-compact-bump-convergence-from-full-support-poisson-kernels`;
  - candidate error: use compact-support convergence for a full-support
    Gaussian;
  - evidence receipt `6f43c013-0bc7-4634-9724-ab6513ebb917`;
  - proof-carried? `false`;
- `:scope-boundary-confirmed`
  - memory:
    `e-codexpilot-reduce-probability-kernel-L2-contraction-to-young`;
  - accurate scope: `p=2`;
  - unmet target scope: every finite `p≥1`;
  - evidence receipts `530247de-a2dc-4207-92e6-ae6cea1b7282` and
    `6f43c013-0bc7-4634-9724-ab6513ebb917`;
  - count as use only when it changes the plan or isolates the generalization.

Keep both separate from `:carried-proof` and `:irrelevant-miss`.

### F. Link the general Young frontier

After promotion, attach
`prove-general-probability-kernel-Lp-contraction-by-integral-Young` to:

- `math/measure-integration-api`;
- `math/missing-dependency-protocol`;
- a94A03;
- a95J08;
- the existing construction-target handle `young-convolution-L1-L2`.

Record that it strictly generalizes, rather than duplicates,
`e-codexpilot-integral-minkowski-eLpNorm-bochner`.

## Routine closes not drafted

- a94A01: sound axiom-clean indicator convergence, but the route is a standard
  assembly of measurable a.e. limits, dominated convergence, and the existing
  indicator `eLpNorm` formula.
- a93A04: no parallel solve memory was minted because the promoted frontier
  should retain its identity and receive a resolution amendment.

## Subject handles

Reused from live store assertions or directly resolving promoted memories:

- `M-codex-sorry-loop`;
- `a93J07`;
- `a94A03`;
- `a94A06`;
- `a95J08`;
- `young-convolution-L1-L2`;
- `math/holomorphic-disk-api`;
- `math/measure-integration-api`;
- `math/missing-dependency-protocol`.

Minted: none.

Searchable hooks and bodies also carry Hurwitz, quantitative open mapping,
Gaussian, general finite `p`, Young, Minkowski, Jensen, Tonelli, full support,
compact support, and scope boundary.
