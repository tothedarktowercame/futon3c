# S4 scribe pass 33

- Mode: four defect diagnoses, four clean closes, and one genuine frontier;
  drafts only; no store-write endpoint was called.
- Solve-lane yield: 4.
- Arc-lane yield: 0.
- Frontier-lane yield: 1.
- Trajectory-lane yield: 1.
- Total yield: 6 drafts.

All eight supplied receipt IDs resolved. The a93A04 receipt, which was not
included by ID in the packet, is
`2a8fa1b7-846d-4a71-b001-8f4fa244c5fa`; it also resolved and named a93A04.

I regenerated both rollout harvests in dry-run mode and body-verified every
cited turn:

- a02J02: codex-7 turns 51–53;
- a92J06: codex-7 turn 54;
- a93A02: codex-7 turn 55;
- a93A04: codex-7 turns 56–57;
- a02J03: codex-6 turn 52;
- a02J07: codex-6 turn 53;
- a92J07: codex-6 turn 56;
- a03J03: codex-6 turns 54–55 and 57–58;
- a93J06: codex-6 turn 59.

None of these new turns is in the store yet. Each draft marks its cited turns
as locally body-verified and awaiting ground-control harvest. The harvester was
not run with `--commit`.

## Mathematical yield

The four clean closes produced non-duplicate construction memories:

1. general-`Lᵖ` Radon–Riesz by Scheffé on norm powers and finite-measure Vitali
   on the original functions;
2. a rational circle integral by partial fractions and an inside/outside pole
   split;
3. `Lᵖ*Lᑫ` convolution vanishing at infinity by compact-support density;
4. a second-order Schwarz bound by applying Schwarz to the derivative and
   integrating radially.

a93A04 produced one frontier memory. Its residual gap is not “variation API”
in general: it is the missing converse that orders an arbitrary finite
pairwise-disjoint family of unoriented intervals into a monotone variation
partition. The derivative-truncation direction is already axiom-clean and is
recorded as proved prefix.

## Verdict 1: one family or separate rules?

Use one umbrella diagnostic and separate mechanism-specific rules.

The shared causal schema is:

> a formal statement omits the guard restricting an operation or value to the
> intended semantic domain, so a degenerate witness satisfies the premise.

The mechanisms remain distinct:

- `integral_undef`: totalizes a mathematically partial Bochner integral to zero;
- `deriv`: totalizes the derivative to zero outside differentiability;
- `ℝ≥0∞`: is not a partial-operation totalization at all—`⊤` is a legitimate
  value in the extended codomain.

Merging the repairs would be wrong. Integral targets need integrability,
absolute continuity, or improper-limit hypotheses; derivative targets need
differentiability; extended bounds need a finite type or an explicit
`M < ⊤`.

The new trajectory draft captures the common preflight and carries a
mechanism table so recall can propose the family without erasing the
distinctions. All four new defect diagnoses are explicitly marked argued, not
machine-checked.

## Verdict 2: statement preflight

This is capturable and is broader than the Bochner-specific memory:
`preflight-domain-guards-and-inferred-bound-types-in-analysis-statements`.

The three cheap checks are:

1. infer the type of every existential bound and try its top/zero witness;
2. check domain guards for every totalized analytic operation;
3. compare regularity and finiteness claims in prose with the actual binders.

The Bochner memory remains the mechanism-specific application. The new memory
does not replace it; it tells a runner when to invoke it and supplies the
parallel derivative and extended-codomain checks.

## Verdict 3: route prescriptiveness

The adjacent a02J02 and a03J03 cases do not support either “name only the
architecture” or “prescribe every step.”

They support a more precise representation:

- mark the architecture separately from the component steps;
- assign confidence/provenance per step;
- treat steps as provisional unless already axiom-clean;
- let the runner replace a step while preserving the route invariant;
- record both faithful execution and evidence-based deviation.

a02J02 retained the Scheffé/Vitali architecture but removed an unnecessary
`|fₙ-g|^p` detour. a03J03 executed the compact-support-density route exactly.
Both are successful uses of a carried route once architecture and steps are
distinguished. This is an amendment to the existing route-override memory, not
a new memory.

## Hand-applicable amendments

### A. Extend the Bochner-integral diagnostic

Target:
`e-codexpilot-derive-integrable-from-nonzero-bochner-integral`

Append two argued instances:

- a02J03 receipt `8bb3a574-ed5c-4317-9fc1-f8f7e69dbecc`:
  interval-integral upper bounds hold vacuously for a locally non-integrable
  spike construction; proposed guard `LocallyIntegrable`;
- a02J07 receipt `3a24af2e-921c-4ef0-8fd3-38d62bf504cd`:
  `gamma'/gamma` is not interval-integrable, so the path integral totalizes to
  zero; proposed guards are interval integrability, absolute continuity, or an
  explicit improper-integral convergence condition.

Set both evidence entries to `:diagnosis-status :argued-not-machine-checked`.
Do not merge the derivative or `ℝ≥0∞` instances into this mechanism-specific
memory.

### B. Add the derivative-totalization sibling

Target: the defect-taxonomy/statement-preflight append-only register created
from pass 32.

Append:

- mechanism `:totalized-derivative`;
- operation `deriv`;
- junk value `0` outside differentiability;
- trigger: formal hypotheses constrain `deriv` but assume only continuity;
- prose mismatch: docstring says `C¹`, binders say `Continuous`;
- evidence receipt `aab1e62e-e494-4437-85e6-ba24598cbdc6`;
- status `:argued-not-machine-checked`;
- repair: require real differentiability, preferably `ContDiff ℝ 1`, before
  using the Cauchy–Riemann bridge.

Keep it a sibling of, not an alias for, `integral_undef`.

### C. Add the extended-bound sibling

Target: the same defect-taxonomy/statement-preflight register.

Append:

- mechanism `:extended-codomain-top`;
- inferred binder type `ℝ≥0∞`;
- degenerate witness `M = ⊤`;
- trigger: `∃ M, ... ≤ M` with no explicit binder type or finiteness premise;
- evidence receipt `a037de1b-ad0d-4971-8641-57564ff71156`;
- status `:argued-not-machine-checked`;
- repairs: bind `M : ℝ≥0` with coercion, or retain `M : ℝ≥0∞` and require
  `M < ⊤`.

Record explicitly that this is not partial-operation totalization.

### D. Refine the documented-route override memory

Target:
`e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`

Append a route representation with:

- `:architecture`: the invariant mathematical decomposition;
- `:steps`: ordered components;
- per-step `:status` such as `:axiom-clean`, `:library-confirmed`,
  `:untested`, or `:superseded`;
- per-step evidence/provenance;
- a requirement that a replacement say which architecture invariant it
  preserves.

Add paired evidence:

- a02J02 receipt `c2f0f1b2-c261-4fa5-8ecb-bcb35aa9529f`: architecture retained,
  one step superseded;
- a03J03 receipt `1693fc6c-4e50-4c1f-a898-292667f5cb73`: architecture and steps
  executed as written.

Suggested confidence:
`:paired-confirmation-that-architecture-and-steps-need-separate-status`.

### E. Extend installed-source-search evidence

Target:
`e-codexpilot-prefer-installed-source-search-when-the-library-namespace-is-guessable`

Append:

- a92J06 receipt `adee5cd0-e7dc-4b63-8111-f137cf598c67`: Loogle URL queries
  were rejected and LeanSearch was irrelevant; local source found the contour
  components;
- a93J06 receipt `380a7899-c60d-4550-8128-a2bf14a6bb49`: public search found no
  useful direct theorem; local source found all four components;
- a93A04 receipt `2a8fa1b7-846d-4a71-b001-8f4fa244c5fa`: local source exposed
  both the available variation direction and the absent converse.

Add a measurement caveat: “components arm” must distinguish public declaration
search from installed-source search because they have different failure modes.

### F. Create the a93A04 frontier attachment

After promotion, attach
`order-a-disjoint-interval-family-into-a-variation-partition` to
`math/measure-integration-api`, preserving:

- the exact schematic residual;
- the two axiom-clean prefix declarations;
- receipt `2a8fa1b7-846d-4a71-b001-8f4fa244c5fa`;
- commit `13a37c420e4cd812d5a8e3ee31e1e133a549a246`;
- status `:gap-open`.

## Subject handles

Reused:

- `M-codex-sorry-loop`;
- all nine exact problem IDs;
- `math/measure-integration-api`;
- `math/holomorphic-disk-api`;
- `math/derivative-bounds-api`;
- `math/corpus-trust-protocol`;
- `math-formalization/notation-semantics-traps`.

Minted: none.

Hooks and bodies contain the searchable terms that subjects alone cannot
supply: Scheffé, Vitali, partial fractions, Cauchy–Goursat, compact-support
density, radial integration, `eVariationOn`, `integral_undef`, `deriv`,
`ℝ≥0∞`, inferred binder type, `le_top`, and prose/formal mismatch.
