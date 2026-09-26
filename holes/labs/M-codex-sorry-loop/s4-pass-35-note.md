# S4 scribe pass 35

- Mode: reachability audit plus two non-duplicate drafts; drafts only; no
  store-write endpoint was called.
- Solve-lane yield: 2.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 1.
- Total yield: 3 drafts.

All six supplied receipt IDs resolved. The a94J08 outcome receipt appeared
during the pass as `e3736f59-5ad3-496c-94f2-4b364a10f738`; it resolves and
names a94J08.

Both rollout sessions were regenerated in dry-run mode. Cited and reviewed
turns were body-verified:

- a95J08: codex-7 turn 63;
- a94J04: codex-7 turn 64;
- a94J08: codex-7 turns 65–66;
- a94A06: codex-6 turn 64;
- a94J02 attempts 1–2: codex-6 turns 65 and 68;
- a94J07: codex-6 turns 66–67;
- a95A02: codex-6 turn 69.

The a94J08 turn and receipt support a solve memory for the
quadratic-quotient/Cauchy-estimate construction. No turn-round was written to
the store by this pass.

## Store checks

The relevant promoted memories were fetched directly from the store before
drafting:

- `e-codexpilot-make-construction-targets-buildable-importable-and-advertised-before-counting-them`;
- `e-codexpilot-treat-not-in-mathlib-comments-as-revision-scoped-search-claims`;
- `e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`;
- `e-codexpilot-preflight-domain-guards-and-inferred-bound-types-in-analysis-statements`;
- `e-codexpilot-bound-automatic-frontier-descent-when-a-leaf-recurses`;
- `e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`;
- all three pass-34 promoted memories.

The live assertion for the construction-delivery memory contains
`construction-target-delivery-chain` and `math/missing-dependency-protocol`.
The live Hurwitz assertion confirms `math/holomorphic-disk-api`.

## Verdict 1: proved but unreachable

This is capturable, and the corpus already contains the correct general rule:
`e-codexpilot-make-construction-targets-buildable-importable-and-advertised-before-counting-them`.
Do not mint a duplicate.

YoungL2 is the second and stronger instance:

- the source contained four axiom-clean declarations and zero sorries;
- axiom census, sorry count, and statement diff all passed;
- no `[[lean_lib]]` exposed the module, so `import YoungL2` failed with
  `unknown module prefix`;
- commit `7958c5371fe4fb0aa0825738963537d5f669e624` added the build entry;
- `lake build YoungL2`, a clean consumer import, and `#check` then succeeded;
- ConstructionTargets and a93J06 regression checks remained clean.

The missing gate is consumer reachability. A reusable formal artifact is not
delivered until:

1. its library is configured;
2. the library builds and emits module artifacts;
3. a fresh consumer imports the intended module;
4. `#check` resolves the promised declaration at the expected type;
5. the consumer can use it without acquiring `sorryAx`.

This should be a release gate for construction targets and root-level support
files, not an ad hoc search instruction.

## Verdict 2: stale pessimistic notes

Both the check and the calibration are capturable, in one rule:

> Treat a “partial support” or “not in Mathlib” note as a revision-scoped
> search claim. Flag the class for verification; do not presume the individual
> note false until installed-source evidence refutes it.

At 4/4 confirmed false instances, this is now a standing check rather than an
emerging caution. The calibration matters because “the class has failed
repeatedly” is evidence for allocating a cheap verification step, not evidence
that every future instance is false.

The verification order should be:

1. translate the note into exact required capabilities;
2. search the installed revision by namespace and declaration fragments;
3. compile a minimal `#check` or use site;
4. only then correct the note or record a genuine boundary.

## Verdict 3: names encoding abandoned routes

This is not a separate defect class. It extends the existing documented-route
override rule.

Declaration names and docstrings are route hypotheses, not semantic
dependencies. When a direct proof replaces the named route:

- preserve a public declaration name if changing it would create needless API
  churn;
- update prose to identify the implemented route;
- record the old name as historical provenance;
- scope the unresolved frontier to the route it actually blocks.

Thus `rouche-root-count-transfer` remains a genuine three-confirmation
frontier, while a94J07 demonstrates that the frontier does not block every
problem whose scaffold happened to name Rouché.

## Additional judgments

### a94A06

No new draft: pass 34 already promoted
`e-codexpilot-derive-improper-integrability-from-a-monotone-antiderivative-limit`,
which records integrability as an output of
`integral_Ioi_of_hasDerivAt_of_nonneg'`. Duplicating it here would inflate the
corpus.

### a95J08 memory use

`scope-guard-against-over-claiming` is a genuine benchmark mode and is distinct
from pass 34's `prevented-misapplication`:

- prevented misapplication blocks an invalid mathematical technique;
- a scope guard blocks an invalid inference about how much progress has been
  made.

Both are prophylactic uses, but the object of prevention differs. In a95J08,
the memories prevented the clean measurability layer from being described as
if the remaining general Young estimate were adapter work.

### Dependency granularity

The a94A03 dependency is on
`a95J08/young_convolution_inequality`, not on the whole a95J08 problem.
a95J08 advanced while that declaration remained open, so the attempted queue
reorder did not unblock a94A03. Cross-problem edges and skip rules should name
the exact declaration.

## Hand-applicable amendments

### A. Strengthen the construction-delivery chain

Target:
`e-codexpilot-make-construction-targets-buildable-importable-and-advertised-before-counting-them`

Append YoungL2 as the second observed instance:

- discovery receipt `55103403-b402-446a-a924-12e4f82a4c1c`;
- producer proof status: four declarations, zero sorries, axiom-clean;
- failure: no `[[lean_lib]]`, `import YoungL2` gives unknown module prefix;
- repair commit `7958c5371fe4fb0aa0825738963537d5f669e624`;
- verification:
  - `lake build YoungL2`;
  - fresh `import YoungL2`;
  - `#check convolution_L2_contraction_of_probability_kernel`;
  - a93J06 elaboration regression;
  - ConstructionTargets build regression.

Add delivery states:

- `:source-proved`;
- `:library-configured`;
- `:module-builds`;
- `:consumer-imports`;
- `:declaration-resolves`;
- `:field-consumed`.

Only the last five make source-proved work operationally reachable; the final
state demonstrates actual payoff.

### B. Promote stale-note verification to a standing check

Target:
`e-codexpilot-treat-not-in-mathlib-comments-as-revision-scoped-search-claims`

Append the four-instance class:

- a03J03 receipt `1693fc6c-4e50-4c1f-a898-292667f5cb73`;
- a94A03 receipt `530247de-a2dc-4207-92e6-ae6cea1b7282`;
- a94J04 receipt `55103403-b402-446a-a924-12e4f82a4c1c`;
- a95A02 receipt `861fcb68-c043-484b-bc26-290d590ae79f`.

Add policy:

- classify `partial support`, `not available`, and `not in Mathlib` prose as
  `:revision-scoped-search-claim`;
- standing action `:verify-installed-revision`;
- calibration `:flag-class-do-not-convict-instance`;
- require exact installed-source or compile evidence before correction.

Suggested confidence:
`:systemic-four-of-four-with-calibrated-instance-review`.

### C. Extend route-override semantics to names

Target:
`e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`

Append a94J07:

- receipt `e6b886ae-3fd4-41aa-94ef-1376dbfaa15c`;
- commit `7ee9c60`;
- declaration names retain `rouche_*`;
- implemented proof is direct Lipschitz domination;
- policy: preserve stable public names, update route prose, and do not infer a
  dependency from the name.

Add frontier scope:

- `rouche-root-count-transfer` blocks the root-count route;
- it does not imply that every Rouché-named scaffold is blocked.

### D. Add the progress-scope guard use mode

Target: the memory-use taxonomy/benchmark register amended in pass 34.

Append:

- mode `:scope-guard-against-over-claiming`;
- used memories:
  - `e-codexpilot-bound-automatic-frontier-descent-when-a-leaf-recurses`;
  - `e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`;
- evidence receipt `afda168b-23c8-4766-aaac-66027a30ca36`;
- prevented inference:
  “clean measurability/existence prefix implies the remaining Young estimate is
  adapter work”;
- proof-carried? `false`;
- plan/claim changed? `true`.

Keep it distinct from `:prevented-misapplication`,
`:scope-boundary-confirmed`, and `:carried-proof`.

### E. Narrow the a94A03 dependency edge

Target:
`e-codexpilot-prove-general-probability-kernel-Lp-contraction-by-integral-Young`

Replace or supersede the problem-level waiting condition with:

- dependency declaration `a95J08/young_convolution_inequality`;
- current status `:open`;
- dependent target `a94A03/gaussConv_Lp_contraction`;
- evidence receipt `afda168b-23c8-4766-aaac-66027a30ca36`;
- partial commit `f8bac9c9b06d3f64052cf857625c4797834ac6b4`;
- observed outcome:
  a95J08 progressed, but the dependency declaration remained sorried, so a94A03
  was not unblocked.

Queue rule: skip/revisit based on the declaration state, not merely whether the
provider problem was dispatched.

### F. Upgrade a94J02 defect evidence

Target: the statement-defect/faithfulness register entry for a94J02.

Append:

- prior status `:argued-not-machine-checked`;
- new status `:machine-checked-refutation`;
- refutation declaration `constructedH_strictMonoOn_counterexample`;
- repaired declaration `constructedH_strictMonoOn_of_isClosed`;
- receipt `16e88b31-0d2d-4cc7-9fd9-ca5b6d5e969b`;
- commit `b7704ad`;
- owner decision now required:
  `:adopt-machine-checked-repair?`, not `:is-diagnosis-credible?`.

Preserve the false original declaration and its remaining sorry until that
decision is made.

## Subject handles

Reused from live store assertions and resolving promoted memories:

- `M-codex-sorry-loop`;
- `a94J02`;
- `a94J07`;
- `a94J08`;
- `math/corpus-trust-protocol`;
- `math-formalization/notation-semantics-traps`;
- `math/derivative-bounds-api`;
- `math/holomorphic-disk-api`;
- `math/entire-and-singularity-api`;
- `math/missing-dependency-protocol`;
- `construction-target-delivery-chain`.

Minted: none.

Hooks and bodies include the searchable terms `lean_lib`, unknown module
prefix, import smoke test, axiom-clean but unreachable, partial support,
revision-scoped claim, Rouché, Lipschitz domination, machine-checked
counterexample, and repaired statement.
