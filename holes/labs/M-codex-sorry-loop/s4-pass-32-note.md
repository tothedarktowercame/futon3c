# S4 scribe pass 32

- Mode: defect-class audit; drafts only; no store-write endpoint was called.
- Solve-lane yield: 0.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 2.
- Total yield: 2 drafts.

All five supplied receipts resolved. I locally dry-run harvested the two
interleaved rollout sessions and body-verified the relevant turns:

- a01J01: `e-codexroll-019fa2c1-t048`, `t049`;
- a01J06: `e-codexroll-019fa2c1-t050`;
- a01J02: `e-codexroll-019f9b12-t050`;
- a02J01: `e-codexroll-019f9b12-t051`;
- a95J03's original placeholder finding: `e-codexroll-019f9b12-t018`.

The a01J02 turn is already present in the store. The a01J06 and a02J01 turns
were body-verified in the local dry-run harvest but still await ground-control
store harvest. I did not invoke the harvester's commit mode.

## Defect taxonomy correction

The proposed headline “five problems, one `integral_undef` root cause” is not
supported by the receipts. The five rows contain at least three different
semantic mechanisms:

1. **Totalized partial operation:** a non-integrable Bochner integral is defined
   as zero, so an upper-bound hypothesis may hold vacuously. This is the
   mechanism in a01A10, a01J01, a02J01, and the earlier a95A08 Jensen defect.
2. **Constant placeholder:** a95J03 defines `windingNumber` as zero and proves a
   bound on that constant. This does not involve `integral_undef`.
3. **Missing model invariant:** a01J06 leaves `zeros` unrelated to `f` and
   `zeroCount`; its axiom-clean countermodel exploits that independence. The
   constant `zeroCount` is also a placeholder, but the false summability target
   specifically follows from the absent link.

a01J02 is a fourth, pre-formal class: its statements do not elaborate. These
classes should remain distinct because their remedies and gates differ.

## Verdict 1: the `integral_undef` diagnostic

The diagnostic is real and now multi-problem, but it should amend
`e-codexpilot-derive-integrable-from-nonzero-bochner-integral`, not become a
duplicate memory.

Before accepting or proving a finite upper bound on a Bochner integral over a
possibly infinite measure space:

1. check whether integrability is assumed or derivable;
2. test a simple non-integrable witness;
3. reduce the integral using `integral_undef`;
4. check whether the hypothesis collapses to `0 ≤ C`;
5. distinguish the intended improper/extended integral from Mathlib's
   totalized Bochner integral.

The evidence strength must remain per case: a01A10 is machine-checked; a01J01
and a02J01 are argued diagnoses in their receipts, not machine-checked
refutations.

## Verdict 2: clean proofs over placeholders

There is a capturable check, and it already exists as
`e-codexpilot-inspect-placeholder-definitions-before-claiming-mathematical-content`.
I fetched that exact memory before deciding not to duplicate it.

For a surprisingly cheap proof:

- unfold the central subject definition;
- flag a constant/literal body, unused parameters, `True` model fields, and
  theorem hypotheses unused by the proof;
- compare the dependency path of the formal statement with the mathematical
  prose;
- classify the result as `closed-over-placeholder`, not mathematically closed.

These are useful static and reviewer triggers. They cannot prove semantic
adequacy: a human still has to decide whether the definition faithfully models
the intended object. Thus the right gate is a machine-assisted flag followed
by independent semantic review, not an automated theorem-validity verdict.

a95J03 should be removed from the mathematical clean count or reported on a
separate axis: it is formally clean and semantically placeholder-bound.

## Verdict 3: elaboration before runner time

This is capturable and produced one new trajectory draft:
`preflight-file-elaboration-before-sorry-accounting-or-runner-dispatch`.

The exact file must elaborate under the real project before sorry accounting or
dispatch. A failed preflight must then distinguish source defects from build
state. In particular, a missing compiled dependency is not a source defect:
build the named dependency and rerun the exact file before classification.

The receipt directly supports the a01J02 source-defect case. Ground control's
4-of-40 census and a02J06 dependency case are retained here as owner-reported
audit context, not promoted as independently cited facts in the draft.

## Hand-applicable amendments

### A. Broaden the Bochner-integral diagnostic

Target:
`e-codexpilot-derive-integrable-from-nonzero-bochner-integral`

Append:

- trigger: any finite equality or upper bound involving a Bochner integral on a
  possibly infinite measure space when integrability is absent;
- diagnostic: test a non-integrable witness and simplify with `integral_undef`;
- failure signature: a substantive analytic hypothesis becomes `0 ≤ C`;
- repair options: add `Integrable`, use an extended/nonnegative integral where
  appropriate, or restate an improper integral as a `Tendsto` limit;
- evidence:
  - a01J01 receipt `319d80df-a964-43a6-960d-5311b3946a53`
    (`:diagnosis-status :argued`);
  - a02J01 receipt `659233e8-9080-464b-9ea9-3f4e957f6556`
    (`:diagnosis-status :argued`);
  - retain the already-applied a01A10 machine-checked evidence.

Do **not** attach a95J03 or a01J06 as `integral_undef` instances.

Suggested confidence:
`:multi-problem-diagnostic-with-mixed-machine-checked-and-argued-instances`.

### B. Add the a95J03 challenge to the placeholder memory

Target:
`e-codexpilot-inspect-placeholder-definitions-before-claiming-mathematical-content`

Append:

- challenge receipt `3e99ea26-1b49-4981-948d-84e1716e6e68`;
- observed status: zero sorries and no `sorryAx`, but
  `windingNumber (_g) (_z) := 0`;
- proof signature: `rw [windingNumber]; omega`, with the semantic parameters
  and hypotheses unused;
- required result classification: `:closed-over-placeholder`;
- reviewer trigger: unfold central definitions when the proof is far shorter
  than the claimed mathematics, and inspect literal bodies and unused inputs.

This is confirming evidence for the existing rule, not a new rule.

### C. Extend the divergence taxonomy

Target:
`e-codexpilot-inspect-conclusion-binding-structure-before-claiming-mathematical-content`

Keep the existing two witnessed members—stub definition and under-constrained
conclusion—and append:

- `:totalized-operation-with-missing-domain-precondition`
  (Bochner `integral_undef`);
- `:unconstrained-model-fields`
  (a01J06: `zeros`, `zeroCount`, and `f` lack linking invariants);
- `:statement-does-not-elaborate` as a pre-formal defect, not a vacuity member.

Evidence:

- a01J01 receipt `319d80df-a964-43a6-960d-5311b3946a53`;
- a01J06 receipt `8279ca0a-a275-4f1f-b628-f6d730fc0e6f`;
- a01J02 receipt `4f67ccb0-2721-43b4-8d9a-c164a6da8ffc`;
- a02J01 receipt `659233e8-9080-464b-9ea9-3f4e957f6556`;
- a95J03 challenge receipt `3e99ea26-1b49-4981-948d-84e1716e6e68`.

### D. Add semantic status to the proof-hole meter

Target:
`e-codexpilot-separate-lexical-sorry-count-from-real-proof-hole-count`

Append two cases:

- a01J06: headline sorry delta `-1`, genuine intended-theorem discharge `0`;
  the removed hole was closed over `zeroCount := 0`, while the remaining target
  is machine-refuted under the unconstrained model;
- a95J03: direct holes `0`, `sorryAx false`, but semantic status
  `:closed-over-placeholder`.

Add a fourth reporting axis beside lexical count, direct holes, and dependency
closure:

- `:semantic-status`, with at least `:statement-audited`,
  `:closed-over-placeholder`, `:under-constrained`, `:ill-typed`, and
  `:not-yet-reviewed`.

This axis is reviewer-attested; it must not be inferred from axiom cleanliness.

## Subject handles

Reused:

- `M-codex-sorry-loop`;
- `a01J02`;
- `a01J06`;
- `math/corpus-trust-protocol`;
- `math-formalization/notation-semantics-traps`.

Minted: none.

The handles are load-bearing, but the hooks and bodies also contain the
searchable terms `integral_undef`, Bochner integral, placeholder, unused
parameters, elaboration, missing dependency, `ProblemData`, `zeroCount`, and
unconstrained fields.
