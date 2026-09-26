# a94A09 scribe and promotion pass

This restart pass distills one dispatch:

- job `invoke-1786369654355-3517-0994cab1`;
- runner seat `ams-codex-2`;
- problem `a94A09`;
- commit `22c5b80c064ae36e83a3b8759607ccf430c76169`.

The existence half compiled. The remaining `sorry` is uniqueness.

## Drafts and promoted IDs

| Draft | Level | Confidence | Memory ID |
| --- | --- | --- | --- |
| `shrink-radius-rouche-fixed-point` | strategy | `n=1` compiled | `e-a09-shrink-radius-rouche-fixed-point` |
| `compact-endpoint-limit-for-scaled-fixed-points` | tactic | `n=1` compiled | `e-a09-compact-endpoint-limit-for-scaled-fixed-points` |
| `open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` | open-hunger | `n=1` unmet query | `e-a09-open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` |

## Importability audit

The only reusable theorem named as available is
`ConstructionTargets.Rouche.zeroCountInClosedBall_add_eq`. The two new
compiled lemmas are explicitly marked as trapped in
`problems/a94A09/lean/Main.lean` and as promotion candidates. Their memories
are re-derivation instructions until promotion.

## Near-duplicate check

Store text searches were run before drafting for:

- `zeroCountInClosedBall homotopy invariant`;
- `Rouche scaled fixed point compact endpoint limit`;
- `disk automorphism Schwarz Pick two fixed points`;
- `closed ball fixed point radial contraction`;
- `consultation discard reason`.

The existing
`e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`
is adjacent but not duplicated: it packages fixed-contour slice hypotheses,
whereas the new strategy selects a smaller contour `t < r < 1` to manufacture
strict domination. Existing zero-count memories concern the formerly missing
argument-principle bridge, now supplied by the importable ConstructionTarget.
No memory matched the compact endpoint-limit pattern or the literal
disk-automorphism/Schwarz–Pick/two-fixed-point query.

The desk-research candidate “record consultation discards with reasons” was
declined as a duplicate of
`e-j07-record-consultation-discards-with-reasons`. The current chain is a good
instance—one recalled memory was used and one was ignored with a reason—but it
does not justify another memory.

## Consultation and recall

Dispatch recall surfaced two memories. The runner used
`e-codexpilot-package-every-rouche-homotopy-slice-for-the-argument-principle`
to package the complete strict-scale slice. It ignored
`e-codexpilot-prove-exponential-cubic-injectivity-by-linear-term-domination`
because perturbative injectivity does not address Schwarz–Pick rigidity. The
offered and outcome receipts are respectively
`e-fab2e3d9-6877-444a-9949-a11720305918` and
`e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4`.

## Hunger audit

No result was excluded as degraded-under-load.

| Literal query vocabulary | Result | Grounded later? | Disposition |
| --- | --- | --- | --- |
| `disk automorphisms Schwarz-Pick two fixed points` | no relevant Zulip or Mathlib/corpus bridge | no | open-hunger memory with every literal term tagged |

The dispatch recall query itself was successful and is therefore not hunger.

## Proposed attachments

| Memory | Pattern | Justification |
| --- | --- | --- |
| `shrink-radius-rouche-fixed-point` | `math/holomorphic-disk-api` | It records a compiled holomorphic-disk construction using the importable Rouché zero-count theorem. |
| `compact-endpoint-limit-for-scaled-fixed-points` | `math/holomorphic-disk-api` | It packages the compactness-and-continuity endpoint step for disk self-maps. |
| `open-hunger-disk-automorphism-schwarz-pick-two-fixed-points` | `math/missing-dependency-protocol` | It records an unresolved dependency with literal demand vocabulary and a precise target. |

All attachments remain proposed. Reviewer-only approval calls are listed in
`APPROVALS.md`.
