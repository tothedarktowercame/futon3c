# S4 scribe pass 30

- Mode: three proving runs; drafts only; no store-write endpoint was called.
- Run A: a01A09 solved axiom-clean, sorries 3 → 0.
- Run A receipt: `558c5f8f-8642-47f0-8c13-1df248d64282`.
- Run A turn-rounds: `e-codexroll-019fa2c1-t043`,
  `e-codexroll-019fa2c1-t044`.
- Run A commit: `622a238329d7331fb72eeef35b85f12d3788e460`.
- Run B: a01A07 closed axiom-clean on attempt 3.
- Run B receipts: `6ea3677b-5eed-4538-b59e-cbf01f8274ed`,
  `de5437c3-1f3d-417b-882a-e1d70883f242`.
- Run B turn-rounds: `e-codexroll-019f9b12-t045`,
  `e-codexroll-019f9b12-t046`.
- Run B final commits: `13ce2ee4810cad8045f52cbb3a074d5a59844843`
  through `1fad0c18f87f8139a82b39ebf7bfaf680628b4f7`.
- Run C: a01A10, two targets solved and one target machine-refuted.
- Run C receipt: `e625cf40-4eef-40ae-bc36-1bfa2a239dba`.
- Run C turn-round: `e-codexroll-019fa2c1-t045`.
- Run C commit: `b36ba73d02f99d7aa84d02952f61b2176610d942`.
- Solve-lane yield: 4 drafts.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 0.
- Total yield: 4 drafts.

All four supplied receipts resolved and named the expected problem. The two
interleaved rollout sessions were locally dry-run harvested and every cited
turn body was checked for its problem:

- a01A09 is codex-7 turns 43–44;
- a01A10 is codex-7 turn 45;
- a01A07 attempt 2 is codex-6 turn 45;
- a01A07 attempt 3 is codex-6 turn 46.

Only `e-codexroll-019fa2c1-t043` was already present in the store. The remaining
four turn-rounds are cited precisely but must be harvested by ground control
before promotion. I did not call the harvester's `--commit` mode because this
pass explicitly forbids store writes.

## Yield

Run A yields two complementary solve records:

1. conjugate-exponent Hölder plus local dominated differentiation proves
   right-half-plane Laplace analyticity;
2. an explicit integrability split proves compact-support entireness without
   silently inventing a measurability assumption.

Run B yields the completed solve record corresponding to pass 29's two
frontiers. It proposes a `:resolves` edge to the local-half-disk frontier and a
`:uses` edge to the already completed disk-area prerequisite.

Run C yields the Gaussian Fourier theorem location and normalization route. No
draft presents the false Fresnel set-integral target as solved.

## Verdict 1: what the frontier-memory success teaches

The event is strong evidence, but not yet a general memory about memory writing.
One successful frontier-to-proof loop does not establish that every field in
those records was causally necessary.

It does support a concrete drafting hypothesis:

1. state the exact residual declaration or equation;
2. separate the proved prefix from the unproved bridge;
3. record serial dependencies explicitly;
4. give an ordered candidate route in library-level operations;
5. name available API and known absent packaging;
6. mark the route untested rather than implying completion;
7. put trigger vocabulary in the hook and body.

Those properties were present in both pass-29 frontier memories. The runner
used the local half-disk route directly and used the disk-area record to avoid
rebuilding a discharged prerequisite. That demonstrates usefulness, not yet a
portable causal rule. I therefore drafted the resulting mathematical solve
memory and recorded the frontier-use provenance, but did not mint a
trajectory-lane “how to write frontier memories” rule.

A second frontier closure in a different mathematical terrain, or a contrast
case where an underspecified frontier fails, would justify promoting the
checklist as a general rule.

## Verdict 2: Fresnel junk-value refutation

No new refutation memory is warranted. The technique is already covered by:

- `e-codexpilot-derive-integrable-from-nonzero-bochner-integral`, whose proof
  shape is exactly `integral_undef` plus contradiction with a nonzero value;
- `e-codexpilot-prove-exponential-Jensen-with-integrable-log-and-set-averages`,
  which already records that a nonintegrable Bochner integral's zero value can
  make a plausible statement false;
- the existing notation/semantics-traps pattern.

a01A10 is a particularly clean confirming instance because the runner proved
all three links independently:

1. the Fresnel integrand is not integrable on `Ioi 0`;
2. its Bochner set integral is therefore zero;
3. the claimed classical improper-integral value is nonzero.

The exact-negation theorem confirms that the formal statement—not the
classical mathematics—is false. The correct repair is a `Tendsto` statement
for bounded-interval integrals. I recommend adding this receipt and commit as
a confirming instance to the existing Bochner-integral memory rather than
creating a duplicate.

All three promoted records named above were fetched before this verdict.

## Verdict 3: manual amendments

### A. Proof-hole measurement memory

Target:
`e-codexpilot-separate-lexical-sorry-count-from-real-proof-hole-count`

Add an observed multi-attempt case for a01A07:

- attempt 1: headline sorry delta −2, genuine discharges 0; two declarations
  became direct-sorry-free but retained transitive `sorryAx`;
- attempt 2: headline delta −1, genuine discharge −1, six new axiom-clean
  supporting declarations;
- attempt 3: headline delta −1, but five declarations became newly usable
  because one discharged dependency removed `sorryAx` from four downstream
  declarations.

Refine the measurement rule to report three distinct quantities:

1. direct/lexical proof-hole count;
2. newly axiom-clean declarations;
3. declarations newly usable after transitive dependency closure.

Evidence:

- receipts `57ca09c6-cfd2-441e-a99b-c96f6c2fffaa`,
  `6ea3677b-5eed-4538-b59e-cbf01f8274ed`,
  `de5437c3-1f3d-417b-882a-e1d70883f242`;
- final commits `13ce2ee4810cad8045f52cbb3a074d5a59844843` through
  `1fad0c18f87f8139a82b39ebf7bfaf680628b4f7`.

Suggested confidence: `:multi-attempt-ground-control-verified`.

### B. Evidence-based route override memory

Target:
`e-codexpilot-override-a-documented-proof-route-when-component-evidence-favors-another`

Add three independent confirming instances plus the new direct use:

- a01A04: retained slicing/Wallis but replaced manual Gamma reconciliation
  with exact parity-specific ball-volume API; receipt
  `639e3a13-dc0f-4673-86af-bbf1dbbfc7f7`, commit
  `80af310905a3a3255df7f96e31d9a162da8c0ded`;
- a01A06: replaced the proposed `x⁻³ᐟ⁴` counterexample with `x⁻¹ᐟ²`, using
  `x⁻³ᐟ⁴` only as an entropy dominator; receipt
  `927e38e5-4296-468a-8631-909543486ceb`, commit
  `9ea0efdcf844292928590cf5cf20b94a16f96e75`;
- a01A10: the runner explicitly used the memory to replace a contour plan
  with `fourierIntegral_gaussian` and to reject the false Bochner-Fresnel
  target; receipt `e625cf40-4eef-40ae-bc36-1bfa2a239dba`, commit
  `b36ba73d02f99d7aa84d02952f61b2176610d942`.

Preserve the boundary: a documented route is a hypothesis to test, not a route
to reject reflexively. Override only after bounded component evidence supports
an end-to-end alternative or refutes the target.

Suggested confidence:
`:multi-domain-recurrent-and-runner-consumed`.

### C. Resolve the two a01A07 frontier memories

Target:
`e-codexpilot-lift-the-circle-submean-bound-to-a-disk-area-bound`

- change `:frontier-status` from `:gap-open` to `:resolved`;
- add resolution receipt `6ea3677b-5eed-4538-b59e-cbf01f8274ed`;
- add commits `c5f91b0`, `13b835c`, `01cf573`, `0c2ab50`, `d4e2318`;
- record that six axiom-clean declarations established the polar disk-area
  bridge and `norm_le_area_integral_div_area`;
- add later use evidence from receipt
  `de5437c3-1f3d-417b-882a-e1d70883f242`: the runner treated it as verified
  prerequisite support and did not rebuild it.

Target:
`e-codexpilot-upgrade-diskwise-L1-convergence-to-local-uniform-convergence`

- change `:frontier-status` from `:gap-open` to `:resolved`;
- add resolution receipt `de5437c3-1f3d-417b-882a-e1d70883f242`;
- add final commits `13ce2ee4810cad8045f52cbb3a074d5a59844843`,
  `438f7055132a6031aceab2728e2a4538a6bdea57`,
  `81dccb351d0fdbb87e1bab8ee6c64110d2aa3c1c`,
  `8d547b44df126ab80f3910bf7e39e8636d70bdf4`,
  `1fad0c18f87f8139a82b39ebf7bfaf680628b4f7`;
- record `:use-status :carried-proof`;
- retain the half-disk route as validated rather than `:untested`;
- record that all 17 declarations are axiom-clean and the four formerly
  transitive declarations no longer contain `sorryAx`.

### D. Bochner-integral nonzero/integrability memory

Target:
`e-codexpilot-derive-integrable-from-nonzero-bochner-integral`

Add a01A10 as the converse-direction confirming application:

- prove nonintegrability;
- rewrite the integral to zero with `integral_undef`;
- prove the proposed right-hand side nonzero;
- conclude the exact equality is false.

Evidence: receipt `e625cf40-4eef-40ae-bc36-1bfa2a239dba`, commit
`b36ba73d02f99d7aa84d02952f61b2176610d942`, declarations
`fresnel_integrand_not_integrable`, `fresnel_bochner_integral_eq_zero`,
`fresnel_claimed_value_ne_zero`, and `not_fresnel_integral_statement`.

Extend the boundary to distinguish a totalized Bochner set integral from the
classical improper oscillatory integral it may resemble.

### E. API-compatibility statement audit

Run A adds one useful but still single-instance QA datum: when a current
Mathlib migration changes theorem syntax, compare all signatures and prove any
nontrivial replacement equivalent in Lean. Here
`AnalyticOn ℂ f Set.univ ↔ ∀ z, AnalyticAt ℂ f z` was checked rather than
assumed. I have not proposed a new memory at one instance; retain this in the
QA ledger until a contrast or recurrence justifies it.

## Subject handles

Reused:

- `M-codex-sorry-loop`
- `a01A07`
- `a01A09`
- `a01A10`
- `math/measure-integration-api`
- `math/holomorphic-disk-api`

Minted: none.

All pattern handles were confirmed in the read-only graph export. Searchable
terms—Laplace transform, conjugate Hölder, dominated parametric
differentiation, compact support, Gaussian Fourier transform, Fresnel,
`integral_undef`, diskwise `L¹`, local half-disk, and compact-uniform
convergence—are present in the hooks and bodies.

Every hook gives an actionable route or semantic check beyond its memory name,
and every draft has a nonempty `:how-to-apply`.
