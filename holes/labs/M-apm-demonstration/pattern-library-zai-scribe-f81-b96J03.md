# Pattern library additions — zai-scribe, scribe-reduce on b96J03 (f81)

Created because no existing library pattern fits the mined rules below.
Ingested explicitly by `scripts/apm-ingest-coined-pattern-files.sh`; ids
below are pattern ids for attachment. One mined rule fits the existing
pattern `math-formalization/probe-constant-namespace-qualification` and is
attached there, not re-coined.

## math-formalization/subgroup-membership-lemma-explicit-subgroup-first
Trigger: applying a Subgroup membership lemma (`Subgroup.pow_mem`,
`Subgroup.zpow_mem`) with the membership hypothesis first fails with a
type mismatch that reads like an instance-resolution failure ("hypothesis
has type Prop but expected Subgroup"). Move: these lemmas take the
subgroup explicitly FIRST — `Subgroup.pow_mem H hmem k`. One `#check
@Subgroup.pow_mem` exposes the argument order; the error message's
instance-flavoured wording is a red herring.

## math-formalization/perm-cycle-predicates-no-decidable-route
Trigger: `native_decide`/`decide` on `Equiv.Perm.IsThreeCycle` or
`.IsCycle` for a concrete finite permutation fails with 'failed to
synthesize Decidable', and `fin_cases x <;> native_decide` on a pointwise
statement fails with 'Expected type must not contain free variables'
(because fin_cases leaves the Fin property field as a metavariable). Move:
(a) prove a 3-cycle via `card_support_eq_three_iff.mp (by native_decide)`
— note that iff is root-namespace, not `Equiv.Perm.`; (b) state cycle/orbit
facts as a standalone top-level quantified proposition (`∀ x : Fin n,
∃ k : Fin n, c ^ (k:Z) x = y`) and close it with plain `decide` — the
quantified form decides cleanly while the binder-context form does not;
(c) for IsCycle use an explicit witness whose SameCycle side is that
quantified decidable statement. Also: `Subgroup.closure` has no computable
Decidable instance even on a finite group, so brute-force `native_decide`
on a closure equality is a dead end — do not retry it.

## math-formalization/alternating-group-nonsolvability-witness-transplant
Trigger: proving `¬ IsSolvable (alternatingGroup (Fin n))` for n ≥ 6 in a
Mathlib revision with no packaged A_n-simplicity for n ≥ 6 (only
`alternatingGroup.isSimpleGroup_five`). Two routes. (1) Perfectness:
`commutator_alternatingGroup_eq_top` (5 ≤ card) gives `Group.IsPerfect`,
then `Group.IsPerfect.not_isSolvable` — but the commutator lemma lives in
the ROOT namespace (declared after `end Equiv.Perm`), not `Equiv.Perm.*`.
(2) Witness transplant: transplant the witness computation of
`Equiv.Perm.fin_5_not_solvable` into the larger alternating group by
multiplying each odd witness permutation by an extra transposition on the
surplus points so every witness becomes even while agreeing with the
original on the 5 base points; the key conjugation identity then holds by
`native_decide` on the underlying permutations and the derived-series
induction from `not_solvable_of_mem_derivedSeries` goes through verbatim.

## math-formalization/alternating-group-two-cycle-generation-jordan-route
Trigger: proving that a 3-cycle and a full-length cycle (prime length n)
generate `alternatingGroup (Fin n)` — i.e. `Subgroup.closure {b, c} = ⊤`.
Do NOT brute-force (closure has no Decidable instance) and do not attempt
the hand combinatorics of bootstrapping all 3-cycles from conjugates.
Move: map the closure through the alternating-group subtype into
`Perm (Fin n)`; prove pretransitivity at ONE base point via
`MulAction.isPretransitive_iff_base` (the orbit of a point under the
n-cycle powers enumerates all points — the standalone quantified orbit
statement proved by decide); upgrade to preprimitive with
`MulAction.IsPreprimitive.of_prime_card` when the degree is prime; close
with `Equiv.Perm.alternatingGroup_le_of_isPreprimitive_of_isThreeCycle_mem`
(Jordan's theorem) once the 3-cycle is verified; recover H = ⊤ by
`Subgroup.map_subtype_inj` and `Subgroup.range_subtype` (there is no
usable `Subgroup.map_top`; use `← MonoidHom.range_eq_map`).
Caution: `MulAction.isPretransitive_iff` is a DIFFERENT lemma (a
stabilizer-transfer iff for already-transitive actions) — the base-point
form is `isPretransitive_iff_base`.

## math-formalization/instance-argument-transfer-lemmas-need-haveI
Trigger: pointwise application of a lemma whose solvability/structure
hypothesis is an instance argument (e.g. `solvable_of_surjective hf hsol`)
fails with 'Function expected' or a failed instance search, especially
when the hypothesis comes from a `def`-wrapped predicate on a presented
group. Move: expose the hypothesis with `haveI : IsSolvable G := hG` (or
`letI`) before the call; contravariant transfer of a negative property
across a surjection then applies cleanly (`solvable_of_surjective` moves
solvability from target back to source, so target nonsolvability gives
source nonsolvability).
