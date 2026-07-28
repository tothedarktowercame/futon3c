# S2 construction targets

Ranking is lexicographic by the preregistered criteria. `U` is the number of
distinct problem files whose active holes name the target, `E` is standalone
statement extractability (0--2), and `M` is proximity to an existing Mathlib
API (0--2). The scores are triage judgments, not proof-cost estimates.

## 1. Young convolution, L¹ * Lᵖ (with the L² pilot instance)

**Score `(U=5, E=2, M=2)`.** Missing statement, in the already extracted L²
form: `Integrable g → MemLp f 2 → eLpNorm (fun x ↦ ∫ y, g (x-y) • f y) 2 ≤
ENNReal.ofReal (∫ y, ‖g y‖) * eLpNorm f 2`; the broader recurring form replaces
`2` by `p` under `1 ≤ p`, `p ≠ ⊤`. It occurs in `a94A03`, `a94J04`, `a95A03`,
`a95J08`, and `a96A04`; the curriculum specifically records the local
`YoungL2.lean` instance as unblocking `a95J08` and `a96A04`. Build first in
`ConstructionTargets/YoungConvolution.lean`, against
`Mathlib.Analysis.Convolution` and the existing `MemLp`/translation-invariance
API. This is a clean reusable inequality, has the widest observed reach, and
the two exact pilot declarations already exist in `YoungL2.lean`.

## 2. Equality case of Schwarz's lemma

**Score `(U=2, E=2, M=2)`.** Missing statement: for a holomorphic
`h : ℂ → ℂ` mapping the unit disk to itself, with `h 0 = 0` and
`‖deriv h 0‖ = 1`, there is `α : ℝ` such that
`h z = exp (I * α) * z` throughout the disk. It unblocks equality and
subordination steps in `a95A08` and `a95J01` (four active occurrences).
Place the pilot in `ConstructionTargets/SchwarzEquality.lean`, next to imports
for Mathlib's existing `Complex.norm_le_norm_of_mapsTo_ball`. The inequality
API and both downstream statements are already explicit, leaving one sharply
bounded equality characterization.

## 3. Components of a polynomial lemniscate complement

**Score `(U=2, E=2, M=2)`.** Missing statement:
`Nat.card (ConnectedComponents ↥((lemniscate p)ᶜ)) ≤ p.natDegree + 1`.
The identical declaration occurs in `a00J04` and `a01A08`; both files say
their analytic steps are complete and the remaining boundary is the finite
component/injection bookkeeping. Put it in
`ConstructionTargets/LemniscateComponents.lean`, working outward from
`IsOpenMap.enatCard_connectedComponents_le_encard_preimage_singleton` and the
existing `CardComponents` API. It ranks above Rouché because the statement is
duplicated verbatim and the comments identify a nearby library theorem.

## 4. Rouché root-count transfer

**Score `(U=2, E=2, M=1)`.** Missing reusable statement, schematically: if
`f` and `g` are holomorphic on a neighbourhood of a closed disk and
`‖f-g‖ < ‖g‖` on its boundary, then their root multisets have equal cardinality
inside the disk. It is named by the root-count boundaries in `a92J05` and
`a94A10`. Start `ConstructionTargets/Rouche.lean`, near Mathlib's contour,
winding-number, and polynomial-root APIs. The dependency is clean and shared,
but it is a larger complex-analysis extension than the three targets above;
the `a94A10` use may additionally need a local-mapping corollary.

## 5. Radial integration and power integrability on ℝ³

**Score `(U=1, E=2, M=1)`.** A useful first statement is a radial reduction for
`E = EuclideanSpace ℝ (Fin 3)`, sufficient to prove
`∫ x : E, 1 / (‖x‖^(3/2) * (1+‖x‖^2)) = 2*π^2`; its companion should turn the
same reduction into `MemLp f_radial p ↔ 3/4 < p ∧ p < 3/2`. These two holes are
in `a95A03`. Use `ConstructionTargets/RadialIntegrationR3.lean`, extending the
existing Euclidean volume/integrability API. It has only one direct problem,
but outranks the other one-off gaps because both downstream statements and
the proved one-dimensional integral building block are explicit.
