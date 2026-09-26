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

---

# Rules for building a ConstructionTarget

Added 2026-08-01 by claude-9. Every rule below is a defect this loop actually
hit, not a precaution. Include all four in the dispatch packet.

## R1. It must be in the build graph, or it is worthless

Until 2026-07-30 there was no `lean_lib` stanza for `ConstructionTargets`, so
the modules were not on the module path: `import ConstructionTargets.X` failed
with "unknown module prefix" and no oleans were ever produced. Lemmas were
proved, gated, and reported clean while being **unreachable from the problems
they were built for**. `YoungL2` had the identical defect and a94J04's runner
hit it on 2026-07-31.

**Gate:** `lake build ConstructionTargets.<Module>` must exit 0 *and* the
packet must require confirming the `[[lean_lib]]` stanza plus the existence of
`.lake/build/lib/lean/ConstructionTargets/<Module>.olean`. A module that
compiles standalone but is outside the build graph looks clean and is invisible.

## R2. MOVE declarations, do not COPY them

`ConstructionTargets/LusinN.lean` was created by copying
`problems/a95A02/lean/Main.lean`. The result: **17 shared declaration names,
all 17 with byte-identical proofs, and a95A02 does not import LusinN.** Two
independent copies, so a fix applied to one silently does not reach the other.
Its file docstring still opens "APM a95A02", which is how the copy is
detectable.

**Gate:** before committing, grep every new top-level name against the other
ConstructionTargets modules *and* against the source problem file. If the
source problem keeps its own copy, the target has not been extracted — it has
been duplicated. Preserve statement text byte-for-byte by `open`ing the shared
namespace rather than qualifying names, so consumer statements do not change.

## R3. Prove the general theorem is true before asking for it

Ground control dispatched a radial-majorant a.e. convergence theorem under
`LocallyIntegrable f`. It is **false**: `exp(x²)` is locally integrable and its
Poisson convolution diverges, because the Poisson kernel decays only like
`1/t²`. The runner refuted it in one line and stopped there, correctly.

**Gate:** state the hypothesis the *consumer actually has* (here `MemLp f 2`)
rather than the weakest-looking one. If a packet proposes a bound, name where
the bound comes from; "sup A is finite" was assumed, not derived, and that is
exactly where the statement was false.

## R4. Prose asserting absence goes stale, and stale absence blocks work

Six times this session a problem file asserted a theorem was missing from
Mathlib when it was present, or missing from the repo when a ConstructionTarget
had since supplied it: a94A03's Young, a92J05's and a97A08's Rouché comments,
a95J08's Minkowski belief, a94J04's Lebesgue-differentiation claim, and
a96A04's compact-support note. Runners correctly declined to edit those
comments without earning the claim.

**Gate:** the packet should name the current state of the library explicitly
("VERIFIED ABSENT", "VERIFIED PRESENT at <file>:<line>") so the runner does not
have to trust in-file prose. A dependency edge asserted in a comment is not
evidence; a94A10 was recorded as Rouché-blocked all day and never was.
