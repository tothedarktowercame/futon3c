# Candidate methods induced from the topology collection

Status: analyst drafts, not new canonical patterns or recorded use edges.
All sources refer to apm-lean commit
`f053ab5936725f2e41c937cfff82277f3a23c868`. `topology-census.json`
contains the complete population, imports, declaration locations, source hashes,
and sanitized ledger dispositions. `topology-induction.json` adds exact source
excerpts for the following witnesses and a coverage matrix for all 61 roots.
Directly inspected witnesses are distinguished from transitive availability.

## T1. Compare generating relations before transporting quotient invariants

**When:** two presentations have the same carrier (or a specified equivalence),
corresponding generating relations, and quotient topologies.
**Construction:** prove relation equivalence; lift it through reflexive,
symmetric and transitive closure; define mutually inverse quotient maps;
prove continuity and representative equations; then transport the invariant.
Keep the basepoint equation if the next step concerns fundamental groups.

**Repeated witnesses:** t96A03 compares coordinate gluing relations, proves
`EqvGen` equivalence and constructs a quotient homeomorphism; t01J03 repeats
this for its spiral presentation. t02A03 uses
`PointWedgeQuotient.quotientHomeomorphOfRelIff` after proving correspondence to
the production relation. These are distinct problem consumers, though the
supporting infrastructure is shared.

**Failure contrast:** one-way preservation descends a map but supplies no
inverse. An abstract unbased invariant isomorphism does not supply the chosen
basepoint identification. This specializes `math-informal/transport-across-isomorphism`:
it explains how to obtain the required isomorphism, not merely how to use one.

## T2. A null bad set cannot exhaust a positive-measure target

**When:** a bad-value set is null for the same measure under which an available
test set has positive measure. For simultaneous constraints require a countable
family, with measurable/nullity facts in the actual coordinate model.
**Construction:** establish bad-set nullity; take a countable union if needed;
contradict containment of the positive-measure test set; choose an outside
point; translate avoidance into the required regularity or nonsurjectivity.

**Repeated witnesses:** t93A04 composes with a projection, applies nullity of
critical values and chooses an avoided value; t92J07 takes a countable union
of critical-value sets and uses the unit ball. t94J05 and t95J04 apply
`FixedChartNullForcesNonsurjectivity.not_surjective_of_fixedChart_coordinateNull`.
The fixed-chart route uses positive measure of an open Euclidean chart image;
it does not assume a global manifold measure. The final encoded predicate in
t93A04 should not be mistaken for an independently constructed manifold.

**Failure contrast:** an uncountable union of null singletons can exhaust the
target. Nullity of critical values is different from nullity of the whole
image. A boundary chart requires appropriate relative geometry. Broad
contradiction/local-to-global patterns do not state these decisive conditions.

## T3. A retraction forces injectivity of a functorial invariant map

**When:** maps i and r satisfy r∘i=id, the invariant respects identity and
composition, and the induced map of i can be shown noninjective.
**Construction:** apply the invariant to the left-inverse equation; infer
injectivity; exhibit a nonzero kernel element (or a nontrivial source and
trivial target); contradict injectivity.

**Repeated witnesses:** t01A03 proves `H1map_injective_of_retraction` and
`noRetraction_of_H1map_zero`; t91J02 proves the corresponding fundamental-group
injectivity and excludes a circle retract of a simply connected sphere.

**Failure contrast:** a zero map out of a zero group gives no contradiction.
The obstruction requires a proved nonzero source witness. t00A04 is a useful
boundary case: its placeholder-free theorem assumes non-nullhomotopy of the
identity and proves a conditional reduction. It does not itself establish
that obstruction for every manifold in the original informal statement.
`math-strategy/structural-obstruction-as-theorem` concerns failure of a proof
method; lexical overlap in “obstruction” is not enough to attach it here.

## T4. Reconstruct a source equivalence from target-natural mapping equivalences

**When:** equivalences between functor categories are available for every
relevant target, together with compatibility under postcomposition, including
the inverse comparison needed for the other round trip.
**Construction:** evaluate at identity functors to define source maps; use
postcomposition comparisons and unit/counit equations to identify their round
trips after mapping; reflect the isomorphisms through fully faithful functors;
package a source equivalence. For group extraction separately identify the
chosen object and its endomorphism group.

**Repeated witnesses:** `GenericTargetNaturalSourceReconstruction` is imported
transitively by t00A01 and t02A03; `TwiceSpiralSourceEquivalence`, used by t01J03,
contains the specialized construction with the same two round trips.
The generic and specialized source bodies were inspected; the two generic
consumers are not two independent implementations of the generic theorem.

**Failure contrast:** equivalence of mapping categories for one fixed target
is insufficient. A correspondence on objects without naturality does not
supply the round-trip isomorphisms. This is more specific than merely
“verify a universal property.”

## T5. Compactness and separation turn a continuous bijection into a homeomorphism

**When:** the source is compact, the target Hausdorff, and the specified map is
continuous and bijective.
**Construction:** images of closed subsets are compact and hence closed;
identify inverse preimages with those images; deduce inverse continuity and
package a homeomorphism with the specified forward map.

**Repeated witnesses:** t92J02 proves `homeomorph_of_compact_to_T2` by this
closed-image argument. t94J02 applies the continuous-bijection characterization
to the identity between two topologies and deduces equality of topologies.
This includes ordinary library-based completed problems, not only the large
ConstructionTargets developments.

**Failure contrast:** the identity from discrete real numbers to usual real
numbers lacks compact source and has discontinuous inverse. The bijection
from a discrete two-point space to an indiscrete one has compact source but
non-Hausdorff target and again discontinuous inverse.

## T6. Identify the actual comparison map before using an abstract isomorphism

**When:** an invariant comparison is known abstractly, but the consumer needs a
specific inclusion, generator image, or naturality equation.
**Construction:** define the actual map; prove its chain-level square or
factorization (using a quotient universal property where required); identify
its induced map with the hom of the constructed isomorphism; only then use
invertibility and transport the designated class.

**Repeated completed library witnesses:** `LocalHomologyNeighborhoodExcision`
proves `neighborhoodInclusion_chain_eq` before `neighborhoodInclusion_H1_isIso`.
`CoefficientRelativeSmall` proves `inclusion_square` and `comparisonIso_hom`;
`CoefficientExcision` proves `composite_eq` and `homologyIso_hom`.
These are reviewed completed stages across the two ledgers, partly in one
shared program, not three independent campaigns. The first has no completed
consumer in this census; the latter two are transitively available to t94J08.
Their source equations, rather than import counts, support the method.

**Failure contrast:** knowing two objects are isomorphic does not make every
map between them invertible or preserve a chosen generator. Generic transport
and diagram chasing provide parents; the actual-map equality is the middle
level obligation that the Student should expose.

## Induction limits and next promotion step

The census examines the whole collection mechanically; these six candidates
come from detailed inspection of multiple witnesses within it. It does not
semantically review every proof body or claim that these six exhaust the
collection. The explicit coverage matrix leaves other roots unassigned.
A dependency is evidence that infrastructure is available, not that a named
method was invoked in the final proof. Repeated source patterns suggest
candidate abstractions; independent examples and negative applicability tests
are still required before canonical publication.

A useful next extraction batch would give each candidate the exact definitions,
hypotheses, provided construction, failed applicability examples, and reviewed
implementation links. Completed-library links may precede completed-problem
uptake. Keep those evidence types separate when building the pattern cascade.
