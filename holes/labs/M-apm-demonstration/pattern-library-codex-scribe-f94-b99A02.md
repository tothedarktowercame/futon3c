# Codex Scribe pattern library — f94

These patterns were added because the reviewed mathematics-memory search found
nearby Sylow restriction and explicit-cardinality patterns, but no pattern for
lifting a unique Sylow subgroup through a normal inclusion or for reducing
global commutativity through the product of all normal Sylow subgroups.

## math-formalization/lift-sylow-normality-through-a-normal-subgroup

- **Trigger:** A normal subgroup contains the relevant prime power, its Sylow
  subgroup is unique, and the goal concerns a prescribed ambient Sylow
  subgroup rather than merely the subgroup inside the normal subgroup.
- **Move:** Make the unique Sylow subgroup characteristic, map it through the
  normal subgroup's subtype, use characteristic-in-normal to obtain an ambient
  normal subgroup, package the image as an ambient Sylow subgroup by its
  cardinality, and transfer normality to the prescribed Sylow subgroup through
  ambient uniqueness.
- **Why it works:** Characteristicity upgrades normality across the inclusion,
  while the preserved prime-power cardinality identifies the mapped subgroup
  as a full ambient Sylow subgroup. A normal Sylow subgroup makes the ambient
  Sylow type unique, so the conclusion is independent of the chosen Sylow.

## math-formalization/reduce-finite-group-commutativity-to-normal-sylow-factors

- **Trigger:** Every Sylow subgroup of a finite group can be shown normal and
  each Sylow subgroup is commutative, but the target asks for commutativity of
  arbitrary ambient elements.
- **Move:** Build `Sylow.directProductOfNormal` from the family of Sylow
  normality proofs, pull both ambient elements back through the resulting
  multiplicative equivalence, and prove their products equal by function
  extensionality using commutativity in each Sylow coordinate.
- **Why it works:** The direct-product equivalence decomposes every group
  element into its prime-primary coordinates, and multiplication in the
  dependent product is coordinatewise. Equality of every commutative
  coordinate therefore transports back to equality in the ambient group.
