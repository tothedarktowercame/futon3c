# Codex Scribe pattern library — f90

These patterns were added because the reviewed mathematics-memory search found
no match for proving abelianness or a maximality property of a fixed field via
the commutator subgroup and Mathlib's quotient form of finite Galois
correspondence.

## math-formalization/prove-a-fixed-field-abelian-through-the-commutator-quotient

- **Trigger:** A finite Galois fixed field must be shown abelian, and its
  automorphism group is exposed only through an equivalence with a quotient of
  the ambient automorphism group.
- **Move:** Put the commutator subgroup in a local name, install its normality
  and the fixed field's `IsGalois` instance, obtain
  `IsGalois.normalAutEquivQuotient`, prove the quotient commutative with
  `Subgroup.Normal.quotient_commutative_iff_commutator_le`, and transfer
  commutativity along the equivalence's surjective multiplicative homomorphism.
- **Why it works:** The commutator subgroup is normal and contains exactly the
  obstruction to commutativity.  Quotienting by it is therefore commutative,
  and a surjective multiplicative map transports that elementwise commutation
  law to the automorphism group of the fixed field.

## math-formalization/turn-abelian-subextension-maximality-into-commutator-containment

- **Trigger:** To prove that every abelian Galois intermediate field lies in a
  chosen fixed field, a direct restriction-and-embedding argument becomes
  dominated by automorphism transport.
- **Move:** Rewrite intermediate-field inclusion with
  `IntermediateField.le_iff_le`, then rewrite the resulting fixing-subgroup
  containment using
  `Subgroup.Normal.quotient_commutative_iff_commutator_le`.  Use
  `IsGalois.normalAutEquivQuotient` for the candidate subextension and transfer
  its commutative automorphism law along the inverse equivalence by
  `Function.Surjective.mul_comm`.
- **Why it works:** Finite Galois correspondence reverses field inclusion into
  fixing-subgroup inclusion.  An automorphism quotient is commutative exactly
  when the ambient commutator lies in its kernel subgroup, so the desired field
  maximality becomes the standard universal property of the commutator.
