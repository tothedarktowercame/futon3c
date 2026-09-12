# Codex Scribe pattern library — f206

These patterns cover formalizing half-line Green-operator contraction
arguments and the converse representation needed to upgrade fixed-point
uniqueness to uniqueness among bounded classical solutions.

## math-formalization/use-c0-on-a-closed-domain-for-decaying-fixed-points

- **Trigger:** A contraction argument is posed for continuous functions on an
  unbounded closed domain, with both a sup-norm bound and decay at infinity,
  and a bespoke complete metric space appears necessary.
- **Move:** Use `C₀(X, E)` as the ambient complete normed space, restrict the
  contraction to a metric closed ball, and prove `CompactIccSpace X` when
  needed to rewrite `cocompact` decay as an order-filter limit.  Transport
  pointwise bounds through the bounded-continuous-function coercion and lift
  pointwise difference estimates to the sup metric.
- **Why it works:** `C₀` already packages continuity, vanishing at infinity,
  completeness, and the sup norm.  A closed ball remains complete, so the
  analytic work reduces to showing that the operator maps into `C₀`, maps the
  ball to itself, and contracts its inherited metric.

## math-formalization/prove-converse-green-representation-before-claiming-bvp-uniqueness

- **Trigger:** Banach's theorem gives a unique fixed point of an integral
  operator, but the target theorem asks for uniqueness among bounded
  classical solutions of the corresponding boundary-value ODE.
- **Move:** Prove that every bounded classical solution satisfies the same
  Green representation.  On a half-line, derive a finite-basepoint variation
  formula, then send the basepoint to the boundary using one-sided limits and
  the boundary value; package the restricted solution into the same closed
  ball and invoke fixed-point uniqueness there.
- **Why it works:** Fixed-point uniqueness only compares fixed points.  The
  converse Green identity supplies the missing implication from the broader
  ODE solution class to the operator's fixed-point class, after which subtype
  equality and evaluation yield pointwise equality.

## math-formalization/split-half-line-green-kernels-into-moving-primitives

- **Trigger:** A Green kernel contains an absolute value such as
  `exp (-|x-y|)`, and direct proofs of continuity, differentiability, or decay
  of its parameterized improper integral are awkward in Mathlib.
- **Move:** Split the domain at the parameter and rewrite the operator as
  finite interval primitives plus a fixed improper integral.  Differentiate
  the moving primitives with the interval-integral fundamental theorem,
  prove the tail identities separately, and recombine algebraically.
- **Why it works:** The split removes the absolute value branch and isolates
  the improper part from the moving endpoint.  Existing interval-integral
  derivative and continuity APIs then handle the parameter dependence, while
  scalar exponential estimates handle the tail.
