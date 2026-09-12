# Codex Scribe pattern library — f99

These patterns were added because the reviewed mathematics-memory search found
no coherent parent for either endpoint-derivative recovery in a weak half-line
ODE encoding or uniform-mesh globalization of local Picard solutions.

## math-formalization/recover-one-sided-ode-data-at-a-closed-endpoint

- **Trigger:** A solution is continuous on a closed half-line and differentiable
  only in its interior, but a closed-interval uniqueness theorem requires a
  one-sided derivative at the endpoint.
- **Move:** Express the interior derivative as the continuous vector field
  along the solution, prove that expression tends to its endpoint value, and
  use the endpoint derivative-from-limit theorem to obtain
  `HasDerivWithinAt` on the half-line. Feed this endpoint fact and ordinary
  interior derivative facts to the closed-interval uniqueness theorem.
- **Why it works:** Continuity of the trajectory and vector field supplies the
  missing limit of derivatives. The one-sided derivative theorem packages
  exactly the regularity needed at the endpoint, avoiding an artificial
  extension to all real times or a separate limiting Gronwall argument.

## math-formalization/globalize-uniform-local-ode-solutions-by-mesh-gluing

- **Trigger:** Picard--Lindelof supplies solutions only on bounded intervals,
  while the goal asks for one solution on an unbounded half-line and no
  packaged global-existence theorem matches the predicate.
- **Move:** Prove a state-independent positive lifespan by allowing the state
  ball and vector-field bound to grow with the initial state. Choose one local
  segment at each point of a fixed time mesh, define successive endpoint
  states recursively, and select the containing segment using a floor-based
  mesh index. Prove adjacent segments agree at mesh points and glue their
  one-sided derivatives there.
- **Why it works:** A uniform positive lifespan prevents Zeno accumulation, so
  the fixed mesh covers the half-line. Recursive endpoint matching gives value
  compatibility, and equality of the autonomous vector-field derivative at a
  shared value makes the left and right derivative germs glue to a full
  derivative.
