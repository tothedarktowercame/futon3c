# S4 scribe pass 24

- Mode: per-run cadence; drafts only; no store-write endpoint was called.
- Row: a01A08, durée run 23.
- Turn-rounds: `e-codexroll-019fa2c1-t034` and
  `e-codexroll-019fa2c1-t035`.
- Receipt: `4c54c3ef-f51e-4207-b272-d8411eafd35b`.
- Commit: `c512690e7fa996bb53cba54b56b40fe28b87a2d4`.
- Solve-lane yield: 1 draft.
- Arc-lane yield: 0.
- Frontier-lane yield: 0.
- Trajectory-lane yield: 0.
- Total yield: 1 draft.

All three cited evidence ids resolved before drafting. The proof was also read
from the cited commit rather than from the current worktree.

The solve draft records the completed component-count construction as one
assembly: classify complement components as sublevel or superlevel; use
properness and minimum modulus to place a root in every sublevel component;
use maximum modulus, polynomial growth, and radial exterior path connectedness
to identify a unique superlevel component; then inject into
`roots(p).toFinset ⊕ Unit`.

This does not duplicate the two promoted frontier records. The exact ids
`e-codexpilot-lemniscate-superlevel-preconnected` and
`e-codexpilot-lemniscate-sublevel-components-inject-into-roots` were fetched
successfully. The new draft proposes reviewed `:resolves` links to both and
records their completed assembly.

## Sibling-transplant verdict

No separate arc- or trajectory-lane memory was drafted. A sibling transplant
is a real route in this row, but the evidence supports only one successful
instance. “Second consecutive run with memory use” is not a second sibling
transplant instance; it is evidence that a prior caution affected runner
conduct. Generalising a transplant policy from that would repeat the
two-instance overreach that the used caution memory was created to bound.

The row does support a local verification checklist, retained as provenance in
the solve draft rather than promoted as a general rule:

1. compare source and target statements exactly, not approximately;
2. inspect the transplant diff for deleted or weakened declarations;
3. recompile under the target namespace;
4. re-elaborate the target declaration independently and inspect its axioms;
5. classify the retained proof by its actual content rather than calling all
   successful transplantation “adapter work.”

Another independently witnessed sibling transplant—especially a failed or
unsound one—would provide the contrast needed for a bounded general rule.

The used memory
`e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`
is recorded as used in the solve draft's provenance. The runner used it as a
caution and retained a construction classification; this is not inflated into
a claim that the memory supplied the mathematics.

The draft has an explicit trigger-oriented `:hook` and a seven-step
`:how-to-apply`. The hook describes the roots-plus-one-exterior counting move;
it does not repeat the memory name.

The arXiv results `1003.4567`, `1406.3545`, and `1602.08337` were background
only and did not supply the construction, so none is represented as a proof
anchor.
