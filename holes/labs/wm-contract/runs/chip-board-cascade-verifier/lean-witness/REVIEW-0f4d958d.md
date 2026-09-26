# 0f4d958d review and completed snapshot correction

Found and repaired a digest/execution capture race. Although execution used one
captured registry, the digest was computed first by reading the registry atom
once per chip. A registration between digesting and snapshot capture therefore
made the reported digest describe different functions from those executed.

The new regression reproduces this exact interleaving at the real digest
boundary. Before repair it reports one failure: expected REPORT, observed
COMMIT, while the old registry digest is retained. After repair, both digest
and execution use the same captured map. The one-argument verbs-digest API is
preserved and now also captures once; its new two-argument arity hashes the
explicit execution snapshot.

Validation:
- Before: 2 tests / 6 assertions, 1 failure (digest/execution race).
- After: 2 tests / 6 assertions, 0 failures/errors. The second test changes
  YIELD during the preceding REPORT and verifies the later chip stays on the
  captured implementation. Isolated atom and no-op/probe handlers only.
- Inbox Lean witness: zero errors, 17 axiom checks without sorryAx, 17 readback
  lines match. Cascade: zero errors, 9 axiom checks without sorryAx, 14 lines
  match. Both source/fixture inventories unchanged during their checks.
- Lint 0/0, parens OK, diff check clean.

No Lean change was necessary: cascade Executes already takes a single registry
parameter. Snapshotting now corresponds to that part of the model. It still
requires that parameter to be the declared semantics to infer no-act safety.
Arbitrary pre-run replacements have not thereby become proved implementations.
The runtime statuses remain pending; separately scoped witness results are
validated. No live reload, run, registry mutation, historical rewrite or queue
operation occurred.
