# N1 cascade board witness — 2026-09-12

Status vocabulary update: new certificates now carry
`:witnessed-under-declared-registry`, scoped to `:lean-model`, with the
semantic assumption and `:runtime-correspondence :not-proven` explicit.
See [the shared status note](../../CHIP-BOARD-WITNESS-STATUS-2026-09-12.md).
Earlier pending-state reports and logs below remain historical evidence.

Mathlib commit `e407ec20cb` adds
`DarkTower/WarMachine/CascadeVerifierBoardWitness.lean`.

## Proven and checked

The finite five-chip board contains no ZAP. Under the declared verb semantics,
every emitted effect of every finite execution prefix is in
`{observe, verify-request, report, yield-turn, typed-none}`. The theorem derives
this subset by cases on the implementations' effect kinds and board membership;
it does not assume no-act as part of trace validity. It overapproximates routes,
so the invariant holds on all routes under those semantics. Separate fixture
proofs check the selected wires and the four-row execution.

The fixed readback has 324 debt items and one verify-request for
`wm-choice/c-grain`, with its retained pointer, basis SHA and freshness.
The Clojure checker reconstructs the consumed input from retained
`:final-state :shelf-debt`, calls the actual board with a no-op effect handler,
and compares the **whole trace (including effects) and final state**, not just
`verify-trace`'s effect-erasing projection. The board digest is recomputed from
the actual resolved board. It then compares fourteen Lean projection lines:
digest, yield end, count, request payload, meters, five chips and four rows.

Validation:

- Lean elaborates with zero errors; nine axiom checks contain no `sorryAx`
  (at most `propext` and `Quot.sound`).
- Fourteen readback lines match, zero deltas; five Lean summary deltas are zero.
- Focused Clojure: **2 tests / 10 assertions**, zero failures/errors. Includes
  empty/nonempty debt, missing-debt typed-none, and registry replacement.
- Lint: zero errors/warnings. Parens and diff checks pass.
- Source/transcript hashes unchanged across the check; retained in validation.log.

## Runtime certificate status is still pending

The unconditional board-only claim is false with the current registry API.
In an isolated atom, `register-verb!` replaces `:smell-backlog` with a function
emitting `:commit`. The five-chip board still has no ZAP; its digest is unchanged;
the resulting certificate still reports `:verified? true`. Lean theorem
`noZapAloneIsInsufficient` exhibits the same missing premise.

The runtime does not enforce the registry docstring's 'load time only' claim,
and the board digest covers no verb implementation or registry revision. A
blanket `:lean/status :validated` would therefore overstate this theorem.
The producer docstring now explains why it remains pending. The separate checker
result is typed `validated` only for the declared-registry model and pinned
readback. A certificate claiming all future executions needs an enforced binding
to the proved registry semantics (or an independently checked returned-trace
claim), not merely absence of ZAP in board data.

Effect kinds are proposals: no theorem here certifies what an arbitrary caller's
I/O handler does, verifies the 324 debt items, or updates their register status.
The basis digest is checked as retained request data, not as a fresh audit of
its referenced file. No live registry, effect handler, queue or historical run
was changed.

## Reproduce from futon3c

```sh
python3 holes/labs/wm-contract/runs/chip-board-cascade-verifier/lean-witness/check.py
clojure -M:test -n futon3c.agents.cascade-verifier-witness-test
```

The Python checker uses canonical mathlib4 and its own existing Lake packages,
then Babashka for the pure runtime readback. No build, fetch or live service call.
