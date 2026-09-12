# Chip-board Lean witness — 2026-09-12

Historical first-witness report below. The three runtime fixes are reviewed in
[REPAIR-REVIEW-80d874b8.md](REPAIR-REVIEW-80d874b8.md); the current checker uses
the repaired model. The original validation.log remains unchanged.

Mathlib commit `9fe1552eaf` adds `DarkTower/WarMachine/ChipBoardWitness.lean`.
This is a manually related model and executable readback comparison, not a
verified compiler or proof of the entire Clojure interpreter.

## What passed

- `lake env lean DarkTower/WarMachine/ChipBoardWitness.lean` elaborated with
  zero errors. Nine `#print axioms` checks contain no `sorryAx`; dependencies
  are at most `propext` and `Quot.sound`.
- The successful-call step relation preserves the named-repository hazard:
  every emitted commit for `some repo` has a strictly earlier false-branch
  FEEL for that same repo. The initial state has no certificate. FEEL-live
  does not manufacture one. ZAP checks equality with the stored certificate.
- Finite board/trace types, Boolean branches and opaque digest strings are
  explicit. The checked two-wire predicate supplies either arm for an
  observation. This models the well-formed string-ID domain, not every
  arbitrary EDN value the current validator might accept.
- Typed returned ends and the runtime's terminal / step-cap / fuel priority
  are declared and checked. This does not claim all schema-valid boards
  return rather than throw.
- The six-row production projection follows the eight-chip board's wires,
  starts at its entry, and ends with YIELD. Its named commit has prior FEEL.
- All 17 readback lines agree: board digest, retained input digest, end,
  eight board chips/arms/arguments, six trace rows/branches/digests/abstract
  effects. Five Lean summary deltas also equal zero.
- Three Clojure counterexample tests: **3 tests / 9 assertions**, zero
  failures/errors. Only this namespace was run; no full suite or live call.
- Clojure lint: zero errors/warnings; check-parens: OK; diff check: clean.

## Why runtime `:lean/status` remains `:pending`

A blanket `:validated` would be false even though this witness elaborates:

1. A valid board starting with ZAP and missing `:repo` emits `[:commit
   {:repo nil ...}]` with no preceding FEEL: the missing state certificate
   and missing target compare equal. `nilZapExecutesWithoutFeel` proves the
   model counterexample; the real executor test reproduces it.
2. `verify-trace` removes `:effects` before comparing. Replacing the recorded
   commit target with `foreign` still verifies. Lean's
   `replayDoesNotCertifyEffects` and the Clojure test exhibit the same limit.
3. `:compare-move` passes board/wiring validation with both arms but has no
   executor implementation, so execution throws instead of returning a
   typed end. The third Clojure test reproduces it.

No invariants were weakened to turn these into successes. The certificate
producer's docstring now states this boundary; certificate values and the
retained production transcript are unchanged. The checker emits a separate
`validated` result explicitly scoped to the pinned projection and named-repo
model, with runtime-certificate status `pending` and the three blockers.

## Reproduce

From futon3c:

```sh
python3 holes/labs/wm-contract/runs/chip-board-inbox-zero/lean-witness/check.py
clojure -M:test -n futon3c.agents.chip-board-witness-test
```

`check.py` runs Lean in canonical mathlib4 (its own packages, no APM package
changes) and the pure Clojure readback with Babashka. Source and transcript
hashes are checked before and after. It invokes no effect handler or service.
The successful output is retained in `validation.log`.

The original observation packet is absent from the transcript. The input
hash is therefore checked as a retained string, not recomputed. The resolved
board is reconstructed for target `futon5a` and its actual digest recomputed.
FEEL's target comes from that board because v0 trace rows omit its args.
Observation payloads are abstracted to effect tags; commit repo/mode is
checked, but no filesystem commit execution or live-agent safety is proved.
A commit proposal is not proof that an effect handler committed anything.
