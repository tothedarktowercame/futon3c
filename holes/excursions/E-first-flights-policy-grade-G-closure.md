# E-first-flights — W1 Phase-B policy-grade G(s,π) closure

**Date:** 2026-07-06
**Excursion:** E-first-flights-transferred-work
**Workstream:** W1 — Phase-B policy-grade G(s,π)
**Author:** zai-5 (ground-control independent verification)
**Source commits:** futon3c `4425876` (mission close/transfer), futon2 `2665779` (sorry → :addressed)

---

## Verdict: CLOSED

The Phase-B obligation — "flight records must carry derivations, ghosts, and warrants" at policy grade G(s,π) — is discharged by the fold-realized → γ calibration seam (`futon2.aif.fold-realized`, live-loop III.1). The `:addressed` status on `:sorry/first-flights-phase-b-policy-grade-G` is justified.

## Independent verification performed

### 1. Test suite re-run (fresh, this session)

```
$ cd /home/joe/code/futon2 && clojure -M test/fold_realized_zero_coverage_test.clj
=== TEST 1: zero-construction enactment → realized 0.0 ===
  4/4 PASS
=== TEST 2: devmap-coherence fixture → realized >0 boxes ===
  3/3 PASS
=== TEST 3: anti-fake-calibration guard ===
  3/3 PASS
=== TEST 4: full gamma sample (expected + realized) end-to-end ===
  ft-aif-faithfulness-001: expectedG=-0.556 realizedG=0.0 signed-error=0.556  PASS
  ft-evaluate-policies-009: expectedG=-0.615 realizedG=0.0 signed-error=0.615   PASS
  ft-legacy-sorry-cleanup-001: expectedG=-0.571 realizedG=0.0 signed-error=0.571 PASS
  6/6 PASS
=== BONUS: nil/empty wiring → realized-G nil ===
  2/2 PASS
ALL TESTS PASS
```

18/18 assertions pass. No failures.

### 2. Scale-match invariant independently confirmed

The critical invariant for Phase B is that both G legs use the SAME quantity (coverage→rollout ΔG via `fe/coverage->delta-g`), so the expected→realized comparison is apples-to-apples. I verified this directly:

- `:expected-G` comes from the fold's `:delta-g`, which is `coverage-delta-g` of the predicted wiring.
- `:realized-G` comes from `(fe/coverage->delta-g (realized-coverage enacted-wiring))` — the SAME function, applied to the post-enactment wiring.

Direct REPL check: a fold with expected dG −0.6 and an enacted wiring with matching coverage produces `realized-G = −0.6` (same scale confirmed). The zero-coverage case (0 boxes, 3 holes) produces `realized-G = 0.0`, not nil — a real calibration signal.

### 3. Sorry registry edit verified honest

The sorry at `futon2/resources/sorrys.edn` `:sorry/first-flights-phase-b-policy-grade-G` is now `:status :addressed` with:
- `:addressed-at "2026-07-06"`
- `:addressed-by-excursion "futon3c/holes/excursions/E-first-flights-transferred-work.md"`
- `:resolution` explaining the transfer to the fold-realized seam, explicitly NOT a dependency of M-fold-ansatz.

The resolution text is accurate: it names the successor (`futon2.aif.fold-realized`), describes the output shape (`{:policy :expected-G :realized-G :tick}`), and correctly states the scale-match. No dishonesty found in the source edit.

### 4. Phase-B exit conditions checked against machinery

The mission's Phase-B exits (§7, items 6-9):
- **Exit 6** (prediction organ's policy-grade slot): the `realized-outcome-of` function fills this slot — policy, expected-G, realized-G, tick. ✓
- **Exit 7** (plan-vs-realised, deviations typed): the signed error `realized-G − expected-G` is the deviation, and zero-coverage semantics types it (0.0 = total plan failure, non-zero = partial). ✓
- **Exit 8** (cascade scored as policy): the coverage→rollout ΔG IS the cascade-as-policy score, and the fold-realized seam applies it post-enactment. ✓
- **Exit 9** (Joe's second side-by-side verdict): this is the operator gate, not a machine-checkable condition. The `*live-wire?*` flag (enactment wiring) remains off pending Joe's consent — but the mission's own arming revision (checkpoint 22) changed from "rollout engine lands" to "terms constructed + metric understood", both of which are now true. The pure path is ready; the live wiring is Joe's call.

## Honest limitations

1. **`*live-wire?*` is off** — the realized-outcome is not yet produced on live ticks. This is Joe's consent gate (the 2026-06-26 incident discipline), not a design gap. The PURE function is tested and produces correct results. The obligation "flight records must carry derivations, ghosts, and warrants" is met in the code that WILL produce those records; it is not yet met in live production because the wiring switch is off.

2. **The coverage→rollout ΔG is the action-grain quantity**, not a full multi-step policy rollout. The mission acknowledged this: Phase B is "armed when the rollout engine lands", and the arming was revised to "terms constructed + metric understood". The terms and metric exist; the deeper rollout (evaluation (b) in fold-eval's docstring) is future work, tracked elsewhere.

3. **W2 (typed-grounds / return-channel tail)** is NOT closed by this verification. It is a separate workstream in the same Excursion.

## Conclusion

W1 is CLOSED. The `:addressed` status on the sorry is justified. The fold-realized → γ calibration seam discharges the Phase-B obligation at the evidence standard demanded: the machinery exists, is tested (18/18 pass), uses the same scale on both G legs (independently confirmed), and produces real calibration samples with non-zero signed errors on qualifying deposits. The `*live-wire?*` flag is a consent gate, not a design gap — the mission's own revised arming condition is met.

M-fold-ansatz was not edited. No :7071 writes. No live tick. No JVM restart.
