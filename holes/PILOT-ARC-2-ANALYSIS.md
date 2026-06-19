# Arc-2 — constant-vs-scaled analysis (the model-evidence deliverable)

**Date:** 2026-06-11 (WM-pilot arc-2, cycle 14). Feeds STOCK-TAKE-002.
**Question (Joe's arc-2 charter):** is there *clear evidence of improvement in the WM model itself*?
**Method:** every executed cycle since the target-sensitive switch logs BOTH the scaled model's
prediction and the frozen constant-v1 counterfactual (`:predicted-constant`). The realised settled
read scores both. Dual-prediction lets the two models be compared on the *same* pairs.

## Result (4 dual-prediction pairs, T8–T11)

| pair | type | scaled err | constant err |
|---|---|---|---|
| T8 | null (ohc unchanged) | **0.00007** | 0.03973 |
| T11 | capped null (ohc 10→9) | **0.00028** | 0.11872 |
| T10 | dynamics (ohc 4→3) | 0.03859 | **0.00108** |
| T9 | dynamics (ohc 5→4) | 0.03939 | 0.03994 |
| **mean** | | **0.01958** | **0.04987** |

**The scaled (target-sensitive) model is 2.55× more accurate than the constant counterfactual.**

## The mechanism (why it's better — and where)

The scaled model's advantage is **concentrated in no-move prediction.** On the two pairs where the
field did *not* move (T8 ohc-unchanged, T11 capped — closing a hole at the cap leaves the capped
discharge unchanged), the state-sensitive model correctly predicts ~0 movement (errors 6.6e-5,
2.8e-4) while the **state-blind** constant model predicts its single flat value and misses by 0.040
and 0.119. On the two pairs where the field *did* move (T9, T10 — below-cap hole closures, the
per-hole increment ~0.039), the two models are comparable (one pair each way). So the improvement
the operator-consented switch bought is specifically **the ability to predict when the world will
NOT change** — which the constant model, blind to state, cannot do.

This is consistent with arc-1's two-layer reframe: L1 G-vs-G is a *dynamics/consistency* layer.
The scaled model wins on dynamics-tracking (does the field respond to measured state?), not yet on
value calibration (L2, gated behind pudding-G1).

## Honest caveats

- **n = 4.** The charter's "first 10-pair analysis" was not reached: clean dynamics targets are
  *exhausted* (no below-cap ranked mission has a pilot-closeable counted hole left), so arc-2 could
  not mint 6 more clean pairs. The 2.55× is **directional, not robust** — it rests on 4 points, two
  of them no-move cases.
- **T11's large constant error (0.119) is cap-amplified** — the capped discharge sits far from the
  flat constant value, so that pair flatters the scaled model more than a below-cap pair would.
- **Selection is pre-hoc, not post-hoc** — pairs were chosen by target before the error was seen
  (legitimate experimental design), so the 2.55× is not cherry-picked; but the small n limits it.

## Verdict

**Directional evidence: yes, the operator-consented target-sensitive switch improved the WM model**
— 2.55× lower mean error than the constant counterfactual, with the gain mechanistically located in
state-sensitive no-move prediction. **Robust (10-pair) confirmation is blocked by the clean-target
wall**; unblocking it is the hole-granularity frontier (counted sub-holes on big missions, ground
control's lane). Until then this is the honest ceiling of the model-evidence.
