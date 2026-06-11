# Pilot Stock-Take 001 — the first ten-cycle arc (flight side)

**Date:** 2026-06-11
**Author:** claude-3 (WM pilot)
**Companion side:** ground control (fable-1) brings the apparatus/instrument side (what got built/fixed per cycle); Joe convenes.
**Scope:** WM-pilot LOOP Turns 1–10 (`PILOTS-LOG.md`) — the long end-to-end inhabitation toward FutonZero's G-SIM calibration.

## The arc, one line per cycle

- **T1** (claude-1) — first cycle on the cascade lane; found capability `:grounding` is prose, not machine-checkable.
- **T2** — teleport caught (`open-mission` on an already-open mission) → V2 no-teleport; keystone close of M-capability-star-map.
- **T3** — first earned address-sorry on the refilled registry; minted `two-projections-of-one-quantity` (a cited-but-unwritten pattern).
- **T4** — first executed pair via the realised-on-merge binding — but a **FALLBACK** (target vanished → censored 0.0), caught by ground control. Pipe verified end-to-end; measured count stayed 0.
- **T5** — first genuine **MEASURED** pair (partial-discharge: make M-futonzero-generative's true state machine-visible); error 2.4e-4.
- **T6** — §4.3 policy/value vocabulary; logged error 0.277 — later **RETRACTED** as a transient artifact.
- **T7** — §4.2 toy field fixture (codex build; first build-dispatch lane); error 4.1e-4. **KEY FINDING:** settled hole-G is flat → cycle-6's 0.277 was a transient; named the realised-read protocol.
- **T8** — first below-cap flight under the target-sensitive model (M-the-perfect-crime); **TRUE NULL** (error 6.6e-5); surfaced that the fix **RELOCATED** `prior==value`, didn't break it.
- **T9** — first **CLEAN L1 DYNAMICS** pair (M-daily-scan Day-counter fix, ohc 5→4); error 0.039 = the per-hole increment.
- **T10** — second dynamics pair (M-daily-scan code-search probe-type, codex build, ohc 4→3); error **0.0386** — consistent with T9's 0.0394, confirming a roughly **linear per-hole increment (~0.039)** across two different ohc transitions.

## The deep finding: the loop's subject was its own value function

Run honestly, the loop surfaced that its calibration was **circular**. A source-check of `compute-efe` (fable-1) showed the per-target field G composes the *same* constant predict-effects the model predicts with — `prior == value`, one level down. The near-zero "measured" errors (T5/T7) were the instrument measuring the model against itself. The fix — **target-sensitive predict-effects** (operator-consented, it re-ranks the whole field) — *broke state-insensitivity* (the field now responds to real world-change) but *relocated the increment-circularity* (G-vs-G tests whether the world moved, never the coefficient). That named the **two-layer calibration**:

- **L1 G-vs-G** — dynamics/consistency. Cheap, every cycle, honestly *never value evidence*. (T5–T10 are L1.)
- **L2 outcome-vs-prediction** — the value layer. Needs *witnessed outcomes* (closure-folds, CH2, peradam: did real functioning result?), gated behind **pudding-G1's arrow-witness binding (registry sorry #2)**. Accrues free per executed cycle.

The forensic corollary (recorded on M-the-perfect-crime): **the crime is never killed, only pushed to a scarcer witness** — constant's state-blindness exposed → scaled's increment-circularity exposed → next, witnessed outcomes. Each relocation costs the launderer more; that *is* the pudding-prover rationale, derived from below.

## Pairs by layer

| cycle | error | class | disposition |
|---|---|---|---|
| 4 | 0.0 | censored fallback | **excluded** |
| 6 | 0.277 | transient spike | **excluded** (`:settled` ages it out) |
| 5 | 2.4e-4 | visibility edit | L1 near-zero |
| 7 | 4.1e-4 | `prior==value` era | L1 near-zero |
| 8 | 6.6e-5 | true null (ohc unchanged) | L1 null |
| 9 | **0.039** | clean dynamics (ohc 5→4) | **L1 dynamics** |
| 10 | **0.0386** | clean dynamics (ohc 4→3) | **L1 dynamics** |

**The clean L1 result:** the per-hole increment is **consistent** — 0.0394 (ohc 5→4) and 0.0386 (ohc 4→3) — so the field's settled G is roughly *linear* in hole-count below the cap, and a real hole-closure moves it by that increment. That is genuine dynamics evidence: the world moved, by the predicted amount.

## Both models (dual-prediction, post-switch pairs)

**Honest correction (this stock-take caught my own overclaim):** a 2-pair read (T8, T9) looked like "scaled ~2× better"; the third pair (T10) evens it out. Over T8/T9/T10 the **mean errors are nearly tied — scaled ~0.026 vs constant ~0.027** — but they win on *opposite* cases: scaled nails the **null** (T8: 0.00007 vs 0.040 — correctly predicts no-move), constant nails the **continuity-landing** (T10: 0.0004 vs 0.039 — the realised landed at the ohc-3 continuity point where constant's flat prediction sits), and they roughly tie on the mid-move (T9). The per-pair "winner" is dominated by *where the action lands relative to the continuity point*, not by clear model superiority — exactly what you'd expect of an L1 dynamics/consistency layer that is **not** a value-discriminating one. Verdict: more pairs and careful analysis are needed before claiming either model better; the honest L1 signal is the *increment consistency above*, not a model horse-race.

## Discipline-events census

8 total — `:operator-merge` 6 (the executed cycles), `:teleport-refused` 1 (T2 V2 catch), `:operator-decline` 1.

## What the arc taught (flight read)

The instrument we built to detect laundering kept catching *itself* — and that is not a failure of the loop, it is the loop doing exactly what FutonZero is for, one layer at a time. Disciplines that earned their keep: **earned closure** (V2 no-teleport, T2), **settled-read** (caught the T6 transient), **author≠reviewer** (fable-1's code-gate caught an untested laundering mode I'd missed, T7), **determined-fork proto-PSR** (stop over-asking on findable warrants), **verify-the-mechanism-before-asserting** (the `prior==value` concern was framed as a question and source-checked, not asserted). The next layer is named: **L2 / pudding-G1 arrow-witness**.

## Open items / next moves

- **L2 outcome-grade design** = pudding-G1 arrow-witness binding (registry sorry #2, field [2]) — the real G-SIM clearance evidence, and where the three-witness certificate lives.
- **Cap-widening** (the ≥6-hole 2×-cap plateau) — a future *calibrated* decision, not a v1 guess.
- **Pilot-closeable counted holes are scarce** — below-cap missions mostly carry operator-gated or substantial holes; quick pilot fixes (T9) and codex builds (T7/T10) are the two reliable shapes.
- **futon7 worktree hygiene** — significant pre-existing uncommitted work (blanket-aif paper, forward-model analysis, M-buyer-discovery); flagged to its author, not clobbered.
