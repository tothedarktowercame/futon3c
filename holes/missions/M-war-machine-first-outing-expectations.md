# M-war-machine-first-outing — Expectations vs Actual (R-L)

**Date:** 2026-05-30. Authored *after* the run (the first outing happened
in-session), so this is the §5 prediction scored against what occurred.

## Predicted (§5 of the mission)

- ~60–75% of cycles spent on **self-bootstrapping** (the WM improving its own
  apparatus); ~25–40% on the outward `:open-mission` tail.
- First cycles on the apparatus sorries; outward work only as the apparatus thins.
- Scored quantities to watch: Σ prediction-error trends down; earned top-shift
  rate high; **regression count = 0**; discharges-vs-halts.

## Actual (M2 run, codex-2 piloting, claude-5 coaching by whistle)

- **4 discharges, all apparatus/self-improvement** (hud-mode rationale,
  coupling-density model, ticks-firing-ratio model, stub-lifts ×31) — i.e.
  effectively **100% bootstrapping**, *over-shooting* the 60–75% prediction. The
  prediction's direction was right (bootstrapping dominates) but the magnitude was
  understated: the outward tail was reached **0%**.
- **Why 0% outward:** the queue exhausted into `:learn-action-class` *before* any
  `:open-mission` surfaced — because the WM's only substrate was the hand-curated
  sorry registry. The "~25–40% outward" never happened not because it wasn't
  valuable but because **those sources aren't wired into the WM** (the
  source-starvation finding; the geometry→AIF pipeline bug).
- **Scored quantities:** per-cycle prediction-error 0.0 on the clean discharges
  (clean-discharge signature); **earned top-shift TRUE on every discharge**;
  **regression count = 0** (the `:sorry-closures-stick` invariant never fired in
  anger); discharges 4 / honest-decompose 1 (ticks, then executed) / honest-partial
  1 (stub-lifts, then completed) / **zero fake-finishes, zero hard-halts**.

## What the miss taught us (the real value of scoring)

The single biggest prediction error was **not** per-cycle — it was at the session
scale: we predicted a mixed bootstrapping/outward run; we got a pure-bootstrapping
run that *ran out of substrate*. That gap **is** the finding: the WM is starved of
input sources. The §5 prediction implicitly assumed the outward sources were
feedable; they aren't yet. The post-outing frontier (Invariant Queue, unfinished
missions, M-the-perfect-crime, geometry→AIF, futon7 daily scans) is precisely the
work of closing that prediction gap.
