# Frame: t00A05 (frame-4, round 1 — THE DECISIVE ONE-SHOT)

## Your role card has changed — read this first
You operate under the v2 sustained-attempt card: an attempt is a SIEGE, not
a probe. Exhaust at least two routes before any obstruction report; search
Mathlib yourself; when a route dies, commit its salvage and pivot WITHIN the
attempt; a compiling skeleton with one named hole beats a report. Commit
partials continuously. Consult the store first; per-memory USED/IGNORED
verdicts in your report.

## Target — you start from the best-of-class state
Your checkout is at the CUMULATIVE frontier: frame-2's 15-lemma toolkit AND
frame-3's step-9 lemmas (rot definition, continuity, chart/decoder seam,
seam-cancel) are already proved in `problems/t00A05/lean/Main.lean`. Exactly
one `sorry` remains: the final conjunct of `apm_t00a05`.

The store holds the complete assembly plan and the TRUE gap calibration:
- `e-a36bde67` — the precise remaining work (start here);
- `e-9400816b` — verified lemma texts, API spellings, the scratch-first
  method (iterate each lemma in /tmp importing Mathlib, ~3s compiles);
- `e-209de11b` — the (a)-(c) assembly order;
- a retained scratch at /tmp/t00a05-k.lean (K-continuity, two API sites
  from landing).
Remaining, in order: sphere-side subspace continuity of SP; K-continuity
composite; miss-a-point surjectivity via the no-simple-loop lemma; four
half-axis crossings; per-arc FTC with both angular primitives; telescope
to -2π.

## Contract
Work ONLY in your checkout (ENVIRONMENT block below); commit ONLY to the
frame branch.

## Acceptance
1. `lake env lean problems/t00A05/lean/Main.lean` → exit 0, ZERO
   "declaration uses `sorry`" warnings.
2. `#print axioms apm_t00a05` → at most [propext, Classical.choice, Quot.sound].
3. Commit; report summary, shas, verbatim axiom output, memory verdicts.
