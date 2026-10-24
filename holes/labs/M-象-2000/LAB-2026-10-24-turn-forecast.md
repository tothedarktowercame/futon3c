# LAB 2026-10-24 — turn forecast: which intents does n+1 carry, and which paragraph does it address

Read-only analysis packet (claude-11, approved by Joe 2026-10-24; run by zai-1).
One script: `futon3c/scripts/turn_forecast.py`. No live code touched, no models
called except the existing Minus40 embedder (sentence-transformers
all-MiniLM-L6-v2 in the futon3a venv, the `embed_facts.py` code path).

## Command that produced every table

    /home/joe/code/futon3a/.venv/bin/python -I futon3c/scripts/turn_forecast.py \
        --store ~/.emacs-graph/session-turn-analysis \
        --split-date 2026-10-17 \
        --out /tmp/turn_forecast_results.json

(`python3 -m py_compile scripts/turn_forecast.py` passes; the run itself needs
the futon3a venv for sentence-transformers and numpy.)

## Data

- Operator-origin records from 2026-09-28 onward: **3,036** (claude-11's ~3,430
  tagged turns presumably predates the `since 2026-09-28` cut or counts another
  origin mix).
- **59 sessions** with operator turns since 09-28 (claude-11's 76 sessions is
  likely the whole store, not the since-09-28 subset).
- **2,960 edges** (turn n → n+1 in the same session); **2,901 triples** (edges
  that also have an n-1) — close to claude-11's ~2,700.
- Split by n+1's date: train (≤ 2026-10-17) **2,469** edges; test
  (2026-10-18 … 10-24) **491** edges, of which **365** have a marked reply n.
- Operator side used: the record's `:acts` with `author "operator"` (kinds +
  about). The `.analysis.json` sidecar was NOT used — `:acts` was strictly more
  complete and already normalized to 象's vocabulary.
- Agent side: `reply_text`, paragraphs parsed here (mark + bracket target per
  ~/code/CLAUDE.md's proforma table).

## (A) Intent forecast

Per-intent one-vs-rest logistic (batch GD, 600 iters, L2 1e-3); features as
binary intent/mark vectors. Mean Brier over 22 intents on the 491 test edges:

| predictor | mean Brier | note |
|---|---|---|
| 1 base rate | 0.0940 | |
| 2 intents of n | **0.0910** | best |
| 3 = 2 + intents of n-1 | 0.0911 | no gain over 2 |
| 3 on marked subset (n=365) | 0.0953 | fair-comparison baseline |
| 4 = 3 + reply marks of n (n=365) | 0.0959 | marks add nothing |

Worst Brier per intent (p3): explain 0.217, propose 0.209, ask-action 0.196,
constrain 0.167, qualify 0.165 — the frequent intents stay hardest (they
co-occur in long multi-intent turns).

Recall@0.25 for the correction intents — **0.00 everywhere** (n: disagree 25,
redirect 19, retract 0, withdraw 4 in the test week; p4 subset: 16/16/0/4).
The correction intents are so rare (≤5%) that every predictor keeps them well
under the 0.25 threshold, so nothing fires.

### The 15 largest test-week errors (p3, summed squared error)

All fifteen are long multi-intent operator turns (6–8 actual intents) where the
model predicted 1–3; the model systematically under-spreads. Representative
rows (≤12 words each of n+1's opening):

| err | n → n+1 | actual vs predicted |
|---|---|---|
| 6.03 | turn-XOoYvT → turn-dX02O1 | 7 actual (incl. disagree, redirect, constrain) vs [propose, ask-action, explain] |
| 5.77 | turn-XUIKFp → turn-9JaEnD | 7 actual vs [approve, constrain, ask-action] |
| 5.17 | turn-DjAtQv → turn-KThHzz | 8 actual (incl. disagree, delegate) vs [propose, explain] |
| 5.13 | turn-XI5XUg → turn-SJjoqd | 7 actual vs [propose, ask-action, report-problem] |
| 5.06 | turn-NOOeBZ → turn-p0q9RO | 8 actual vs [explain, ask-action] |
| 5.05 | turn-z5ceZK → turn-wzpC5W | 6 actual (incl. disagree, redirect) vs [explain, propose, qualify] |
| 5.01 | turn-aN2DTY → turn-3ikOEH | 8 actual vs [explain, report, propose] |
| 5.01 | turn-Kkv7r8 → turn-mmkB9b | 6 actual vs [ask-action, propose, delegate] |
| 4.97 | turn-mTbjM6 → turn-ZUdxqe | 6 actual vs [ask-action, report-problem, propose] |
| 4.77 | turn-B65He4 → turn-oDaFoR | 6 actual vs [propose, clarify, constrain] |
| 4.68 | turn-0uDkiL → turn-MYrze8 | 6 actual vs [propose, report-problem, ask-action] |
| 4.59 | turn-gzLIna → turn-JsVWLH | 6 actual vs [explain, approve, ask-action] |
| 4.56 | turn-y9X5hW → turn-SaU0t9 | 2 actual ([clarify, verify]) vs 5 predicted — the reverse error |
| 4.48 | turn-DHzkds → turn-Iv3EyQ | 8 actual vs [ask-action] |
| 4.46 | turn-EsK7lO → turn-QcxipC | 6 actual vs [propose, qualify, explain] |

(The full rows, with per-intent probabilities, are in the results JSON:
`A.largest_errors_p3`.)

## (B) Uptake forecast

Clean labels (test week): **32 edges** (14 by ≥4-word verbatim quote of exactly
one paragraph, 18 by `:reply_to` mark pointer). Noisy labels: **351** edges;
mean best-vs-second embedding margin 0.100; the noisy label agrees with the
clean label on 15/32 edge-matched pairs (47%).

| ranker | clean top-1 (n=32) | clean MRR | noisy top-1 (n=351) | noisy MRR |
|---|---|---|---|---|
| first (Gist) | 0.094 | 0.374 | 0.254 | 0.532 |
| **last** | **0.531** | **0.721** | 0.225 | 0.506 |
| 🈸-else-Gist | 0.250 | 0.481 | 0.217 | 0.503 |
| embedding (clean only) | 0.281 | 0.535 | — | — |

Train week (for reference): clean n=20, last top-1 0.500, first 0.100,
embedding 0.20 — same shape.

## Predictions vs results

- "Predictor 2 beats the base rate on mean Brier" — **held** (0.0910 vs 0.0940;
  thin, and the absolute scale is dominated by rare intents).
- "Predictor 3 adds little over 2" — **held** (0.0911, a hair worse).
- "Recall of the correction intents is low (< 0.3) for every predictor" —
  **held**, at the extreme: 0.00 for all, at threshold 0.25.
- "The 🈸 ranker or the Gist ranker has the best top-1 on clean labels" —
  **failed**: the **last** paragraph wins on clean (0.531 vs 0.250 🈸/Gist
  0.094). Joe's follow-ups most often address the reply's closing (next-step /
  ask) paragraph, not the Gist; note the last paragraph frequently IS the 🈸
  or 🈰/next-step paragraph, so the intent of "what n+1 addresses" sits at the
  end, while the Gist summarises rather than invites uptake.
- "The clean set is small (< 300 edges)" — **held** (32).
- Reply marks add nothing to intent forecast (p4 ≤ p3 on the same subset) —
  an unclaimed null worth recording.
