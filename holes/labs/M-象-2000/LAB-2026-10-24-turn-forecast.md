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

## Guards and symbols (added 2026-10-24, second packet)

Sibling script `futon3c/scripts/turn_forecast_symbols.py` (reuses the forecast
loaders read-only by import; runs on plain `python3`, no embeddings needed —
clean labels only). Command that produced everything below:

    python3 -I futon3c/scripts/turn_forecast_symbols.py \
        --store ~/.emacs-graph/session-turn-analysis \
        --split-date 2026-10-17 \
        --out futon3c/data/xiaoxiang-symbols/candidates-2026-10-24.json \
        --results-json /tmp/tfs_results.json

**Population.** 5,608 drafts exist (from 2026-10-03). Of 2,960 edges, **1,278
have a draft on turn n**: train (n+1 ≤ 10-17) **782**, test (10-18…10-24)
**496** (the test week edges now number 497; the store grew ~6 records between
the two packets' runs).

**Guards.** Atomic guards evaluated at send time: `declared:<mark>` (basis
"declared" or a `<mark>:` line in the source — too rare to ever reach 15 train
fires, so none kept), `sure:<intent>` (a fragment guessing that intent top-1
with sure=true), `close:<intent>` (reply n's closing-paragraph mark). Kept
(≥15 train fires): `close:ask-action close:propose close:qualify close:report
close:unresolved sure:approve sure:ask-action sure:continue sure:delegate` —
9 atomics. **No 2-guard conjunction reached 15 train fires**, so the
conjunction row of the design produced nothing.

**Predictions.** uptake:closing, uptake:closing-askmark (clean labels only),
and the 5 most frequent intents of n+1 (propose, ask-action, report-problem,
explain, approve).

**Test baselines** (test week, draft edges): uptake:closing 0.531,
uptake:closing-askmark 0.593; intents propose 0.310, ask-action 0.246,
report-problem 0.171, explain 0.306, approve 0.163.

### All 54 guard–prediction pairs (S = survives: ≥5 test fires/labeled and test hit > baseline; * = train hit rate also beat that intent's train base rate)

| S | guard | prediction | train hit | test (labeled/hit) | test rate | baseline | shuffled | |
|---|---|---|---|---|---|---|---|---|
| **S** | `close:ask-action` | `intent:explain` | 60/20=0.33 | 96/35 | 0.365 | 0.306 | 0.311 * |
| **S** | `close:ask-action` | `intent:propose` | 60/21=0.35 | 96/35 | 0.365 | 0.310 | 0.313 * |
| **S** | `close:propose` | `intent:approve` | 42/10=0.24 | 23/9 | 0.391 | 0.163 | 0.161 * |
| **S** | `close:propose` | `intent:propose` | 42/14=0.33 | 23/9 | 0.391 | 0.310 | 0.304 * |
| **S** | `close:propose` | `intent:report-problem` | 42/11=0.26 | 23/4 | 0.174 | 0.171 | 0.172 * |
| **S** | `close:qualify` | `intent:approve` | 67/5=0.07 | 39/8 | 0.205 | 0.163 | 0.160 |
| **S** | `close:qualify` | `intent:report-problem` | 67/25=0.37 | 39/10 | 0.256 | 0.171 | 0.172 * |
| **S** | `close:report` | `intent:explain` | 55/11=0.20 | 76/25 | 0.329 | 0.306 | 0.314 * |
| **S** | `close:report` | `intent:propose` | 55/23=0.42 | 76/25 | 0.329 | 0.310 | 0.314 * |
| **S** | `close:unresolved` | `intent:explain` | 29/4=0.14 | 25/8 | 0.320 | 0.306 | 0.316 |
| **S** | `sure:approve` | `intent:approve` | 51/10=0.20 | 48/9 | 0.188 | 0.163 | 0.161 * |
| **S** | `sure:approve` | `intent:ask-action` | 51/15=0.29 | 48/12 | 0.250 | 0.246 | 0.251 * |
| **S** | `sure:approve` | `intent:explain` | 51/8=0.16 | 48/16 | 0.333 | 0.306 | 0.317 |
| **S** | `sure:approve` | `intent:propose` | 51/14=0.27 | 48/15 | 0.312 | 0.310 | 0.314 * |
| **S** | `sure:approve` | `intent:report-problem` | 51/15=0.29 | 48/9 | 0.188 | 0.171 | 0.171 * |
| **S** | `sure:ask-action` | `intent:approve` | 143/21=0.15 | 90/16 | 0.178 | 0.163 | 0.163 * |
| **S** | `sure:ask-action` | `intent:ask-action` | 143/41=0.29 | 90/23 | 0.256 | 0.246 | 0.249 * |
| **S** | `sure:continue` | `intent:approve` | 87/3=0.03 | 17/3 | 0.176 | 0.163 | 0.160 |
| **S** | `sure:delegate` | `intent:approve` | 28/5=0.18 | 28/5 | 0.179 | 0.163 | 0.157 * |
| **S** | `sure:delegate` | `intent:ask-action` | 28/10=0.36 | 28/9 | 0.321 | 0.246 | 0.250 * |
| **S** | `sure:delegate` | `intent:propose` | 28/10=0.36 | 28/9 | 0.321 | 0.310 | 0.309 * |
| **S** | `sure:delegate` | `intent:report-problem` | 28/6=0.21 | 28/6 | 0.214 | 0.171 | 0.168 * |
| — | `close:ask-action` | `intent:approve` | 60/6=0.10 | 96/5 | 0.052 | 0.163 |  |
| — | `close:ask-action` | `intent:ask-action` | 60/10=0.17 | 96/21 | 0.219 | 0.246 |  |
| — | `close:ask-action` | `intent:report-problem` | 60/14=0.23 | 96/14 | 0.146 | 0.171 |  |
| — | `close:ask-action` | `uptake:closing` | 5/2=0.40 | 8/4 | 0.500 | 0.531 |  |
| — | `close:ask-action` | `uptake:closing-askmark` | 5/2=0.40 | 8/4 | 0.500 | 0.593 |  |
| — | `close:propose` | `intent:ask-action` | 42/5=0.12 | 23/5 | 0.217 | 0.246 |  |
| — | `close:propose` | `intent:explain` | 42/7=0.17 | 23/7 | 0.304 | 0.306 |  |
| — | `close:propose` | `uptake:closing` | 1/1=1.00 | 1/1 | 1.000 | 0.531 |  |
| — | `close:propose` | `uptake:closing-askmark` | 1/1=1.00 | 1/1 | 1.000 | 0.593 |  |
| — | `close:qualify` | `intent:ask-action` | 67/15=0.22 | 39/9 | 0.231 | 0.246 |  |
| — | `close:qualify` | `intent:explain` | 67/14=0.21 | 39/11 | 0.282 | 0.306 |  |
| — | `close:qualify` | `intent:propose` | 67/19=0.28 | 39/10 | 0.256 | 0.310 |  |
| — | `close:qualify` | `uptake:closing` | 2/2=1.00 | 1/0 | 0.000 | 0.531 |  |
| — | `close:report` | `intent:approve` | 55/8=0.15 | 76/11 | 0.145 | 0.163 |  |
| — | `close:report` | `intent:ask-action` | 55/17=0.31 | 76/18 | 0.237 | 0.246 |  |
| — | `close:report` | `intent:report-problem` | 55/10=0.18 | 76/11 | 0.145 | 0.171 |  |
| — | `close:unresolved` | `intent:approve` | 29/3=0.10 | 25/4 | 0.160 | 0.163 |  |
| — | `close:unresolved` | `intent:ask-action` | 29/6=0.21 | 25/3 | 0.120 | 0.246 |  |
| — | `close:unresolved` | `intent:propose` | 29/6=0.21 | 25/7 | 0.280 | 0.310 |  |
| — | `close:unresolved` | `intent:report-problem` | 29/4=0.14 | 25/4 | 0.160 | 0.171 |  |
| — | `close:unresolved` | `uptake:closing` | 3/2=0.67 | 1/1 | 1.000 | 0.531 |  |
| — | `sure:approve` | `uptake:closing` | 2/1=0.50 | 2/1 | 0.500 | 0.531 |  |
| — | `sure:approve` | `uptake:closing-askmark` | 1/0=0.00 | 2/1 | 0.500 | 0.593 |  |
| — | `sure:ask-action` | `intent:explain` | 143/17=0.12 | 90/18 | 0.200 | 0.306 |  |
| — | `sure:ask-action` | `intent:propose` | 143/30=0.21 | 90/21 | 0.233 | 0.310 |  |
| — | `sure:ask-action` | `intent:report-problem` | 143/31=0.22 | 90/11 | 0.122 | 0.171 |  |
| — | `sure:ask-action` | `uptake:closing` | 2/2=1.00 | 5/2 | 0.400 | 0.531 |  |
| — | `sure:continue` | `intent:ask-action` | 87/5=0.06 | 17/3 | 0.176 | 0.246 |  |
| — | `sure:continue` | `intent:explain` | 87/1=0.01 | 17/4 | 0.235 | 0.306 |  |
| — | `sure:continue` | `intent:propose` | 87/2=0.02 | 17/4 | 0.235 | 0.310 |  |
| — | `sure:continue` | `intent:report-problem` | 87/1=0.01 | 17/1 | 0.059 | 0.171 |  |
| — | `sure:delegate` | `intent:explain` | 28/4=0.14 | 28/6 | 0.214 | 0.306 |  |

**22 pairs survive, 18 of which also beat their train base rate** (marked *).
Honest caveats: (1) the margins are thin (best: 0.391 vs 0.163,
`close:propose → intent:approve`, n=23) and 54 pairs were scored, so several
survivors are multiple-comparison noise — the 4 survivors whose TRAIN hit
rate was at or below their train base rate (no *: sure:approve→explain,
sure:approve→report-problem, sure:continue→approve, close:unresolved→explain)
should be treated as noise
until re-scored on a fresh week; (2) **no uptake prediction survived** — the
clean-label population among draft edges is tiny (1–8 labeled per guard), far
under the 5-labeled bar or below the strong 0.531 last-paragraph baseline.

**Negative control.** For each survivor, the guard's firing was shuffled
across test edges (same firing count, 200 permutations): every shuffled mean
hit rate lands on the baseline (e.g. 0.311 vs baseline 0.306; 0.161 vs 0.163;
0.249 vs 0.246 — full column above). The survivors' advantage comes from
WHICH edges the guards select, and disappears when that selection is
destroyed — the scoring can fail.

**Symbol records.** The 22 survivors are written to
`futon3c/data/xiaoxiang-symbols/candidates-2026-10-24.json` (one record per
pair, `status "candidate"`, ≤20 evidence record ids each). Nothing was
written to Minus40.

### Named candidates (counts only — all data is from 2026-10-24, one day, no held-out split)

Data: 17 live records with `:pattern_verdicts` + the 18-row hand experiment
(`occurrences.json`) = 35 rows.

- **(i) iching/* with 0/4 parts mapped:** 12 iching/* proposals, **9 with
  chain_mapped = 0** (75%): hexagram-09-xiaochu, 01-qian (×2), 56-lv, 18-gu,
  49-ge, 54-guimei, 64-weiji, 63-jiji. The iching family reads as mostly
  non-occurrent in the chain window.
- **(ii) mapped parts point into the reply:** of 67 mapped parts across all
  rows, **65 name `who "reply"` and 2 `who "operator"`** (97% reply). The
  mapped evidence for proposed patterns sits almost entirely in the agent's
  reply, not Joe's turn.
