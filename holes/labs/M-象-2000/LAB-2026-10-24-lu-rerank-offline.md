# LAB 2026-10-24 — 驴 rerank offline: does a Haiku reranker pick patterns that occur more?

Read-only offline experiment (Joe approved 2026-10-24: "sure let's try it").
Script: `futon3c/scripts/lu_rerank_offline.py`. No seat registered, no live
code changed, nothing written to any turn record or to Minus40.

## Command

    python3 -I futon3c/scripts/lu_rerank_offline.py --out /tmp/lu_rerank_results.json

(`python3 -m py_compile scripts/lu_rerank_offline.py` passes. Results JSON
kept at /tmp/lu_rerank_results.json.)

## Population

24 turns (claude-11 counted 23 at ~23:15Z; one more landed before the run —
turn-ZeHxUu, 23:03Z): operator records with both `:proposed_patterns[*].ranked`
and `:pattern_verdicts`. Ids, oldest first: turn-eHlLvW nOQoPa jm9stI q0yjr0
LGwXRY KluJjV fDecr1 wbl5rY hxgXfK EAgovO P85Vbx LkBkeo 35avLU yYeWx5 81Kf2K
ApGsTp C0Wln7 R3atHx reiaQX 3pRTA8 Fgcw9U GtAnBj SH3bzy ZeHxUu — all
2026-10-24, one session.

## Arms

- **E (embedding):** the recorded `:ranked` top 3. 72 slots; 4 patterns have
  no file in `futon3/library` (or/fit-for-use-stars, pacspine/frictionless-next,
  p4ng/pattern-activation, fulab/tradeoff-record) and score as absent → **68
  scored picks**.
- **L (驴):** Haiku (`claude -p --model claude-haiku-5-5`, the judge.py
  `ask_claude_cli` helper, one call per turn) reranks a pool and keeps 3, each
  pick carrying a verbatim ≥5-word quote from the operator text naming its IF
  (string-checked; failures dropped, next taken). **65 scored picks, 3 dropped
  by the quote check** (2 on turn-KluJjV, 1 on turn-C0Wln7); turn-ZeHxUu got 0
  usable picks (Haiku's reply had no passing quote).
  - pool (a): the same search the pattern-card provider feeds from
    (`futon3a/scripts/notions_search.py` over
    `resources/notions/minilm_pattern_embeddings.json` — the retrieval whose
    cached `:results` the provider ranks), re-run one-shot with k=20,
    read-only. **Deviation:** the production query is a proto-text (first 200
    chars of the user message + first 200 of the response) that is not
    recorded on the turn record, so the turn's `source_text` was used instead.
  - pool (b): giant-graph neighbours (session-cascade `graph.json`) of
    patterns with status occurrence/partial in earlier records'
    `:pattern_verdicts` of the same session, ≤15, ordered by edge kind.
- **Scoring:** one 馬-sonnet whistle per turn maps the union of E∪L (blind —
  the prompt names no arm for any pattern), window = the record plus up to 7
  earlier operator turns. The 馬 prompt and the map checks are **ported from
  `futon3c.xiang.ma-occurrence` into the script** (prompt-for / check-map /
  chain-window, verbatim in structure), so the namespace never runs and
  nothing is written to a record.

## Per-arm counts

| | picks | void | partial | occurrence | mean chain_mapped | chain_mapped ≥ 2 | operator-side share of maps | dropped by quote |
|---|---|---|---|---|---|---|---|---|
| E | 68 | 55 | 13 | **0** | 0.324 | 6 | 4.5% (1/22) | 0 |
| L | 65 | 44 | 21 | **0** | **0.508** | **12** | 9.1% (3/33) | 3 |

Mapped parts: E 22 (operator-side 1), L 33 (operator-side 3). (Neither arm
produced a single 4/4 occurrence — see prediction 3.)

## Per turn (E mapped counts | L mapped counts)

| turn | E | L | L dropped |
|---|---|---|---|
| eHlLvW | 3,0,0 | 1,0,2 | 0 |
| nOQoPa | 0,0,0 | 0,0,0 | 0 |
| jm9stI | 3,0,0 | 0,0,0 | 0 |
| q0yjr0 | –,2,0 | 0,0 | 0 |
| LGwXRY | 3,0,0 | 0,2,2 | 0 |
| KluJjV | 0,0,0 | 1,1,0 | 2 |
| fDecr1 | 0,0,0 | 0,0,1 | 0 |
| wbl5rY | 0,0,0 | 2,1,0 | 0 |
| hxgXfK | 2,0,– | 2,1,0 | 0 |
| EAgovO | 0,–,0 | 2,1,0 | 0 |
| P85Vbx | 0,1,0 | 1,0 | 0 |
| LkBkeo | 0,0,0 | 0,0,0 | 0 |
| 35avLU | 0,0,0 | 0,2,0 | 0 |
| yYeWx5 | 2,0,0 | 0,0,0 | 0 |
| 81Kf2K | 0,0,0 | 0,0,2 | 0 |
| ApGsTp | 0,0,0 | 0,0,0 | 0 |
| C0Wln7 | 0,0,0 | 0 | 1 |
| R3atHx | 0,0,0 | 0,0,0 | 0 |
| reiaQX | 1,1,0 | 0,0,0 | 0 |
| 3pRTA8 | 1,0,0 | 2,0,2 | 0 |
| Fgcw9U | 0,0,0 | 2,2,1 | 0 |
| GtAnBj | 1,–,0 | 0,0,0 | 0 |
| SH3bzy | 0,0,1 | 0,0,0 | 0 |
| ZeHxUu | 0,1,0 | (no picks) | 0 |

(– = pattern file absent from futon3/library; full pattern ids and 驴's quotes
are in /tmp/lu_rerank_results.json.)

## Predictions (claude-11, written before the run)

1. **L has more patterns with chain_mapped ≥ 2 than E** — **held**: L 12 vs E 6
   (and mean chain_mapped 0.51 vs 0.32; partial 21 vs 13).
2. **L's operator-side maps > 10%, E's ≈ 3%** — **failed on the L side**:
   L 9.1% (6/66), just under the bar; E 4.5% (2/44), close to the prediction.
   Direction right, threshold missed.
3. **Both arms produce 4/4 occurrences on fewer than 1 in 5 picks** — **held
   at the extreme: zero 4/4 occurrences in either arm** (0/68 and 0/65).

## Cost

- Haiku: 24 calls (one per turn), mean **11.9 s** per call (range ~5–16 s).
- 馬-sonnet: 24 whistles, ~15–30 s each.
- One Haiku rerank is comfortably off the critical path (the pattern card's
  budget is 100 ms, but 驴 would replace the top-3 *display*, which is already
  a post-turn async path).

## Reading

The reranker's edge is real but modest, and it comes mostly from pool (b):
驴 repeatedly re-picked the session's already-occurrent patterns
(operator-turns-become-inference-observations, operator-turns-enter-…,
recorded-handoff/delivery-receipt, wr-4/wr-27), which the embedding retrieval
rarely surfaces. That is a cascade signal, not a retrieval signal: the graph
neighbours of earlier verdicts are doing much of the work. A clean follow-up
would score pool (a) alone vs pool (b) alone.
