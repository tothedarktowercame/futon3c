# Claude Platform replay pilot

**Date:** 2026-10-30  
**Status:** initial 40-request pilot complete  
**Question:** Can the bounded 馬 and 驴 readings move from accumulating Claude
Code sessions to stateless Claude Platform requests without losing their
checked outputs, and can Haiku replace Sonnet on either task?

## Method

`scripts/xiang_platform_replay.py` reads frozen user prompts and their directly
linked assistant replies from the existing seat JSONL.  It does not reconstruct
or update a turn record.  Each frozen prompt is sent independently to Haiku
5.5 and Sonnet 5.5.  The result ledger records the provider request id, usage,
latency, price under the declared model price table, baseline reply, replay
reply, and a task-specific comparison.

- 馬: verdict and exact tuple agreement per common act.
- 驴: top-three set overlap and top-one equality.
- Both: strict outer JSON-array parse success.

These are transport/model comparison measurements, not truth labels.  The 驴
outcome experiment must subsequently score the replay picks against the later
pattern verdicts.  馬 disagreements require a blind adjudication sample rather
than treating the historical Sonnet answer as authority.

## Safety and budget

The API key is read from a mode-0600 file and is never placed in output.  The
harness computes a conservative per-request bound using UTF-8 bytes as an
upper bound for input tokens plus the full configured output allowance.  It
refuses the next request when that bound would exceed `--max-usd`.  Output is
created exclusively, so a prior ledger cannot be overwritten.

## Pilot commands

The frozen seat sessions are named by the corresponding Agency session files.
The initial pilot is ten prompts per task, two models, 2048 output tokens, and
a two-dollar hard ceiling per task.  Ledgers belong under this lab, not in the
live turn store.

```sh
python3 scripts/xiang_platform_replay.py \
  --task lu \
  --source ~/.claude/projects/-home-joe-code-storage-operator-turns/3a532be5-9982-4aa3-9df5-1f95a1afdca5.jsonl \
  --limit 10 --max-usd 2 \
  --output holes/labs/M-象-2000/platform-replay/lu-pilot.jsonl

python3 scripts/xiang_platform_replay.py \
  --task ma \
  --source ~/.claude/projects/-home-joe-code-storage-operator-turns/53f867ad-c2d8-4e11-9f2a-7c1155be07d2.jsonl \
  --limit 10 --max-usd 2 \
  --output holes/labs/M-象-2000/platform-replay/ma-pilot.jsonl
```

## Results

The pilot ran ten frozen prompts for each task through both models (40 Platform
requests total).  Actual spend was **$0.412341**: $0.367614 for 驴 and
$0.044727 for 馬.

| task | model | valid JSON | fully quote-grounded | comparison with historical reply | cost |
|---|---:|---:|---:|---|---:|
| 驴 | Haiku 5.5 | 8/10 | 8/10 | mean top-3 overlap 1.4/3; top-1 equal 3/10 | $0.021848 |
| 驴 | Sonnet 5.5 | 10/10 | 10/10 | mean top-3 overlap 1.6/3; top-1 equal 4/10 | $0.345766 |
| 馬 | Haiku 5.5 | 10/10 | 6/10 | mean verdict agreement 90%; exact tuple 36.7% | $0.003533 |
| 馬 | Sonnet 5.5 | 10/10 | 10/10 | verdict agreement 100%; exact tuple 56.7% | $0.041194 |

The four ungrounded 馬/Haiku cases used a quote from a different paragraph
than the declared `by` paragraph.  The live checker would turn those acts into
`unread`; syntactically valid JSON therefore overstates Haiku's suitability
for 馬.  The two 驴/Haiku failures were truncated JSON responses; every parsed
object in the other eight responses passed the existing five-word verbatim
quote check.

This pilot supports direct stateless Platform Sonnet for 馬 as a faithful
transport candidate.  It does not yet support replacing 馬 with Haiku.  驴
still needs outcome scoring against later pattern verdicts: disagreement with
the historical rerank is not itself an error, while the two truncations may be
addressed by a bounded retry or a larger output allowance only if that policy
is tested explicitly.
