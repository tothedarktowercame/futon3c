# Kimi 5-hour limit at 15:44 UTC, 2026-09-24: what used it

claude-11, discovery only, for claude-10 / Joe. Read-only apart from this note;
no Kimi dispatches. Raw pulls are in `/tmp/kimi5h/` (evidence JSON per seat,
jobs list) and will not survive a reboot.

## Short answer

Joe's own use didn't cause it. The key was used by claude-10's ten Agency
dispatches between 13:10 and 15:44 UTC: 310 model requests carrying **80.3 M
input tokens**. That is 259k tokens per request on average. The requests were
that large because **every kimi seat keeps one conversation for the whole day
and re-sends all of it on every round**. kimi-4 opened its 03:31 UTC job at 95k
tokens of context and its 15:44 job at 335k. The 15:44 job was refused on its
second round.

This was the **second** time today. The 5-hour limit was also hit at
**05:11 UTC** (00:11 Central) by claude-8's overnight wave of jobs. The Agency
job list shows 6 refused kimi jobs between 05:01 and 05:16 UTC. So "no heavy
jobs since 8 AM" is true of Joe and not of the key.

## Sources

- Evidence store, `GET :7070/api/alpha/evidence?author=kimi-N&tag=turn-round&since=…`.
  The harness (`src/futon3c/agents/zai_api.clj`, `persist-round!` ~l.1136,
  `normalized-usage` ~l.953) writes one record per model round with
  `cost/input-tokens`, `cost/cached-input-tokens`, `cost/output-tokens`.
  kimi-1…7 have 1,102 rounds on 09-24 and 691 on 09-23 (first 09-23 17:15 UTC).
  kimi-8…12 have none.
- Agency jobs, `GET :7070/api/alpha/invoke/jobs?limit=500` (covers back to 09-22 06:52).
- `GET https://api.kimi.com/coding/v1/usages`, sampled 15:49 and 15:53 UTC.
- Kimi docs: https://www.kimi.com/code/docs/en/kimi-code/membership.html ,
  plus third-party summaries https://tracecheck.dev/kimi-code/usage-limits/ and
  https://www.kimi.com/en/help/membership/membership-pricing .

## Q1: what does "100" count, and is the window fixed or rolling?

**The unit is not published.** Kimi's membership page says only that billing
follows "actual usage — the more content read and generated, and the more
complex the task, the higher the cost". It gives no token or request figure.
Third-party pages quote "300–1,200 requests per rolling 5-hour window" and say
the public source does not map this to tiers. `limit "100"` behaves like a
percentage: `used` is exactly "100" at exhaustion.

**It is not a plain request count.** The overnight block reached 614 requests
before its refusal. The morning block was refused at 310.

Per 5-hour block (grid below), from evidence:

| block (UTC)       | requests | input  | cached | uncached | output | refused? |
|-------------------|---------:|-------:|-------:|---------:|-------:|----------|
| 09-23 16:40–21:40 | 418 | 24.8 M | 24.2 M | 0.58 M | 202k | no |
| 09-23 21:40–02:40 | 273 | 26.8 M | 26.2 M | 0.60 M | 153k | no |
| 09-24 02:40–07:40 | 616 | 121.4 M | 119.4 M | 2.00 M | 308k | yes, 05:11 |
| 09-24 12:40–17:40 | 310 | 80.3 M | 77.4 M | 2.90 M | 214k | yes, 15:44 |

Both refused blocks had 3–5× the input tokens of the blocks that were not
refused. Uncached input was also 3–5× higher. Output was not much different.
Two refusals cannot tell whether the quota weighs total input, uncached input,
or a price-weighted mix. What the data does show is that **input volume (context
size × rounds) separates refused from not-refused**, and request count and
output do not.

**Window: this looks like a fixed 5-hour grid, not a rolling window and not
"5 h from first use".** `resetTime` is 17:40:00.988 UTC and did not move between
samples. The first request of this block was at 13:11:17, and nothing in the
evidence used the key near 12:40. A rolling window would report 13:11 + 5 h =
18:11, and "from first use" would also give 18:11. A grid of
…02:40, 07:40, 12:40, 17:40, 22:40 fits both refusals. It also fits the 09-23
boundary: one burst ended at 21:39:48 and the next began at 21:40:24. The
morning block therefore started at 12:40 (07:40 Central) whether or not anyone
used the key. That answers the "window opened at 7:40" puzzle without needing
an unknown user.
*Verify:* sample `/usages` after 17:40. If the next `resetTime` is 22:40
regardless of when the first request lands, the grid is confirmed. The monthly
window resets at 16:47:57 on the 23rd, so the grid is not anchored to the
subscription time either. Its origin is unknown.

## Q2: per-job consumption in the refused block (12:40–17:40)

All jobs ran on model `k3`. Input is the sum over rounds. "1st-round" is the
context the job opened with.

| job start | seat | caller | rounds | input | uncached | output | 1st-round ctx |
|-----------|------|--------|-------:|------:|---------:|-------:|--------------:|
| 13:11 | kimi-4 | claude-10 | 48 | 14.7 M | 0.39 M | 21k | 286k |
| 13:24 | kimi-3 | claude-10 | 52 | 14.9 M | 0.31 M | 21k | 264k |
| 13:24 | kimi-2 | claude-10 | 31 | 7.6 M | 0.26 M | 11k | 227k |
| 13:36 | kimi-5 | claude-10 | 32 | 5.8 M | 0.20 M | 17k | 166k |
| 13:55 | kimi-7 | claude-10 | 38 | 2.2 M | 0.10 M | 42k | 7k (fresh seat) |
| 13:56 | kimi-6 | claude-10 | 35 | 11.5 M | 0.36 M | 28k | 305k |
| 15:00 | kimi-2 | claude-10 | 36 | 11.0 M | 0.34 M | 38k | 265k |
| 15:01 | kimi-3 | claude-10 | 31 | 10.7 M | 0.37 M | 35k | 311k |
| 15:35 | kimi-1 | claude-1  | 5 | 1.2 M | 0.25 M | 1k | 246k |
| 15:44 | kimi-4 | claude-10 | 2 | 0.7 M | 0.33 M | 0.2k | 335k — refused |

claude-10's dispatches account for 98% of the block's input tokens. claude-1's
single per-turn analysis call (kimi-1) is 1.2 M. Compare kimi-7: it did 38
rounds from a fresh session and cost 2.2 M. kimi-6 did 35 rounds on its
day-old session and cost 11.5 M.

## Q3: anything using the key outside the Agency?

**Nothing found.** Absences checked:
- On Zone, only two files read `~/.kimikey` / `api.kimi.com`:
  `futon3c/src/futon3c/agents/kimi_api.clj` (the seats) and `voxterm/server.py`
  `_usage_kimi` (GET `/usages` only, no completions). This comes from a
  recursive grep of `/home/joe/code` over py/el/sh/clj/js/ts.
- No crontab entry mentions kimi. None of the 9 user systemd timers are
  kimi-related.
- claude-1's per-turn analysis (`emacs/session-turn-analysis.el`,
  `session-mode-analysis-agent` = "kimi-1") dispatches through the Agency, so
  it is already counted above: kimi-1 made 5 rounds in the refused block, at
  15:35.
- metameso has no `~/.kimikey`/`~/.kimi-key` and no KIMI env. lucy didn't
  resolve from Zone, so it is **unchecked**. Per memory, lucy's matrix bridge
  invokes Zone's Agency, which would show up as kimi-N evidence anyway.
- Evidence totals are enough to explain the refusal without any outside user,
  and the grid (Q1) explains the 12:40 window start. I can't rule out the key
  being copied to another machine. Nothing needs it.

## Q4: what multiplies usage

1. **Seat sessions are never reset between dispatches (the main factor).**
   `zai_api.clj` holds `!messages` per seat for the JVM's lifetime. It is
   truncated only when the session id rotates (~l.1690), and there is no
   compaction. Every kimi seat ran one session id all day (kimi-4 =
   `…c31f940c` from 03:31 to 15:44). So each new job re-sends every earlier
   job's transcript, and so does every tool round within a job. kimi-4's
   opening context went 95k → 157k → 176k → 202k → 258k → 264k → 286k → 335k
   across its eight jobs. The morning jobs were cheap in work and expensive
   in context.
2. **Cache misses at each job's first round.** After an idle gap of 8 hours
   the provider cache has expired, so the first round is sent uncached: 227k–
   335k uncached tokens per job (for example 15:44 kimi-4: 334,877 uncached of
   335,009). Rounds within a job hit the cache (96–99%). This is why uncached
   input was highest in the morning block (2.90 M) even though it had half the
   requests of the night block.
3. **Retries:** small. There were three re-dispatches after the 05:0x
   refusals (kimi-4 ×2, kimi-1 auto-bellback) and none in the morning block.
   The harness's own identical-retry guards (~l.383, 585–603) did not show up
   as runaway rounds. The largest job was 91 rounds, overnight.
4. **Jobs running after "done": none.** Each morning job's `finished-at`
   matches its last recorded round, to the second.
5. **Subagents:** none. Kimi seats bell other agents through `agency_send.py`
   but spawn no kimi sub-seats. All kimi traffic is on kimi-1…7.

## What would fix it (not done: discovery only)

Rotate or compact a kimi seat's session between unrelated dispatches, or
dispatch to a fresh session by default. A fresh-session job like kimi-7's
costs about a fifth of a day-old one for the same number of rounds.
