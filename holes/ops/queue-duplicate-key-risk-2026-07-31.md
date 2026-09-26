# Queue duplicate-key risk — do NOT normalise blindly

**Written 2026-07-31 by claude-9. Read before running the cron or any
load→dumps pass over `data/codex-sorry-queue.edn`.**

## The correction

I twice told Joe the duplicate keys came from "the dispatch bookkeeping path
appending where it should set". **That was wrong.** `codex_sorry_cron.py` writes
rows correctly — `dict(row)` then `.update({...})`, saved via `dumps(queue)`,
which sets and cannot duplicate. The duplicates come from **text-level hand
edits** — the `row[:-1] + ' :key val}'` append idiom — used by me throughout the
overnight session and by earlier sessions. The writer is fine; the practice was
not.

## The landmine

`edn_format` keeps the **last occurrence in text** — verified:
`{:job-id "NEW" :job-id "OLD"}` parses to `OLD`. In these rows the last-in-text
value is the **oldest**, because appends landed after the original field.

A `loads` → `dumps` round trip collapses every duplicate: **58 rows → 0.** So the
next cron run silently normalises the whole queue — and **freezes the stale
value**. This is not a cleanup; it is a data loss.

It has already cost something real: trusting a row's `:job-id`, I collected and
began processing a **stale** a95A02 job (…454, commit `bdaae46`) when the file
was four commits further on at `ed29063`. Caught only because the runner's
reported sorry count disagreed with the file I had just measured.

## Rows that would freeze a STALE `:job-id`

14 of the duplicated rows currently point at the wrong job:

| row | parser keeps | should be | copies |
|---|---|---|---|
| `hard-problems-a00j05-lean-main-lean` | …307-feb5cab2 | …389-d3ee2d20 | 3 |
| `hard-problems-a01a03-lean-main-lean` | …316-9e0412d5 | …317-efe6d620 | 5 |
| `hard-problems-a01a04-lean-main-lean` | …319-e6e43466 | …392-3c78ad0b | 3 |
| `hard-problems-a01a06-lean-main-lean` | …323-bca5d852 | …396-80ac8783 | 3 |
| `hard-problems-a01a07-lean-main-lean` | …328-1da63d82 | …403-177f0408 | 5 |
| `hard-problems-a02j02-lean-main-lean` | …422-8b0bdb11 | …426-75c10560 | 2 |
| `hard-problems-a02j05-lean-main-lean` | …330-c8636d21 | …411-86a472d7 | 3 |
| `hard-problems-a03j03-lean-main-lean` | …429-2af10f6f | …432-37a69345 | 2 |
| `hard-problems-a93a03-lean-main-lean` | …340-979d9f4e | …342-d628b6a7 | 2 |
| `hard-problems-a93a04-lean-main-lean` | …434-9497082a | …440-ea2ea5fc | 2 |
| `hard-problems-a94a03-lean-main-lean` | …441-1c05a75c | …443-547dd835 | 2 |
| `hard-problems-a94j02-lean-main-lean` | …446-4d291d8d | …451-cdc46268 | 2 |
| `hard-problems-a95j08-lean-main-lean` | …445-440fdc02 | …463-2354633d | 3 |
| `one-sorry-a92j05-lean-main-lean` | …378-286a4226 | …381-36c58afb | 2 |

## The correct fix, in order

1. **Do not run a normalisation pass, and do not let the cron save the queue,
   until the values are chosen per row.** The collapse is silent and lossy.
2. For each row above, decide the correct `:job-id` (newest, per the table) and
   set it explicitly, then collapse that row.
3. Only then re-emit the whole queue once, with nothing in flight, verifying row
   count and spot-checking values.
4. Stop hand-appending. Use parse → mutate → re-emit, or set-if-present /
   append-only-if-absent with an assertion that no duplicate was introduced.

`scripts/queue_key_audit.py` reports the current state. `:attempts` is
duplicated the same way and equally stale — a95A02 read `attempts 1` when the
truth was 4.
