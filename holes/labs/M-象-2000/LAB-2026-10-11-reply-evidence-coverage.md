# LAB-2026-10-11 reply-evidence coverage

Read-only census for M-象-2000.  Read at **2026-10-11 04:13:52 UTC** from
futon1b `http://127.0.0.1:7073`; no evidence, record, or source was changed.
The reproducer is
[`scripts/reply_evidence_coverage.py`](scripts/reply_evidence_coverage.py) and
was run with `python3 -I`.

## Result

The recent population maps well enough to make a reply-text backfill useful,
but not universally: **213/247 (86.2%)** of all analyzed operator records from
Oct 9 through Oct 11 have exactly one reply under the two joins.  The older
sample is much weaker: **19/100** map, predominantly because the evidence
chain/window has no assistant entry for the record.

| population | records | record has `evidence_id` | join 1 | join 2 fallback | none |
|---|---:|---:|---:|---:|---:|
| all analyzed operator records, Oct 9--11 | 247 | 168 | 83 | 130 | 34 |
| deterministic sample, Oct 1--3 | 100 | 73 | 6 | 13 | 81 |
| **total** | **347** | **241** | **89** | **143** | **115** |

There were no ambiguous join-1 results and **no case where independently
available joins 1 and 2 selected different reply groups**.

Date detail (the early 100 are SHA-256-ranked by record id, so repeatable):

| date | records | has evidence id | join 1 | join 2 | none |
|---|---:|---:|---:|---:|---:|
| Oct 1 sample | 20 | 19 | 1 | 6 | 13 |
| Oct 2 sample | 35 | 26 | 4 | 5 | 26 |
| Oct 3 sample | 45 | 28 | 1 | 2 | 42 |
| Oct 9 all | 93 | 89 | 45 | 45 | 3 |
| Oct 10 all | 117 | 72 | 34 | 57 | 26 |
| Oct 11 all (at read time) | 37 | 7 | 4 | 28 | 5 |

## Joins implemented

Only files matching `turn-[^.]+.json` were considered.  A record had to be an
operator record with `analysis_status == "analyzed"`.  The early sample takes
the first 100 after sorting eligible Oct 1--3 records by SHA-256 of record id.

Futon1b has bounded list filters for time, session, subject, tags, etc., but
**no `in-reply-to` query filter**.  `/api/alpha/evidence/{id}/chain` walks from
a child backward to its root and therefore cannot discover children from an
operator id.  The script pages
`GET /api/alpha/evidence?tags=chat,turn&since=2026-10-01T00:00:00Z&limit=1000`
using `next-cursor`, then constructs the reverse parent index locally.  It read
3,173 chat-turn entries.

The rules are:

1. **Join 1:** starting at the record's `evidence_id`, accept exactly one
   assistant chat-turn whose `in-reply-to` is that id.  If none is direct,
   inspect one intervening child (the "next entry in the chain") and accept
   exactly one assistant child there.  More than one would be ambiguous; none
   occurred.
2. **Join 2:** independently find assistant chat-turns with the same
   `session_id`, strictly after the record's `created_at` and strictly before
   the next operator record in that session.  Choose the first by
   `(evidence/at, evidence/id)`.  It is used only when join 1 has no unique
   result.
3. Otherwise classify the record as **none**.

For disagreement checking, segmented entries are compared by
`unified-turn-id` (falling back to evidence id), not merely by the segment's
own evidence id.

## Segmented assistant entries

In the paged Oct 1 onward chat-turn evidence window, **720 assistant entries
have `segment-index > 0`**, grouped under 93 `unified-turn-id` values.  Fifteen
of the 232 matched records selected a reply belonging to one of these split
groups.

The important qualification is that the list window contains only **34
complete groups** (indices `0..n`, exactly one final segment); the other 59
groups are incomplete or have multiple final entries in this window.  On all
34 complete groups, the final segment's `unified-text` contains every segment's
`text` in index order: it does carry the whole reply.  It is not byte-equal to
plain concatenation in any group because the unifier adds separators (for
example two newline characters).  No whole-reply claim is made for the 59
incomplete groups.

Thus a backfill should use the final segment's `unified-text` when a selected
entry belongs to a complete split group, rather than storing one segment's
`text`.

## Worked examples

### Join 1: direct evidence parent

Record `turn-PPqEmR` (`claude-17-turn-397`, created
`2026-10-01T16:17:47Z`) names operator evidence
`emacs-92836e3d7da9e090b433975a60ea75e8`.  Assistant evidence
`emacs-5434776942c76dc3a457e01f69c531d9` has exactly that id as
`in-reply-to`, is in the same session
`564c8e50-c240-46fc-81ad-55afafbc63ee`, and arrived at 16:18:41Z.  The time
join independently selects the same entry.

### Join 2: chain id does not reach the reply

Record `turn-9sg8SX` (`claude-17-turn-382`, created
`2026-10-01T00:42:05Z`) names
`emacs-9045a0685a96a9cba03e6fcf35feea5f`, but the reverse chain has no
assistant child at depth one or two.  In the same session, the first assistant
before the next operator record is
`emacs-b57e1af0784021ef80ec734554e2287f` at 00:44:12Z (its stored parent is
`e-ece63c36-207c-47bc-bb0f-8936ab0a0c52`).  It is uniquely selected by join
2 and its body names `claude-17-turn-382`, agreeing with the record's turn id.

### None

Record `turn-lmqjBr` (`codex-10-turn-12`, created
`2026-10-01T00:09:12Z`) names evidence
`emacs-87cbaf810becde07df5f180e78941ded`.  That entry is a **user** chat-turn
for `codex-10-turn-11`, not the assistant reply to turn 12.  It has no
assistant child at depth one or two, and no assistant entry occurs in the
record's same-session interval before the next operator record.  Both joins
therefore correctly return none.

## Implication for the next packet

A conservative reply-text backfill can cover the 232 unique matches and leave
the 115 `none` records untouched.  It should retain both the join used and the
reply evidence id in its dry-run report, normalize complete segmented replies
to final `unified-text`, and refuse any future chain/time disagreement rather
than guessing.
