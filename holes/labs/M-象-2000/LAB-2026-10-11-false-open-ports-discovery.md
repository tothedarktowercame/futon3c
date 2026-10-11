# False-open ports: discovery (claude-16, 2026-10-11)

Companion to `CHECKPOINT-2026-10-11-false-open-ports.md` (claude-15's labelled
session). That note asked whether `link-quoted-brackets` misses the quotes or
the adjacency table does not close the kind pairs. Neither is reached first.

## Finding 1: the replies are not on the records

`reply_text` is empty on 20 of the 24 operator turns in the labelled session
(`ec3a9b31…`) before 03:36Z; only the four turns after the Emacs recorder was
switched back to `jvm` carry one. Without reply text there are no reply acts,
so no operator fragment can be answered, whatever the matcher or the table say.

Census over analysed operator records (files in
`~/.emacs-graph/session-turn-analysis`, read 2026-10-11 ~05:00Z):

| day | records | with reply_text | with ports |
|---|---|---|---|
| 09-28 to 10-03 | 1380 | 0 | 0 |
| 10-04 | 162 | 135 | 133 |
| 10-05 | 73 | 68 | 68 |
| 10-06 | 8 | 8 | 8 |
| 10-09 | 93 | 0 | 88 |
| 10-10 | 117 | 0 | 115 |
| 10-11 | 33 | 7 | 32 |

Reply text on the record began on 10-04 with the map form of
`attach-happened!` (`{:reply :commits}`); the `files` recorder sends a string
summary, so the outage-window records got `happened_summary` ("Reply begins:"
plus a few paragraphs) and no `reply_text`. The codex-70 backfill (5e654230)
replayed the records as they stood, so its ports were computed from operator
fragments alone. This is the mechanism behind the 1,857 open ports counted
on 10-11: records with no reply acts.

## Finding 2: where replies exist, brackets are labels, not quotes

Over the 797 marked reply paragraphs on records with reply text since 10-01:

| bracket shape | paragraphs |
|---|---|
| label only, or quote under 8 characters | 662 |
| quoted span of 8+ characters | 123 |
| of which: matches exactly one kinded fragment | 108 |
| of which: matches no fragment | 14 |
| of which: matches several | 1 |
| marked, no bracket | 12 |

So `quoted-targets` and `link-quoted-brackets` do what they say when given a
quote; the proforma practice since 10-04 mostly gives labels ("where we are",
"plan going forward"). The labelled session's own replies are label-only in
15 of 17 brackets. A target link closes the port regardless of the adjacency
table (`answerso`: "a target or carries-out link whose pair the table does
not list closes the port"), so the table is not what limits linked answers.

Also: `turn-ports` builds the kernel db as `(lx/db acts)` with no inferred
links, so `answers-inferred` is never populated on the live path. Only
explicit links (targets, carries-out, cites) close anything today.

## Finding 3: the replies are recoverable

Agent replies are on futon1b :7073 as agent-chat/turn evidence: role
"assistant", `unified-text`, `segment-final`, `turn-id` such as
`claude-15-turn-21`, chained by `in-reply-to` from the operator turn's
`emacs-…` evidence (checked on `emacs-05223cff…`). Reply text can therefore
be backfilled onto the records and ports recomputed, for the outage window and
for turns before 10-04.

## Method

Python over the record files; the regexes copy `quoted-targets` (leading
bracket, double or curly quotes, squashed, 8+ characters) and the fragment
match in `link-quoted-brackets` (fragment contains quote, or quote contains
fragment; kinded fragments only). A Clojure confirmation through the real
functions is the first check of the implementation packet, not of this note.

## Next

1. Discovery packet: coverage of agent-reply evidence against turn records
   for 10-09 to 10-11 and a sample before 10-04 (join on session and time, or
   through the operator evidence's chain). Read-only; output a table.
2. Implementation packet: backfill `reply_text` from that evidence; rerun
   `compute-ports!`. Acceptance: the labelled session recomputed and the
   24-row table re-labelled against the result.
3. Then, with numbers: how label brackets should link (word-overlap match, or a
   sole-port rule for a report/explain paragraph in a turn with one opening
   fragment), placed in `answers-inferred`; and whether `:constrain` is a
   standing (in-forceo) rather than a port.
