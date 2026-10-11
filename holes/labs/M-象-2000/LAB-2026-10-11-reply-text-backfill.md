# LAB-2026-10-11 reply-text backfill

Applied 2026-10-11 through the live Agency turn service, following the frozen
before-state ledger committed as `2e5b8fde`.  No record file or futon1b entry
was written directly; no JVM was restarted or namespace reloaded.

## Result

All **232** uniquely joined records were posted to
`POST /api/alpha/xiang/turns/<record-id>/happened` in per-session
`created_at` order, at no more than one POST per second:

| measure | result |
|---|---:|
| join 1 records | 89 |
| join 2 records | 143 |
| replies from complete split groups (`unified-text`) | 6 |
| total reply characters stored | 497,564 |
| records carrying parsed commits | 60 |
| parsed commits sent | 126 |
| POST responses HTTP 200 | 232 |
| `reply_text` exact read-back | 232 |
| records with ports after recomputation | 225 |
| portless zero-act records | 7 |
| records whose serialized ports changed | 212 |

The runner used a final segment's `unified-text` only when the segment group
had contiguous indices starting at zero and exactly one final entry.  Other
entries used their own `text`.  Existing happened-note commits were parsed
with the turn kernel's `- <repo> <sha> <subject>` grammar; trailing
`(+N -N over N files)` text was removed before the route regenerated the
summary.

The ordinary scheduled computation has a 50-turn session window, which cannot
recompute old records in long sessions.  After every successful route call the
runner therefore invoked the same live `compute-ports!` function through
Drawbridge with `ports-session-cap` scoped to 10,000 by `with-redefs-fn`.  The
cap was 50 again on return.  The eight sessions' last settled records were
then recomputed the same way before measuring session totals.

## Open-port counts

Across the eight touched sessions, the sum of `still_open` at each session's
last settled turn changed from **383 to 661**.  The increase is expected and
important: adding replies closes a few operator acts, but also exposes the
reply's own offers, questions, reports and proposals, which had previously
been absent from the kernel input.

| session | last settled record | before | after |
|---|---|---:|---:|
| `00e653e3-28f7-4add-8304-47ca0475b5c2` | `turn-RKTxLv` | 5 | 5 |
| `564c8e50-c240-46fc-81ad-55afafbc63ee` | `turn-j711nF` | 140 | 50 |
| `5a109aed-1c56-4c99-9876-b5c2d6aac003` | `turn-6hFDIW` | 46 | 57 |
| `75b00893-43cc-4172-917a-06a591193b20` | `turn-OD2RDe` | 9 | 25 |
| `9ef8e479-e1b7-4755-894d-1264cacb6c3b` | `turn-1UTHJA` | 87 | 277 |
| `a89c3e64-2a65-4a00-ac82-3ec1ddfd5ba3` | `turn-9PITYv` | blank (no ports) | 120 |
| `ae883f3a-6164-4404-834e-e89a0381ce6a` | `turn-wNt6UQ` | 56 | 65 |
| `ec3a9b31-6656-4530-90db-53a22762c17f` | `turn-poV4qM` | 40 | 62 |

The ledger contains per-record before and after values.  Of 232 rows, 206 had
pre-existing ports and 225 have ports now; summing those per-record snapshots
changes 4,413 to 14,517.  These sums are not an inbox size because every row is
a historical snapshot; the session-tail table above is the non-duplicating
session total.

## Read-back failures

Seven records got the exact reply and regenerated happened summary but remain
without ports:

- `turn-JTuY1v` (`claude-12-turn-6`)
- `turn-WtIZCU` (`claude-11-turn-33`)
- `turn-mVytXT` (`claude-11-turn-49`)
- `turn-VgUJ8K` (`claude-11-turn-53`)
- `turn-ABYrM4` (`claude-11-turn-54`)
- `turn-968NVf` (`claude-12-turn-131`)
- `turn-qX81HV` (`claude-12-turn-145`)

These are the same seven zero-act records identified during the settlement
backfill.  Even with reply text and the full session window,
`session-acts` produces no act for the target turn; `turn-ports` therefore has
no timestamp anchor and reports `No acts for turn`.  The runner retained the
failure and continued because every route response was 200; it would have
stopped immediately on the first non-200.

## Labelled claude-15 session

This reproduces the 24 labels in
`CHECKPOINT-2026-10-11-false-open-ports.md` unchanged and adds the requested
computed result from the recomputed live session.  Only one of the original
24 acts closed.  The reply brackets were mostly labels rather than quotations,
so restoring reply text does not by itself give the linker evidence that the
human labels possess.

| since (UTC) | kind | operator text (start) | what happened | label | computed after backfill |
|---|---|---|---|---|---|
| 10-10 19:36 | report-problem | i tried to open a tunnel with sh t.sh on phone1 no success | cause found (stale port 2222), killed, reported | answered | still open |
| 10-10 19:39 | ask-action | can you grab a 40°belowdb spec from downloads | fetched, summarised | done | still open |
| 10-10 19:45 | propose | We should explore the other options. | two surveys run, comparison and recommendation delivered | answered | still open |
| 10-10 22:13 | ask-action | did you get any results back? | yes, restated | answered | still open |
| 10-10 22:22 | propose | We could reade claude code memories rather than claude.ai memories | profiled them, proposed the mapping | answered | still open |
| 10-10 23:24 | propose | insertion time vs record time can differ without any issue | agreed, probed, plan changed | answered | still open |
| 10-11 01:15 | ask-action | are they still going? | answered (finished) | answered | still open |
| 10-11 01:16 | clarify | what is milestone 4? | answered | answered | still open |
| 10-11 01:45 | clarify | I hope this makes sense. | the debugger idea was taken up in the reply | answered | still open |
| 10-11 01:45 | propose | run all of FUTON in a kind of debugger | discussed; nothing built | **open** | closed in `claude-15-turn-16` |
| 10-11 02:10 | propose | turning off the standard memory system and instructing Claude via CLAUDE.md | discussed (autoMemoryEnabled / autoMemoryDirectory); decision deferred | answered | still open |
| 10-11 02:22 | clarify | That's NOT a directory based search but much more like the 3/2 category elaboration | answered (cascade reading) | answered | still open |
| 10-11 02:22 | propose | merge this workstream with M-象-2000 | discussed; not done | **open** | still open |
| 10-11 02:25 | ask-action | (codex-68's bell) the 3/2-category orientation document | review delivered | answered | still open |
| 10-11 02:27 | clarify | Is that clarifying? | answered | answered | still open |
| 10-11 02:33 | propose | That could be a useful guiding principle for our DB. | taken up ("crystallise at write time") | answered | still open |
| 10-11 02:36 | constrain | Let's not reinvent the wheel. | honoured; a standing rule | constraint | still open |
| 10-11 02:42 | propose | we could work with that one [extra futon1b] and then switch it over | adopted in the plan | answered | still open |
| 10-11 02:42 | propose | we're pretty much in a position to "hook it all up" | plan steps 1, 2, 4 built; 3, 5 not yet | **partly open** | still open |
| 10-11 02:42 | constrain | I don't really want all of the agents' turns to be read by a second agent | honoured; a standing rule | constraint | still open |
| 10-11 02:42 | propose | the claude-14 transcript ... "your agent chat is a crime scene" | planned (step 5); not run | **open** | still open |
| 10-11 02:44 | delegate | bell a description of the work so far and the plan to claude-16 | sent; reply received | done | still open |
| 10-11 03:36 | delegate | (claude-15's reply) backfill → codex-70 | open when computed; codex-70 has since completed it | done (later) | still open |
| 10-11 03:36 | ask-action | please restore the setting & backfill | done | done | still open |

The 19:36 first turn remains under its original placeholder session
`claude-15 (awaiting session)`; the other rows are in the labelled UUID
session.  This is why the table intentionally evaluates that row from its own
record rather than silently dropping it from the UUID session result.

## Evidence limitation

`attach-happened!` attempted the deterministic
`e-xiang-turn-<record-id>` append again, and futon1b accepted it as a quiet
duplicate.  Therefore the existing evidence entries on `:7073` retain their
**pre-backfill bodies**.  The corrected `reply_text`, regenerated
`happened_summary`, and ports live on the turn records; this operation did not
and was not intended to replace immutable evidence.
