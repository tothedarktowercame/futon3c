# False-open ports: a labelled test case (claude-15, 2026-10-11)

**What this is.** One session's still-open ports, computed by
`futon3c.xiang.turn-acts/session-acts` and `turn-ports` (the same functions
`turn-service/compute-ports!` uses), each labelled by the agent who held the
session with what actually happened to it. It is for whoever next works on how
the kernel links answers to the acts they answer.

**Source.** claude-15's REPL session `ec3a9b31-6656-4530-90db-53a22762c17f`,
10-10 19:36Z to 10-11 03:36Z. Ports computed by minus40's follower
(`~/code/minus40/src/minus40/follower.clj`), which reproduced the live
service's stored ports exactly for 12/12 pre-outage turns checked. The labels
are claude-15's own account of its session, not a second rater's.

**Result.** 24 acts open at the session's last settled turn:
- 18 were answered or done;
- 2 are standing constraints, not obligations;
- 4 are genuinely open or partly open.

So in this session, "open" mostly meant "no answer was linked", not "still owed".

| since (UTC) | kind | operator text (start) | what happened | label |
|---|---|---|---|---|
| 10-10 19:36 | report-problem | i tried to open a tunnel with sh t.sh on phone1 no success | cause found (stale port 2222), killed, reported | answered |
| 10-10 19:39 | ask-action | can you grab a 40ºbelowdb spec from downloads | fetched, summarised | done |
| 10-10 19:45 | propose | We should explore the other options. | two surveys run, comparison and recommendation delivered | answered |
| 10-10 22:13 | ask-action | did you get any results back? | yes, restated | answered |
| 10-10 22:22 | propose | We could reade claude code memories rather than claude.ai memories | profiled them, proposed the mapping | answered |
| 10-10 23:24 | propose | insertion time vs record time can differ without any issue | agreed, probed, plan changed | answered |
| 10-11 01:15 | ask-action | are they still going? | answered (finished) | answered |
| 10-11 01:16 | clarify | what is milestone 4? | answered | answered |
| 10-11 01:45 | clarify | I hope this makes sense. | the debugger idea was taken up in the reply | answered |
| 10-11 01:45 | propose | run all of FUTON in a kind of debugger | discussed; nothing built | **open** |
| 10-11 02:10 | propose | turning off the standard memory system and instructing Claude via CLAUDE.md | discussed (autoMemoryEnabled / autoMemoryDirectory); decision deferred | answered |
| 10-11 02:22 | clarify | That's NOT a directory based search but much more like the 3/2 category elaboration | answered (cascade reading) | answered |
| 10-11 02:22 | propose | merge this workstream with M-象-2000 | discussed; not done | **open** |
| 10-11 02:25 | ask-action | (codex-68's bell) the 3/2-category orientation document | review delivered | answered |
| 10-11 02:27 | clarify | Is that clarifying? | answered | answered |
| 10-11 02:33 | propose | That could be a useful guiding principle for our DB. | taken up ("crystallise at write time") | answered |
| 10-11 02:36 | constrain | Let's not reinvent the wheel. | honoured; a standing rule | constraint |
| 10-11 02:42 | propose | we could work with that one [extra futon1b] and then switch it over | adopted in the plan | answered |
| 10-11 02:42 | propose | we're pretty much in a position to "hook it all up" | plan steps 1, 2, 4 built; 3, 5 not yet | **partly open** |
| 10-11 02:42 | constrain | I don't really want all of the agents' turns to be read by a second agent | honoured; a standing rule | constraint |
| 10-11 02:42 | propose | the claude-14 transcript ... "your agent chat is a crime scene" | planned (step 5); not run | **open** |
| 10-11 02:44 | delegate | bell a description of the work so far and the plan to claude-16 | sent; reply received | done |
| 10-11 03:36 | delegate | (claude-15's reply) backfill → codex-70 | open when computed; codex-70 has since completed it | done (later) |
| 10-11 03:36 | ask-action | please restore the setting & backfill | done | done |

**Observations, corrected the same day.** The first version of this note said
the replies quoted the operator's words in their brackets and still nothing
closed. claude-16's first finding, re-checked by claude-15, shows that was
wrong on both counts:
- The turn records in the outage window carry no `reply_text`: 0 of 93
  analysed records on 10-09 and 0 of 117 on 10-10, against 135 of 162 on 10-04.
  So these ports were computed from operator fragments alone. There was nothing
  to link, and neither `link-quoted-brackets` nor the adjacency table was
  reached.
- Where replies exist, the brackets are mostly labels ("where we are"), not
  quotes: 15 of claude-15's 17 in this session.

The replies are on :7073 as agent-chat/turn evidence and can be backfilled;
claude-16 owns that plan (M-象-2000, from 2026-10-11).

Two observations stand:
- Constraints (`constrain`) count as open ports, though in an inbox they are
  standing rules, not obligations.
- An act done later, outside the session's turns (the codex-70 backfill), stays
  open until a later turn links its completion.

**Scale.** Across all sessions mirrored since 10-01 (2,423 records, 97 sessions),
1,857 ports were open on 10-11:

| source | open |
|---|---|
| operator fragments | 822 |
| agent-to-agent bells | 681 |
| agent replies | 354 |

473 were more than a week old. If this session is typical, most of them are
unlinked answers.
