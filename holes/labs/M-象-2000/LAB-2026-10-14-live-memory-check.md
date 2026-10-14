# LAB 2026-10-14 — live check of the 象 memory path (claude-16)

Written 2026-10-14T16:28Z. Joe asked for assurance that the basic memory system works,
can be brought online and is fully integrated, and for live demos over his
current interactions. This note follows one live turn (Joe's ask itself,
`turn-NhxIGZ`, claude-16-turn-101, 16:12:48Z) through every stage and reports
what each stage did. Read-only except where stated.

## Stage by stage

| stage | what happened | state |
|---|---|---|
| 1 capture (Emacs → `~/.emacs-graph/session-turn-analysis`) | record written 16:12:48Z, origin operator, surface typed, recorder mode `jvm` | works |
| 2 interpretation | 小象 draft (naive Bayes) propose/constrain, unsure; 象-sonnet analysis 16:14:18Z: `qualify` + `ask-action`, one pattern rejected with reason. 90 s turn to reading | works; pool seats 象-1..4 benched on quota until 17:02Z, 象-sonnet carrying |
| 3 retrieval into the prompt | header carried war-room/wr-26 (score 0.32), math-informal-CA/pass-to-a-subsequence, p4ng/call-a-synthesizer | works, loosely relevant |
| 4 evidence row in futon1b | record `evidence_id` null. The REPL buffer's last delivery outcome is `retry`; 452 "queued for durable retry" messages since this Emacs started (10-08). Outbox empty now, so the rows were delivered late, but the record never learns the id: `agent-chat--last-evidence-id` is set only on `acked` | integration gap |
| 5 futon1b (:7073) | heap 4014/4096 MB, old generation 99.4–99.6 %, a full GC about once a second (jstat FGC 9471→9475 in 4 s; 5906 s total in full GC); `evidence?session-id=` answers 504 `query-deadline-exceeded` or hangs 40 s; `evidence/count` hangs; cheap hyperedge read 0.7 s; `/health` 3 ms, so zone-health reports PASS. `evidence-text` sidecar `not-reconciled-since-boot` (text search not serving). Up 2 d 09 h. Unit `futon1b-zone.service` has MemoryHigh=infinity; the vitality sampler targets the retired user unit `futon1b-server.service`, so there is no heap log and no alarm | degraded: reads time out, writes retried |
| 6 frames + McCarthy checks | `turn_frames.py` fell back to files alone ("evidence fetch failed"); 54 frames, 10-11 02:46Z → 10-14 16:12Z. `xiang_acceptance.py`: R18, R16/R29, R3/R4, R6/R19, R19 PASS; R17 FAIL 53/54 because HAPPENED is empty, which is stage 5 not a missing reply | checks run; one fails for the stage-5 reason |
| 7 brief | `resume_brief.py --since 2026-10-13` died on a naive-vs-aware datetime; fixed 6343241a3 (4 tests pass). Brief joins 6 records in 5 sessions for M-象-2000; "Awaiting other agents" lists 59 ask-actions, the Zai requisitions already closed by bellbacks | renders; closed asks still count as open |

Today's operator records: 42. The 26 Matrix-surface ones (recorded through
the JVM route by the bridge) all carry an evidence id; 16 of the typed-surface
ones since 13:21Z carry none. Frames and acceptance output:
`~/notes/xiang-live-2026-10-14/`.

## What "brought online and fully integrated" needs

1. **futon1b out of its GC spiral.** The store is nominally healthy and
   actually unable to answer an evidence query inside its 5 s deadline. A
   restart clears the heap; Joe's 2026-09-08 ruling says futon1b restarts need
   no quiet time (durability is client-side, and the Emacs outbox is that
   client). If the heap pegs again within hours, the working set has outgrown
   `-Xmx4g` and the memory incident note's remedy 1 (raise the heap, set
   MemoryHigh with headroom) applies. Decision: Joe (shared JVM).
2. **Record the stable evidence id on `retry`.** The id is deterministic and
   delivery is durable, so the turn record can carry it with a delivery state
   instead of null; the acceptance check that needs an `acked` id keeps its
   own variable. Small Emacs change in `agent-chat.el` near the chat-turn
   emit; not made here (live Emacs).
3. **Point the vitality sampler at `futon1b-zone.service`** so heap over 85 %
   alarms before an operator turn is the first symptom (the note's remedy 4).
4. **Closing rule for asks answered by bellback**, so the brief's "awaiting"
   list stops showing finished requisitions (the false-open problem, now on the
   brief side).

## Live demos available now, all loaded in the running Emacs (checked `fboundp`)

- `M-x turn-stepper` in a REPL buffer: SAID / PARSE / PATTERNS / HAPPENED
  per operator turn of that session; HAPPENED fills once futon1b answers.
- `M-x xiang-resume-brief` → M-象-2000: the brief over this mission's records.
- `C-c s o`: this session's overview panel; `C-c s c` toggles cue underlines.
- feed.html at zone.hyperreal.enterprises/wip/turns (operator turns only since
  26ea4df98; nothing schedules its regeneration).
- `M-x proof-dialogue-demo`: the frozen t94A01 trace (codex-75, 725869826).

## Addendum 2026-10-14T17:36Z: the E-cool-memories features (minus40 / M40°)

Joe asked after the features from E-cool-memories. Those live in `minus40`
(40ºBelowDB), a separate scratch futon1b on :7093 (`-Xmx1g`, a `systemd-run`
scope, not a unit), fed by two Claude Code hooks in `~/.claude/settings.json`
(PostToolUse `ingest_hook.py`, SessionStart `brief.py`) plus three loops
(`follower.sh`, `relate.py loop`, `fault_intake.py`). Nothing in futon3c's
`src/`, `emacs/` or `scripts/` references it; the resume brief's Facts section
is a stub ("no minus40 fact read in this version").

| feature | checked | state |
|---|---|---|
| `m40 net-changed 2026-10-12 2026-10-14` | 26 facts added, 0 removed, 1.4 s | works (after the restart below) |
| `m40 history read-the-clock` | 13 events, the 10-12 rewrite shown as 4 stops + 3 starts | works |
| `export-delta` piped into `CoolMemoriesTraceChecker.lean` | `M40-DELTA-ACCEPTED 1953→1979`, 3.4 s | works; the P4 live check reproduced on today's data |
| Lean fixture check | `M40-DELTA-ACCEPTED 2→2`, 1.7 s; oleans for all five CoolMemories targets built 10-13 19:51 | works |
| `scripts/test_m40.py` | 6 tests pass (run as a script; `-m unittest scripts/test_m40.py` finds 1 and errors) | works |
| scratch store :7093 | accept backlog 51/50 and connect timeouts from 03:03Z until a restart at 17:30:58Z (14.5 h); the hook logged 826 `FAULT-RECONCILE timed out` lines and advanced no state; follower outputs frozen 02:55Z–17:34Z; `/health` on :7092 kept answering, so nothing alarmed | was down most of today; the restart was not from this seat |
| SessionStart brief | `brief-enabled.txt` lists claude-15 only; claude-16 gets none | opt-in, one seat |

The store's failure shape is the 2026-07-13 futon1b one (`-Xmx1g`, socket
open, accept queue full, health acceptor alive). The same `ExitOnOutOfMemoryError`
plus `Restart=on-failure` that fixed it for :7073 is absent from the scratch
store's command line.

## Addendum 2026-10-14T17:43Z: the three steps, done

1. **Store as a unit.** `~/.config/systemd/user/minus40-store.service` (copy in
   minus40 `scripts/systemd/`, 592e0a3): `-J-Xmx2g -J-XX:+ExitOnOutOfMemoryError`,
   `Restart=on-failure`, start limit 3 in 5 min. The ad hoc scope was stopped at
   17:40:50Z and the unit answered `/health` at 17:41:08Z; the JVM command line
   shows the 2g heap after the alias's 1g, so the override took. Follower,
   relate and fault loops carried on; `net-changed 10-13 10-14` answers in 2.4 s.
2. **Session brief for all seats.** `brief-enabled.txt` = `all` (old list kept
   beside it). A dry run as claude-16 produced 3,921 chars: open asks with no
   answer linked, then recently changed memory. Its "open asks" list my own
   reply paragraphs (㊩ ㊭ 🈸) as asks by claude-16, since agent replies are read
   as acts: a thing to watch, not a fault of the brief.
3. **Facts in the resume brief.** futon3c bcbe204c8: the section reads minus40
   through m40.py, keeps facts true now that mention the mission id or its
   excursions, newest first, cap 15, with a cache fallback and a note when the
   store is down; `--no-facts`, `--fact-terms`. Live for M-象-2000: 3 facts
   (read-the-clock, zai-five-hour-window, minus40-memory-store), 1.0 s.

Seen on the way: the scratch store holds minus40's own test projects
(`cc:-m40-bench-corner-…`, `cc:-tmp`, hooktest/crysttest memories), so a term
like `minus40` matches them by id. Not fixed; a store-side filter or a cleanup
of test projects is minus40's call.

## Addendum 2026-10-14T18:17Z: joins made after futon1b's repair (Joe: "join things up a bit more")

futon1b :7073 was restarted at 17:30Z by an external agent (heap 877/4096 MB,
session evidence query 0.7 s, text search serving). With it answering:

| join | what | evidence |
|---|---|---|
| frames ↔ store | `turn_frames.py` for this session now has HAPPENED rows (26 frames, 1,505 rows); McCarthy checks R17 and R18 PASS, R19 PENDING on start-heads only | `~/notes/xiang-live-2026-10-14/frames-claude-16-after.json` |
| bell ask ↔ Agency job | the brief asks the Agency for each open bell ask's job (`<job-id>-b-<n>` act ids); ended jobs move to Recently closed with "job done <finished-at>" | 3cda506ef; live: 246 of the campaign's requisition asks closed this way |
| fact ↔ writing turn | each fact's memory version at its said time names the writing session (minus40 hook); the latest operator record in that session at or before then is shown under the fact with what Joe said | 3cda506ef; live: all 3 M-象-2000 facts resolved, e.g. zai-five-hour-window written during claude-15-turn-50 after "happy to go with your direction on this one" |
| record ↔ evidence row | 62 operator records since 10-12 with `evidence_id` null now carry their row's id (`evidence_delivery` "backfilled") | f5e1ecd39, log `data/evidence-id-backfill.jsonl` |
| record ↔ row, going forward | Emacs passes the outbox's stable id as `evidence_id` with `evidence_delivery` queued/acked; server stores it; both live-reloaded (operator Emacs forms, JVM turn-record + http) | f5e1ecd39 |

**Found on the way: one exchange, two turn ids.** Emacs emitted the operator's
evidence row before advancing the turn counter, so in every typed session the
user row carried turn N-1 while the assistant row and the 象 record carried N
(10 of 10 acknowledged claude-16 records checked). Fixed in
`agent-chat--start-turn` (f5e1ecd39) and live in the operator Emacs since
18:14Z; rows written before then keep the offset, which the backfill tolerates
(turn N or N-1, text equal). Anything joining user rows to records by turn id
alone over history must allow for it.
