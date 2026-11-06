# Mission: M-ways-of-working — declared ways of working, each with a probe that can fail

**Date:** 2026-11-03
**Status:** OPEN — INSTANTIATE in progress (6 of 12 probes built; records-warrant, selection-reason added 2026-11-06, `f97d33669`, `70432621c`; tail-bite built same day `3f7d99b7c`)
**Owner:** claude-5 (Joe opened it 2026-11-03; took over from claude-16's handoff)
**Repo:** futon3c (`holes/excursions/ways-of-working.json`,
`scripts/ways-of-working-probe.py`, `scripts/publish-cascade-snapshot.sh`),
with patterns in `futon3/library/ways-of-working/` and
`futon3/library/creativity-commons/`.
**Excursions:** E-autoclock-wow
**Published at:** https://zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html
(Declared Ways of Working panel; published by the `wip-cascade-snapshot`
timer — unparked 11-04, publishing every minute; earlier "by hand" note was
out of date, claude-6 11-06).

## HEAD

Joe, 2026-11-02: declared ways of working must show up in Cascade Live, with
exactly which are off, broken or not working. Joe, 2026-11-03: "make it
mission scale, i.e., M-ways-of-working since it covers several items at the
same level of complexity as E-autoclock-wow (and it may be extended further
later). As a whole, M-ways-of-working would relate to the creativity-commons
patterns."

A way of working is prose until it has a probe. A probe counts only if it
can fail: each one gets a test that constructs the bad case its way names,
and shows the probe catching it.

## Scope

**In:** the 11 ways declared in `ways-of-working.json`; one probe per way, or
a recorded reason why it stays unprobed; the findings those probes surface,
each sent to its owner; new ways that come from this work, such as the
requisition rule in E-autoclock-wow.

**Out:** fixing every finding inside this mission. A finding goes to its own
item, which must be agreed (E-autoclock-wow), and is linked from here.
Publishing `real-users` values: these stay in futon7 `holes/cui-bono.md`.
The inbox-zero watcher phase stays off (Joe).

**Time box:** review at 2026-11-17. Two weeks is enough to build or rule out
the remaining probes; after that, split rather than extend.

**Exit:** every way reads `on` or has a recorded reason for `not-built`;
every `on` probe has a constructed-bad-case test; every failing invariant has
an owner item linked here; and the panel is published by a timer, not by
hand.

## State of the ways (audited reading, 2026-11-04/05)

Eight Zai audits, one per way, each spot-checked by claude-16. Table, reasons
and fix list: `~/notes/ways-of-working-audit/2026-11-04/SUMMARY.md`.

| Way | Probe | Published | Audit verdict | Best reading of the way |
|---|---|---|---|---|
| 0. status-is-known | `status-known` | on, failing (declared 11-05, `67232384b`, `fae2f7e6a`) | — | failing only on the 3 ways not built (eat-your-own-tail, records-carry-warrant, selection-gives-a-reason); the other 9 probed ways are fresh, readable, have a test file and a probe. Test-file presence is the weak part of (c): it does not prove the test constructs the bad case |
| operator-turns-logged | `operator-turns` | on, holding | partly correct → join fixed 11-05 (`497e7b2f6`) | holding: 84/84 matched (10 min window; ratio ≥ 0.95 for turns ≥ 200 chars). Population still misses voxterm-direct, phone/cloud, IRC bridge, bell-prompt turns, other homes |
| feedback-reaches-every-participant | `feedback-reaches` | on, failing | no reading possible → fixed 11-05 (`583264790`, `4c2dd02b9`) | failing: 28/420 bell returns unproved after 30 m; 51/58 replies fail the marks form; 0/425 bells lack a return path |
| stays-alive-and-paid | `zone-health` | on, failing | correct; probe hardened 11-05 (futon3c `fff085d6a`, futon0 `6429d69`) | failing: federation (lucy), parked timers (Joe's rulings). A missing check now reads broken; a stopped disk-watch fails write-rate. Bills/runway: logged-in page only (Joe 11-05); export held (Joe 11-05) until the Mozilla case for support ("cui bono" fall-through: claude-6 working through the backlog, codex-7 making it live in Cascade Live) shows where financials fit, since they are one small facet of that case |
| one-queryable-self-account | `cascade-standards` | on, failing | partly correct → fixed 11-05 (`5d0d881c8`, page `7b2ce53ae`; Joe: "let the WM sort itself out") | failing: s1–s4 unchecked (no literal true remains), s6 not built; s5 true. futon2 META selection now refuses :cascade-standards-incomplete until real checks exist |
| backlog-understood | `draw-pile` | on, failing | correct; punching added 11-05 (`4e264c04c`, `400657354`) | failing: 404 unplaced (172 open, 232 closed unattested). 227 punched by Joe's rulings: 12 repair tickets with proof, 84 E-kimi by the dated amnesty, 131 E-kimi confirmed DONE |
| inbox-zero | `git-clean` | off, failing | partly correct → population fixed 11-05 (`f41ed6adc`, declaration `3d72468ed`) | failing: 28 of 63 repos dirty or unpushed (storage 862 ahead, minus40 124 with no remote, futon2/futon3c ~900/700 untracked); 149 leaked fixture dirs excluded and reported |
| intents-are-tracked | `intents-tracked` | on, failing | partly correct → fixed 11-05 (`d10e68ed3`, review `556643076`) | failing: 3 stale asks (codex-2 10-21 pair, claude-15 f-s2-0) and 761 of 1,272 operator turns 象 never analysed (one named violation); pasted text no longer counted; 4 Zai-seat acts shown as unwitnessable, not stale; invalid records 20,083 → 2,109 (象 artefacts excluded). Still cannot see commits or bell-borne asks |
| dispatches-name-an-agreed-item (E-autoclock-wow) | `autoclock-gate` | on, failing (declared 11-05, `9dd44d294`, `8eb7f2a08`) | — | failing: gate not built (`admit-request`: zero lines; claude-4); 53 E-kimi-task-* minted in 24 h |
| records-carry-warrant | `records-warrant` | on, failing (built 11-06, `f97d33669`) | — | failing: every test-result claim in bellback replies (7 in the 7-day window, 0 carried) is a pasted count with no test-registry-… id; violations name the jobs. Finding 7 fixed same day (`24a015589`): the venv at storage/test-registry/futon3c-suite/venv registers all 11 probe test files, `check` returns warrant? true |
| selection-gives-a-reason | `selection-reason` | on, failing (built 11-06, `70432621c`; runner-up made DISTINCT after claude-6 review, `d78704cff`) | — | failing: 2 of 96 clicks in window collapsed to one distinct target — recorded as finding items per claude-6 (bellback 11-06): run `36dbc737` (08:46Z, click ad24a43f) chose `E-comprehension-foundation`, cascade-picks only itself; run `ade71eab` (09:09Z, click ba4fb959) chose `M-war-machine-aif-completion`, cascade-picks only itself. (claude-6 first recalled an E-queries-and-scopes collapse; the records say otherwise, so the records stand.) A chosen click with one distinct option violates Lean's Q2 floor of 2 — the WM should have abstained (`:insufficient-comparison`). Both runs predate futon2 `b914a28d9` (2026-11-06 09:59:54Z, one-cascade fixture expects abstention), so per claude-6 both are **history**: no failing click after the fix, and the way should hold from 09:59Z onward. (Correction, claude-6 11-06: the 10:4x loop pause was separate — those post-fix clicks abstained twice with `:insufficient-comparison`, which is the correct behaviour; pause logged at `~/notes/wm-next/tests/clicks-loop-q7sel4.log` 10:45:08Z.) Upstream cause (stale/unread placed targets collapsing the menu) under repair: re-read queue, Kimi second-reading lane, Giraffe unresolved-dependency handling (`39933b51d`, `0c95df57a`). The other 94 clicks carry a typed reason. Records read, never recomputed |
| eat-your-own-tail | `tail-bite` | on, failing (built 11-06, `3f7d99b7c`) | — | failing: 815 of 993 work jobs (invoke ledger, 7-day window) lack an in-run observation or a citable record — 82%: the cold-state case is the norm, as the way's problem statement says. Design (Joe + zai-9 11-06): the global "tail gets eaten" claim is the halting problem, so the invariant is bounded per futonic-loop turn — (1) 香 during 化 (a text observation timestamped inside the run window), (2) 債 from 香 + (3) 既 citable (the durable result-summary cites a sha/warrant/test-registry id — the ratchet). No convergence claim; improvement stays in 味. Records read, never recomputed; 10 tests incl. both constructed bad cases (cold observation; live-observed-but-uncitable), warrant `test-registry-11628d49…`. Full triadic closure waits on the GCC (M-jvm-arxana → M-repl-circus → feedback-reaches as application layer, Joe 11-06); the probe is the ratchet recording which parts of the habit are missing. Theorization via futonic-logic (Peircean triad in futonic glyphs); native-Chinese pass completed as futon2 E-kimi-task-354 (kimi-1, done 11-06 20:47Z; text at storage/kimi-1-excursions/E-kimi-task-354-zishiqiwei.md). Its verdicts worth keeping: 鹽/債/既/化/香/味 excellent as native words (債's 應/債 split is felt, not philosophical); 咅 is a bad pick (its Shuowen sense is spitting-refusal — native readers would use 破 or 白); 部 reads as frozen bureaucracy but that freezing may itself be ratchet at the decomposition layer; the way's Chinese name should be 自尝 ("tasting the soup one ladle at a time" — halting is common sense in the kitchen, no Turing needed); plus two un-asked tensions: 香 fatigues (no time-constant in the vocabulary — a "change the ladle" obligation may be owed periodically) and 鹽 doesn't dissolve in a cold 皿 (the loop has a cold-start problem: no 既 to cite on first tasting) |
| real-users | none | unprobed by design | — | values live in futon7 |

Way 0 (Joe, 2026-11-04): "we know the status of our ways of working". A way's
status is known only if its reading is fresh, its probe has a test that
constructs the bad case and sees it fail, and each failure names an owner item.
A current reading is not yet the assurance that it stays current (Joe): the
way-0 probe is what supplies that.

## Candidate ways not yet declared (Joe, 2026-11-03)

Joe: "m40 hasn't been used lately (to my knowledge) and I want to understand
why. There's a whole way of working around memory, mining patterns, updating a
giant connected cascade that isn't recorded in the Pattern Pipeline Cascade
... a lot of recent work has been dealing with Giraffe, 馬, WM and related
topics and I feel like that is forming a cluster of work that needs finishing
and bringing online along with the other WOW items even if it is more complex
than many of those."

**12. Memory is used: minus40.** Measured by claude-5, 2026-11-03:
- Across every Claude transcript of the last 30 days there were **5** minus40
  tool calls (`m40_find` 3, `m40_near` 2): 4 on 10-29 and 1 on 11-01.
- minus40's last commit is 10-17 (`b68c852`). The store
  (`minus40-store.service`, :7093) is running.
- What still runs is passive: the SessionStart brief (`brief.py`) and two
  PostToolUse hooks (`ingest_hook.py`, `call_hook.py`). In this session they
  flagged a near-duplicate memory correctly, so ingest is current.
- Why the tools go unused, as far as the records show:
  1. No CLAUDE.md or AGENTS.md mentions minus40 or when to reach for it.
  2. Its eight MCP tools are deferred: a session sees only their names until
     it runs a ToolSearch.
  3. The brief's "open asks with no answer linked" are mostly 10-19/10-20
     turns, so it reads as stale.
  4. Codex seats get none of it.
  5. Memory work moved to Zai seats on 10-19 (memory
     `zai-runners-for-memory-work`), and no minus40 commit followed.

  A probe would count tool calls and brief items acted on per day.
  **Done 11-03:** a "minus40: ask the memory before you answer from it"
  section in `~/code/CLAUDE.md` (not a git repo, so this is the record) says
  when to load which tool. The brief's staleness is part of the
  intents-are-tracked discovery (codex-7). Re-measure tool calls on 11-10.
  Brief fixed 11-04 (zai-5, minus40 `c539e72` and `590d054`, futon3c
  `ce96a852f`): open asks come from the latest 象 record per session, keyed by
  (session, act id); claude-15's brief went from 12 asks dated 10-11 to 10-14
  to 12 from its newest session. Warm run 0.6 to 1.2 s.

**13. The selection cluster: Giraffe, 馬, WM.** M-giraffe (the work pool as
data), the 馬 reading and occurrence lanes (`ma-reading` 805 and
`ma-occurrence` 467 bells in the last 6 days), and the War Machine's
selection. It bears on `selection-gives-a-reason` and `backlog-understood`,
and on E-autoclock-wow's surprisal accounting, which reads Giraffe's field.
Bringing it online is larger than one probe. It needs its own excursion
under this mission, with M-giraffe's owner (Joe + claude-11) and futon2's
War Machine rules (Claude proposes and reviews; Codex builds).

**14. The pattern cascade is kept up to date.** Memory, pattern mining and
the connected cascade are not one of the 11 declared ways, and the Pipeline
Pattern Cascade page does not show them.

## One connected graph for work, memory and patterns (Joe, 2026-11-05)

Joe: "the commons map is lovely but it is the *start* in my view of a Giant
Connected Component that we are going to want to have in XTDB". Three
consumers each need a current view of part of it:

- Giraffe: a priority-ordered cascade of work and its adjacent possible.
- minus40: an up-to-date sense of memories.
- 馬: an up-to-date sense of mined patterns.

Joe expects these to be compatible in ways not yet formalised; see
M-formal-agency (opened 2026-11-04). Today the commons map is uncommitted
TSVs in `~/notes/commons-map`, read by the draw-pile probe for placements.
Candidate ways 12 and 14 (memory upkeep, mined-pattern upkeep) belong here.

## Graph cache serves stale and refreshes in the background (2026-11-05)

From 00:23Z to 00:50Z the WIP page did not publish: the mission-scope/pattern
section passed its 30-min age limit, its walk now takes ~172 s against the 90 s
request deadline, and a failed walk neither refreshed nor served the entry.
Fixed by zai-4 (`d064364a8`) with two review fixes by claude-16: separate
supervisor and walk pools (`7dbc52d81`; one 2-thread pool deadlocked when two
sections were due) and a moved count served stale too (`a681bcd24`). Only a
missing entry or failed placeholder now walks in the request path; everything
else is served marked stale and refreshed once in the background. The slow
walk itself (futon1b cold pages) is still open.

## Open findings, each to become its own item

1. ~~The consent-gate sweeper sends without `--from`~~ Resolved
   (`d7ea790eb`): its bells are brief-mode notifications, which need no return
   path, and the probe now exempts them. Live: 0 of 202 work bells without a
   path; 65 notifications exempt.
2. **The `wm-full-loop` inbox: resolved for the probe, files await Joe.**
   codex-12 (`~/notes/packets/ways-of-working/wm-inbox-discovery-REPORT.md`):
   `wm-full-loop` bells, then polls the original job to a terminal state
   (futon2 `flight_runner.clj:120-168`: "a bellback has no return path to
   the machine's persona"). So the 507 inbox files are redundant copies,
   and Agency's "delivered" there only means a file was written
   (`http.clj:2779-2801`). The probe now takes a polling caller's terminal
   result as receipt (`5127f81da`, with a test). Spot-checked: 507 files, 0
   consumed, 171 inbox directories.
   **Open:**
   - (a) ~~Joe decides the 507~~ — **executed 2026-11-06 (Joe: "manifest
     then archive")**: 593 files then in the inbox (grown from 507 since
     the 11-03 discovery) manifested with per-file sha256, tarballed,
     round-trip verified, and removed; archive at
     `~/notes/packets/ways-of-working/wm-inbox-archive-20261106/`
     (manifest.jsonl, wm-full-loop-inbox.tar.gz, archive.sha256
     `05cffb94…`). New arrivals continue to accumulate until (b) is fixed.
   - (b) Agency could stop writing completion bellbacks for polling callers.
   - (c) 166 of the 171 directories are `ack-seat-*` / `pull-seat-*` test
     callers writing into the live inbox, the same shape as the
     invoke-ledger test clobber.
3. **futon1b cold pages — approach settled (Joe 2026-11-06: "OK to C").**
   The `/hyperedges` type-only branch re-checks each row in XTDB
   (`fetch-hyperedge-cols-by-ids`, one point-read per row). Baseline
   25–37 ms/row (docstring, 2026-10-29); 2026-11-06 20:56Z: 25.6 s for
   `type=mission-scope/pattern&limit=100` (~255 ms/row, load avg ~10,
   futon1b-zone ~350% CPU with background census/doc-fetch loops).
   **Fix (C): serve the type-only paging shape from the SQLite sidecar,
   exactly as the `end-prefix` branch already is** ("served ONLY from the
   sidecar"); extend the current-valid per-type projection to the walk's
   paging shape, with a before/after page measurement recorded in the
   ticket. Contention from the background loops stays a separate owner
   item (every other reader still goes through XTDB). zai-9.
   **Journal volume settled same day:** start line demoted to opt-in debug
   (`-Dfuton1b.request.start.log=true` or a binding re-enables; futon1b
   `8092333`); end lines keep elapsed/outcome. Inert until the service
   restarts — deliberately NOT restarted yet because codex-21's uncommitted
   M-jvm-arxana work (`futon1b_gates.clj` +51, `futon1b_arxana.clj`)
   compiles from the same source tree; restart lands when theirs does.
3. **The reply census misses most replies: cause found, fix dispatched.**
   codex-15 (`reply-census-discovery-REPORT.md`): final replies exist, but
   `in-reply-to` points at the latest session row, so tool activity makes a
   reply point at an intervening row. Joining on `(session-id, turn-id)`
   finds replies for 66 of 66 turns, all 24 of claude-5's. Build dispatched
   to codex-15. A turn's evidence text holds the narration between tool
   calls as well as the final reply (claude-5 turn 1: 1,482 chars, starting
   "Found the cause: …"). **Settled (Joe, 2026-11-03):** the marks apply to
   the final reply only. Narration between tool calls is self-talk, "not
   speech acts in the normal sense", and is better left unmarked. Since the
   evidence does not separate the two, the probe takes the reply to start
   at the first paragraph that opens with a mark (the proforma puts the ㊥
   gist first) and judges from there; a turn with no marked paragraph is an
   unmarked reply. Applied at review of codex-15's build, not re-belled.
3b. **Why replies fail the marks form (24 h to 11-04 00:10, 80 replies).**
   Judged from the first marked paragraph on, so narration is excluded:
   - first paragraph is not the ㊥ gist: 32 (all four Claude seats);
   - an unmarked paragraph after the first mark: 12 (claude-5 10, claude-14
     2), plus 2 unmarked tables/lists (claude-2). These are real: the
     reply carries on without a mark;
   - a mark with no bracketed target: 3 (claude-16);
   - no mark at all: 21, of which codex-9 18 and codex-7 2. The proforma
     was in `~/code/AGENTS.md`, but Codex reads AGENTS.md only from the git
     root down to its working directory, and these seats start in `~/` and
     `~/code/mfuton-share`. Fixed 11-04: the proforma is one file,
     `~/code/reply-proforma.md`, @-imported by `~/code/CLAUDE.md` and
     symlinked as `~/.codex/AGENTS.md` (a `codex exec` from `~/` quotes it);
   - one invented mark: claude-5 wrote 🖖 four times on 11-03 (not in the
     key; 🈖 explain was meant). Joe: it recurs, so the proforma now says
     so. If the note does not stop it, replace 🖖 with 🈖 in code.
   10 of 80 pass (claude-5 7, claude-2 3). Review notes: codex-15's join
   test fails against the old in-reply-to join (checked by substitution).
   claude-5's commit `815791320` swept in codex-15's uncommitted
   first-marked-paragraph change, so that change carries claude-5's
   trailers.
3a. **intents-tracked review (claude-5, 11-03).** Two defects fixed in
   review, both with tests that fail on the old code:
   - The latest-record test passed even when every record was read, because
     closures are collected from all records either way. A new case (an act
     the latest record no longer carries) fails under that mutation.
   - Act ids restart with each session of a seat: 521 of 8,012 operator act
     ids occur in more than one session (`zai-7-turn-1-f-s1-0` is a 10-16
     delegation and a 10-31 bug report). The probe keyed on the id alone,
     and its "duplicate counts once" test pinned that merge. It now keys on
     (session, act id); a sidecar answer closes only its own record's
     session.
   The live count is unchanged at 6. By hand:
   - zai-8 "please add the small change for unified behaviour" (10-19
     04:46): **met** by zai-8's commit `e27d3bf93` 78 s later. The probe
     has no commit witness, and the commit's Agent-Session
     (`zai-10e2f699-…`) is not the 象 session id, so the join would be by
     seat and time.
   - codex-2 "can you see what claude-2 is doing?" and "I don't have
     visibility?" (10-21): **never answered**. The turn failed with a Codex
     thread-store conflict. Obsolete now.
   - zai-5 "Are these uncommitted files to do with your work?" (10-18),
     zai-6 and zai-7 "You will help with E-implementing-G-over-cascades"
     (10-16): **cannot be told**, see finding 6.
   No registered run: the registry accepts only pytest commands, and the
   system python has no pytest (finding 7).
4. **futon1b logs a start and an end line per request:** about 170k
   journal lines a day (futon0 README-hardening §1).
5. ~~Warrants do not reach the evidence store.~~ Withdrawn: they were never
   meant to (Joe, 2026-11-03: "I think they were meant to go into a
   sidecar"). The sidecar is `storage/test-registry/warrant-index.sqlite`
   (`sqlite_backend.clj`, since 10-04), and it is healthy: since the 10-20
   boot it records 833 registered runs, 698 of them warrants, the newest at
   22:59Z today. Only the futon3c CLAUDE.md I-6 enumeration line was stale;
   corrected. **What the sidecar does show:** registered runs are almost all
   futon2 (792). futon3c's canonical checkout has **1**. Every futon3c test
   run today (codex-7, codex-13, codex-15 and claude-5's reviews) was
   unregistered, a pasted count.
6. ~~No review record joins a commit to its warrants.~~ Withdrawn (Joe,
   2026-11-03): "warrants apply to tests, not to commits". The way counts
   test runs and the claims that rest on them, not commits, so no
   commit→warrant join is needed. The discovery's 320-commit population is
   not the denominator. A probe candidate: test-result claims in
   bell-backs and reviews ("Ran N tests …") that carry no warrant id, i.e.
   pasted counts, against those that do.
6. **Zai seats write no assistant rows to evidence.** For the four Zai
   sessions above, futon1b holds only the operator's rows, and the 象 record
   has an empty `reply_text`. So no probe can see whether a Zai seat answered.
   This affects intents-tracked, feedback-reaches and the reply census.
7. ~~**futon3c's Python probe tests cannot be registered.**~~ **Fixed 11-06
   (zai-9, `24a015589` + the registration run):** the venv lives at
   `/home/joe/code/storage/test-registry/futon3c-suite/venv` (pytest 9.2.0,
   python 3.12) — outside the checkout, because a venv inside the repo puts
   site-packages in the load closure and the `scope-not-committed` gate
   correctly refuses the warrant; outside-repo venvs are fingerprinted by
   dist ("immutable-by-convention", `test_registry.clj` python-fingerprint*).
   `scripts/pytest_warrant_suite.py` gained `--pattern` (futon3c names are
   `test_*.py`, the built-in default was mfuton's `*_test.py`), with a
   bad-case test (`scripts/test_pytest_warrant_suite.py`). Sweep:
   `WARRANT_AUTHOR=zai-9 PYTEST_ADDOPTS="-p no:cacheprovider" <venv>/bin/python
   scripts/pytest_warrant_suite.py --repo . --test-dir scripts --pattern
   'test_ways_of_working_*.py' --python <venv>/bin/python --code-paths
   scripts/ways-of-working-probe.py --index …/futon3c-suite/index.json
   --artifacts …/futon3c-suite/artifacts --bin-dir …/futon3c-suite/bin`.
   All 10 probe test files registered, 78 passed, ~17 s, and `check` returns
   `warrant? true` (e.g. `test-registry-f8a23ccc…`, `…155b5f874`). The
   index is `storage/test-registry/futon3c-suite/index.json`; future probe
   builds cite it instead of pasting counts.

## Hygiene: dispatch as a fourth generation (Joe, 2026-11-03)

Joe: "There's a broader class of 'hygiene' patterns that deal with warrants,
Inbox Zero, and that maybe even touch on agent dispatch themes that would link
with M-象-2000 though I don't know if that link has been made explicitly."

`futon3/library/hygiene/README.md` names three generations of one upkeep
policy: buffer cleaner, inbox zero and test registry. Its six shared patterns
fit the requisition gate (E-autoclock-wow) as a fourth column:

| hygiene pattern | dispatch / requisition (proposed) |
|---|---|
| observe-the-authority | read the clock from the Agency's durable decision, not the buffer. 11-03 incident: the buffer was overwritten by the server's stale clock, the inverse fault (`439152345`) |
| classify-by-remedy-judgement | work vs notification vs continuation, by validated mode, not caller name |
| exempt-the-in-use | continuations of an admitted job, and an operator clock-in still in flight |
| receipt-then-gate | the requisition is the receipt; the gate admits on it |
| route-to-who-can-act | a refusal goes to the operator as "clock on what?"; machine callers get a typed error |
| settle-with-meters | the five probe counts in E-autoclock-wow |

M-象-2000 does not cite `hygiene/` (one unrelated "join hygiene" line,
`M-象-2000.md:361`), so that link has not been made. The candidate join is
intents-are-tracked: 象 marks each operator turn's intent, and a requisition
records the item each dispatch serves. Together they would let a probe ask
whether the dispatches made after a turn served what the turn asked for.

**Measurements behind Joe's /tmp remark (claude-5, 2026-11-03 23:1x):**
`/tmp` holds 45,609 entries, 25 GB, all since the 10-20 boot. The largest
families are test scratch: `wm-runner-record-suite-*` 778,
`xiang-svc-*` 740, `reading-decision-{root,graph,flights}-*` 674 each,
`wm-click-bindings-test-*` 661. So the futon2 suites behind them ran about
650–780 times in two weeks. Over the same period
`~/code/storage/test-registry/` gained 839 run logs. The 771 in
`futon2-suite/index.json` is one row per namespace (latest run), not a run
count, so it says nothing about discipline by itself. What can't be told
from this: how many of those suite runs were reruns that a warrant would
have spared. That needs the run logs joined to the `/tmp` scratch by time,
or the test fixtures naming their run.

## Relation to M-the-perfect-crime (Joe, 2026-11-03)

Joe: "the broader issues here relate to M-the-perfect-crime". That mission's
covert question is what catches a failure at the *prose* layer, where an
instrument's high-definition output stands in for the thing it describes
(Baudrillard). Today's work produced several examples where a record or a
count stood in for what it reported:

- a pasted "Ran N tests, 0 failures" in place of a registered run (finding 5);
- "delivered" for 507 returns written to an inbox nobody reads (finding 2);
- "25 of 26 replies malformed", a sharp number from a census that saw 1 in 9
  of one seat's replies (finding 3);
- a CLAUDE.md line telling every agent to enumerate warrants with a query
  that had returned nothing for five weeks;
- `/report`'s 88 subjects, all "no-warrant", read from the wrong authority.

The discipline this mission uses against it, a constructed bad case per
probe, is the forensic mode M-the-perfect-crime proposes for authoring
practice. Its open checklist item ("the chained-claim sweep joins agent
final-turn claims to Joe's next-turn stance") is the same join that
intents-are-tracked and feedback-reaches need. The two missions should share
that sweep, not build it twice.

## Ways and the creativity-commons patterns (draft, claude-5; for Joe to check)

The creativity-commons patterns (`futon3/library/creativity-commons/`) treat
contributors as producers and a shared resource as a place. Read that way,
each way of working is one of the commons' rules, and its probe is that
rule's monitoring:

| Pattern | Ways it bears on | How |
|---|---|---|
| `distributed-monitoring` | operator-turns-logged, records-carry-warrant, feedback-reaches | the probes and reviewer seats are monitoring done by the producers themselves |
| `contributor-boundaries` | E-autoclock-wow, inbox-zero | who may add to an item is recorded where a test can read it: the requisition; committed work is visible to others |
| `place-boundaries` | one-queryable-self-account, backlog-understood | what exists is on the map, or it is a hole |
| `nested-places` | backlog-understood, E-autoclock-wow | campaign › mission › excursion › ticket, with A- items as shared method |
| `producers-can-modify-the-tests` | every probe | a seat that finds a probe wrong can change it, under review |
| `affected-participants-change-the-rules` | intents-are-tracked, feedback-reaches | operator intents and seat findings reach the rule's owner |
| `formative-sanctions` | selection-gives-a-reason, E-autoclock-wow | graduated responses: a question before a refusal; a click can be refused |
| `low-cost-local-conflict-arenas` | feedback-reaches | author/reviewer disagreements settled on the item, with the operator above |
| `benefit-proportional-to-contribution` | real-users | who uses the work, and who it is for |
| `institutions-are-products` | stays-alive-and-paid | the arrangements themselves have to be kept running and paid for |
| `process-fits-local-conditions` | eat-your-own-tail | the running work is its own observation surface |
| `recognised-right-to-self-organise` | (none yet) | |
| `ARGUMENT` | the mission as a whole | |

This is my reading, not yet checked against each pattern's IF/HOWEVER/THEN.
DERIVE should do that check, and should say where a way has no pattern or a
pattern has no way.

## Next

1. Records-carry-warrant: discovery done. The probe reads the sidecar (`registry_runs`) and counts test-result claims in bell-backs and reviews that carry no warrant id. futon3c first needs the registry used at all (finding 5). Until then the way stays `not-built`, with that reason.
2. Open an item for each finding above, with this mission as its parent.
3. Intents-are-tracked: check whether minus40's open-asks list can be the
   census (the session brief already prints "open asks with no answer
   linked").
4. *Done 11-04 (claude-16 with zai-4; Joe asked for updating "only the parts
   that changed").* Each probe writes its own result file
   (`~/.cache/futon3c/ways-of-working/results/`); `ways-of-working-probes.timer`
   refreshes them every 5 min (zone-health 5.5 s, the operator census 3 s with
   its cache, where it took 50-165 s); the publish runs only the two cheap probes
   and assembles the rest; each panel row shows its reading's age, stale after
   30 min. `wip-cascade-snapshot.timer` publishes every minute (futon3c
   `b4a8ee903`, `925696395`, `56bddeb30`, `a157f7e47`; futon0 `2344705`).
   `/api/alpha/cascade-real/graph` took 13 s warm and over 90 s cold, nearly all
   of it the 2,146-row `mission-scope/pattern` walk; it now reads each type's
   exact count (about 0.4 s for all seven) and re-walks only types whose count
   moved, or every 30 min as a backstop (zai-4 discovery
   `~/notes/disk-writes/cascade-graph-discovery.md`; futon3c `4365e4f04`, live
   0.4 s). *Open:* a cold page of that walk costs about 100x a warm one in
   futon1b, which still hurts any other reader of it.

### Checkpoint 1 — 2026-11-03
**What was done:** handoff from claude-16; probes `operator-turns` and
`feedback-reaches` built by codex-7 and codex-15 and reviewed (each with a
constructed bad case); panel published 22:34Z; E-autoclock-wow opened, with
MAP and gate design; mention-autoclock confirmed; unwatched-run auto-compact
and the autorunner Cooked line built (live after Agency restart).
**Test state:** `test_ways_of_working_feedback.py` 5 tests OK;
`test_ways_of_working_operator_turns.py` 5 tests OK;
`agent-pouch-unwatched-compact-test` 8 assertions, 0 failures;
`agent-pouch-autorunner-http-test` 2 assertions, 0 failures;
`test/agent-chat-test.el` 64/64.
**Next:** records-carry-warrant discovery; items for findings 1–4.
