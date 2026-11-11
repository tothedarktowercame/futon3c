# E-dog-helps: how we would know that 狗 is helping

Excursion of [M-repl-circus](../missions/M-repl-circus.md) (Joe, 2026-11-11:
"come up with some analytics that we could use to say whether 狗 is helping
after a while"). Written by claude-6 at claude-13's request (bell
`invoke-1794439126526-17651-149656df`), 2026-11-11. A design only: nothing
here is built, and the packets in section 5 are not dispatched.

Joe's framing: "I am feeling slightly lost as to what we would count as
"success" or "quality improvement" with 狗. In the earlier "memory whitepaper"
work I wanted to see if Zai could be given implanted memories that would
enable it to solve mathematical problems better. A similar idea would apply
to any agents that would be better able to solve coding problems. What we've
achieved so far is a bit of instrumentation; that might be enough to start to
gather data that help us determine whether this animal is helping or not."

## Summary

- "Helping" is defined by four outcomes (section 1). The one closest to
  Joe's experience: he has to restate a point that a memory already holds
  less often when 狗 showed that memory than when it did not.
- The comparison is a **recalled-but-withheld arm** (section 2): 狗 computes
  the recall on every operator turn as now, and on a fixed 30% of turns
  records it without showing it. The use check runs on both arms.
- Turn volume (about 77 operator turns a day) answers one question within
  a week and a second within about a month. It cannot detect a small change in
  Joe's overall correction rate in any useful time, and section 2 says why.
- One thing 狗 already computes and throws away is the best single signal:
  when the bury step finds that Joe's ruling is already a memory, that is
  Joe saying something again. Today that match is dropped without a record.

## What the records show now (read 2026-11-11 23:20Z)

From `~/.emacs-graph/session-turn-analysis/turn-*.json`, records modified in
the last 21 days, `origin: operator`:

| quantity | value |
|---|---|
| operator turns | 1,627 (about 77 a day) |
| turns with a `disagree`, `redirect` or `withdraw` act by Joe | 173 (10.6%) |
| turns where 馬's alignment has at least one `not-answered` act | 398 of 1,542 (25.8%) |
| turns carrying `dog_recall` (live since about 19:10Z today) | 20, of which 19 have items |
| recalled items / marked used by `dog_use` | 43 / 3 (7%) |
| answering seat, last 14 days | claude 886, codex 304, zai 22 |

The act kinds are 象's; I have not checked by hand how often `disagree` or
`redirect` is a correction of the agent as opposed to a change of plan.
Packet A below includes that check.

## 1. What "狗 is helping" means

Use rate (`dog_use`) says the reply touched a recalled memory. It does not
say the turn went better. The outcomes:

1. **Joe restates known points less.** A memory records something Joe said.
   If Joe says it again later, the memory did not reach the agent in time.
   Fewer restatements of points that were recalled and shown.
2. **Joe corrects the reply less on turns where 狗 had something to offer.**
   The next operator turn in the session carries fewer `disagree` /
   `redirect` / `withdraw` acts, and 馬 marks fewer of the turn's acts
   `not-answered`.
3. **Work that a memory warned about fails less.** A dispatch, test run or
   command fails in a way an existing memory describes (examples in this
   store: park deadline units, `--mode work`, tests in `/tmp`). Fewer such
   failures after the memory was shown.
4. **Tasks succeed more often with recalled memories than without.** The
   whitepaper's question, for coding and maths dispatches (section 6).

And one thing that counts against it:

5. **狗 misleads.** A recalled memory is used and the use is what Joe then
   corrects (the lapsed stop-order case from zai-1's review). This is
   counted separately, because a net gain can hide it.

## 2. The comparison

### Design: recalled but withheld

On each operator turn the listener computes the recall as it does now.
The arm is a pure function of the record id (for example, the first byte of
its SHA-256 below 77 gives 30%), so it is reproducible and nobody chooses it.

- **shown** (70%): as now. Items go on the Prompt line and into `dog_recall`.
- **withheld** (30%): items go into `dog_recall` with `arm: "withheld"`; the
  Prompt line carries nothing from 狗. The post-hook's use check and bury
  run as on any turn.

What the withheld arm gives that an on/off switch per day or per seat does
not:

- The same turns, seats, hours and topics in both arms.
- The use check's false-positive rate. A reply can share two bigrams with a
  memory the agent never saw. `dog_use` on withheld items measures that, so
  **shown use rate minus withheld use rate** is the part of use that showing
  caused.
- Every outcome in section 1 can be compared on items that 狗 judged
  relevant in both arms, which an untreated day cannot offer.

Two strata come free and should be reported with every figure:

- **Turns with no recalled items.** The arms are identical there. Any
  difference between them is noise, which is the whitepaper's built-in
  placebo (E-memory-whitepaper-v3-programme, Phase 2b).
- **Whether the seat could already see the memory.** A Claude seat has its
  own project's `MEMORY.md` loaded every session, so withholding a memory
  from that project withholds little. 狗's effect should show in memories
  from other projects and in 狗's own buried ones. If the effect is the same
  in both strata, something other than the recalled text is producing it.

### What it costs

- 30% of operator turns get no 狗 line. Given 7% measured use, that is
  about two used memories a day forgone.
- One small change to the listener and provider (packet B).
- No extra queries. The recall is computed either way.

### What the volume supports

Sample sizes for 80% power at 5% two-sided, 30/70 split, from the rates
above (normal approximation; the script should report intervals, not just
these):

| question | unit | assumed rates | needed | at current volume |
|---|---|---|---|---|
| Does showing cause use? | recalled item | 7% shown vs 2% withheld | about 630 items | about 4 days (roughly 165 items a day) |
| Does showing reduce Joe's restatements of that memory? | recalled item | 10% vs 5%, a guess: no baseline exists | about 1,030 items | about a week once packet C runs |
| Does 狗 lower the hard-correction rate on the next turn? | operator turn | 10.6% vs 7% | about 2,300 turns | about 30 days |
| The same, for a smaller effect | operator turn | 10.6% vs 9% | about 12,900 turns | about 24 weeks |

The third and fourth rows need a caution. If 7% of recalled items are used,
then even a memory that halved the chance of a correction whenever it was
used would move the all-turns rate from 10.6% to about 10.2%. That is the
fourth row or worse. So the turn-level correction rate is reported as a
trend and is not the test. The test is at the item level (rows 1 and 2),
where the outcome is about the specific memory that was or was not shown.

## 3. The analytics

Each metric reads the operator 象 record unless it says otherwise. "Next
turn" means the next `origin: operator` record with the same `session_id`,
ordered by `created_at`. All of them are computed offline by one script
over the record files (packet A), with no change to the live path.

| id | metric | source fields | computation | reads as helping | reads as not helping |
|---|---|---|---|---|---|
| D1 | use uplift | `dog_recall.items`, `dog_recall.arm`, `dog_use.items[].used` | used share among shown items minus used share among withheld items, with interval | uplift of 3 points or more, interval excluding 0, by 630 items | interval includes 0 at 1,000 items: the line is not being read, or the echo rule is measuring topic overlap |
| D2 | restatement after recall | `dog_recall.items` on turn t; `source_text` of the next 3 operator turns in the session; memory body on disk | per recalled item: does Joe's later text echo the memory (the use check's rule, applied to Joe's text) | restated share lower for shown than withheld items, interval excluding 0, by 4 weeks | no difference at 2,000 items |
| D3 | said-again events | bury step's suppressed candidates (not recorded today; packet D) joined to `dog_recall` of the previous agent turn in the session | count per week of Joe's rulings that an existing memory already held, split by what 狗 did with that memory on the turn before: shown, withheld, not recalled | falling weekly count; "shown" share of events well under 70% | "shown" share near 70%: showing the memory does not stop the restatement |
| D4 | next-turn correction | `acts[].kind` on the next operator turn; `alignment.acts[].verdict` on turn t | share of turns followed by a `disagree`/`redirect`/`withdraw`; share with a `not-answered` act; by arm and stratum | trend only (see section 2); direction agrees with D2 | direction opposite to D2 for 4 weeks running |
| D5 | misleading recall | `dog_use.items[].used = true` on turn t, and a correction act on the next turn whose text echoes the same memory | count per week, each one listed with record ids for reading | zero or falling | more than 2 a week after the dated-directive filter: fix recall before anything else |
| D6 | warned failures | minus40 memories whose text names a failure; Agency job records and 🈹 marks in `alignment` | hand-kept case log, one line per failure that an existing memory described, with whether 狗 recalled it on the turn that led to it | cases where it was recalled and shown become rare | cases keep occurring with the memory shown and unused |
| D7 | buried chain | `dog_buried.items`, later `dog_recall.items[].memory` under project `狗`, later `dog_use` | of memories buried, the share recalled within 14 days, and of those the share used; count of retractions (`dog-retraction-*`) | first buried, then recalled, then used chain appears, then recurs | after 4 weeks nothing buried has been recalled: bury is writing what recall cannot find |
| D8 | cost | `dog_recall.query-ms`; length of the Prompt-line segment | p50 and max per week | flat | rising with store size |

D6 is a case log because the volume is a few events a week and the join
(this failure, that memory) needs a reader. It is not a rate.

### Reading the result after four weeks

- **Helping:** D1 uplift established, and D2 lower in the shown arm, and D5
  not rising. D3 and D7 are the supporting record.
- **Read but not helping:** D1 uplift established, D2 flat. Agents take up
  the memories and Joe restates the points anyway. Then the question is
  what is being recalled, and D2 broken down by memory type and by 驴 seed
  versus keyword seed says where.
- **Not read:** no D1 uplift. Stop tuning recall and look at the Prompt
  line itself (position, wording, whether seats are told what it is).
- **Harmful:** D5 rising. Withhold more, fix the filter.

The thresholds are starting values. Row 2 of the sample-size table rests on
a guessed baseline, so the first week's D2 figure in the withheld arm should
replace it and the four-week reading date should be recomputed then.

## 4. What this does not measure

- **Codex seats' own memory.** It is provider-side and leaves no record
  here (dog-family-measurement note, section 1). 狗's recall to a Codex
  seat is measured like any other; what Codex remembers by itself is not.
- **Claude's index-driven use.** `MEMORY.md` is loaded without an event.
  The seat-could-already-see stratum is the only handle on it.
- **Whether a used memory was right.** D5 catches the cases Joe corrects.
  A wrong memory that Joe does not notice is invisible to all of this.
- **Zai and Kimi on operator turns.** 22 turns in 14 days. Their evidence
  has to come from dispatches (section 6).

## 5. Missing instrumentation, as packets

Not dispatched. Each is one behaviour.

- **A. Report script.** `scripts/dog_helps_report.py`: reads the record
  files, computes D1, D2, D4, D5, D7, D8 with intervals, by arm and by the
  two strata; prints the table and writes it dated under
  `holes/labs/M-repl-circus/`. Includes a hand-check sheet: 30 sampled turns
  with a `disagree`/`redirect`/`withdraw` act, for a reader to mark whether
  each is a correction of the agent.
- **B. Arm assignment.** `dog_provider.clj`: arm from the record id;
  `dog_recall.arm`; the provider returns no segment for a withheld turn.
  Test: a withheld record has items and `arm: "withheld"`, the rendered
  Prompt line has no memory fact, and `dog_use` is still written.
- **C. Stratum stamps on each recalled item.** At recall time: the
  memory's project, its `type` from frontmatter, whether the answering
  seat's own memory index lists it, and the memory's age in days. These
  cannot be rebuilt later, because memories are edited and seats move.
- **D. Record what bury suppresses.** `bury-candidates` drops a ruling when
  `known-memories` finds it already in the store (dog_provider.clj, the
  `remove` at the end of `bury-candidates`). Write those to
  `dog_buried.said-again` as `{act, memory-ids}`. This is D3's source.
- **E. Use check on Codex-answered turns.** The family note reports
  `dog_use` never written for the four Codex turns of 11-11 because the
  check was not yet live. Confirm on a turn after 21:50Z that it now is.
- **F. Paired dispatch arm for Zai and Kimi** (section 6): per-job
  assignment of minus40 injection, recorded on the job.

A, B and D are enough to start. C should land in the same week, since
every turn recorded without it loses its stratum for good.

## 6. Relation to the whitepaper's implanted-memory question

E-memory-whitepaper-v3-programme asks whether a memory channel changes what
a seat achieves on a problem: the same packet to two arms, `:push` against
`:none`, scored on closure, with the rule that a difference in the
recall-hit stratum has to exceed the difference in the recall-empty
stratum (Phase 2b, prediction P7). 狗 asks the same thing about operator
turns. Three things can be shared.

1. **The falsifier.** Both use the empty stratum as the noise floor. The
   report script should print the 狗 result in the P7 form: hit-stratum arm
   difference beside empty-stratum arm difference.
2. **The standard for "used".** The whitepaper counts a use only when the
   artifact shows it (the a94A09 fingerprint), never the seat's say-so.
   `dog_use`'s echo rule is weaker: shared bigrams in the reply text. The
   withheld arm is what makes the weaker rule usable, by measuring how
   often it fires with no cause. For dispatches that produce a commit, the
   fingerprint standard applies unchanged and should be preferred.
3. **The kinds.** The whitepaper's table separates regulative memories
   (how to work) from substitutive ones (where a fact or lemma lives) and
   predicted that regulative ones would account for most witnessed use.
   What 狗 buries is Joe's rulings, which are regulative. Stamping the kind
   on each recalled item (packet C) lets D1 and D2 be read against that
   prediction.

Where they differ: 狗 runs only on operator turns, so it cannot answer
"does a Zai seat solve more with memories". The existing switch for that is
minus40's injection before round 1 (`inject.py`, per seat through
`inject-enabled.txt`; currently zai-1, zai-2, zai-5, kimi-1). Packet F moves
that switch from a seat list to a per-job assignment recorded on the job, so
that the same seat does injected and uninjected work in the same week. The
outcome there is the whitepaper's: the job's result on review, tests or
`lake build`, rounds and tokens to completion. Volume is the constraint the
whitepaper already met: batches of ten paired problems, read as a
trajectory, not as a significance test.

So the two lines report one shared table (arm difference in the hit
stratum, arm difference in the empty stratum, use under a stated standard,
by memory kind) over two kinds of unit: operator turns for 狗, dispatched
jobs for injection.

## Sources read

- `src/futon3c/agency/dog_provider.clj` at `bca608795` (recall, use-check,
  bury-candidates, known-memories, prepare-bury!).
- `holes/labs/M-repl-circus/dog-family-measurement-2026-11-11.md` and
  `REVIEW-dog-and-minus40-2026-11-11.md`.
- `holes/excursions/E-memory-whitepaper-v3-programme.md`. The v2 programme
  and the plan documents were not read for this.
- Turn records under `~/.emacs-graph/session-turn-analysis` (counts above);
  one record read in full (`turn-WHNTlI`).
