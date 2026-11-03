# Mission: M-ways-of-working — declared ways of working, each with a probe that can fail

**Date:** 2026-11-03
**Status:** OPEN — INSTANTIATE in progress (2 of 11 probes built 2026-11-03)
**Owner:** claude-5 (Joe opened it 2026-11-03; took over from claude-16's handoff)
**Repo:** futon3c (`holes/excursions/ways-of-working.json`,
`scripts/ways-of-working-probe.py`, `scripts/publish-cascade-snapshot.sh`),
with patterns in `futon3/library/ways-of-working/` and
`futon3/library/creativity-commons/`.
**Excursions:** E-autoclock-wow
**Published at:** https://zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html
(Declared Ways of Working panel; published by hand while the
`wip-cascade-snapshot` timer is parked).

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

## State of the ways (2026-11-03 22:34 publish)

| Way | Probe | State | Notes |
|---|---|---|---|
| operator-turns-logged | `operator-turns` | on, holding | 66/66 turns matched over 24 h (codex-7 `2493f725a`) |
| feedback-reaches-every-participant | `feedback-reaches` | on, failing | 55/247 bells with no return path, one emitter; 3 unproved returns; marks count partial (codex-15 `f391eca2c`, review `965e220fa`) |
| stays-alive-and-paid | `zone-health` | on, failing | evidence backup now nightly (futon0 `fa1618c`); bills and runway by hand |
| one-queryable-self-account | `cascade-standards` | on, failing | s6 readability not built |
| backlog-understood | `draw-pile` | on, failing | |
| inbox-zero | `git-clean` | off, failing | timer parked |
| records-carry-warrant | none | not built | unit is the test run, not the commit (Joe 11-03; declaration corrected); finding 5 blocks enumeration |
| intents-are-tracked | none | not built | minus40 open asks are a candidate source |
| selection-gives-a-reason | none | not built | War Machine side; futon2 rules apply |
| eat-your-own-tail | none | not built | needs a design first |
| real-users | none | unprobed by design | values live in futon7 |

## Open findings, each to become its own item

1. **The consent-gate sweeper sends without `--from`:** 55 bells in 24 h with
   no return path, all from `[sweeper/consent-gate-emit]` (futon2). It
   should send `--from`, or type its bells as notifications.
2. **The `wm-full-loop` inbox is never drained:** 507 files in
   `~/.claude/agency-inbox/wm-full-loop/` since 10-01. "Delivered" to an
   inbox is not receipt unless something reads it.
3. **The reply census misses most replies:** 26 replies joined against 66
   operator turns, and 1 of about 9 of claude-5's. The suspected gap is
   replies after tool use; unconfirmed.
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
4. Turn the publish timer back on when the probe run fits its budget (about
   2.5 min today).

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
