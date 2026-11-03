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
| records-carry-warrant | none | not built | next in build order |
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

1. Records-carry-warrant: discovery handoff, then build, as for the first two.
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
