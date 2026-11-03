# Excursion: E-autoclock-wow — every dispatch names its META item

**Date:** 2026-11-03
**Status:** MAP done 2026-11-03; gate design drafted, nothing built
**Owner:** claude-5 (writing it up); the build goes through M-autoclock-in's owner, claude-4
**Part of:** M-ways-of-working (Joe, 2026-11-03).
**Spawned from:** M-autoclock-in §INSTANTIATE-7a and §INSTANTIATE-8; the
`feedback-reaches-every-participant` way of working
(`holes/excursions/ways-of-working.json`), whose probe landed today
(`f391eca2c`, `965e220fa`).
**Repo:** futon3c (Agency bell handler, `agents/zai_api.clj` requisition
parsing, `scripts/agency_send.py`), with the Way of Working declared in
`holes/excursions/ways-of-working.json`.

## HEAD

Joe, 2026-11-03: "it says 'no mission' here and you could be blocked to
dispatch until we agree what you're clocked on."

**Lift** the Kimi requisition rule (M-autoclock-in §INSTANTIATE-8) from Kimi
seats to every work request. **Turn off the workaround** of minting a fresh
excursion per requisition. Add **A-** (algorithms) to the allowed targets, so a
requisition names an **M-, E-, T- or A-** item: the same four kinds as the
Giraffe work pool (M-giraffe HEAD: "786 M/E/T/A items").

Then every session has a trail back to a META item. Whatever already sorts,
selects and checks those items (Giraffe, G, flight, VERIFY checklists) applies
to a dispatch from its first turn. Whatever quality control an item carries
can be pushed back to the seats working on it.

## What is wrong now (measured 2026-11-03)

1. **Unclocked means anything.** §INSTANTIATE-7a: "Unclocked callers supply
   no inheritance." The recipient falls back to its own session clock. On
   2026-11-03, claude-5 (clock: no mission) dispatched six Ways of Working jobs.
   Its recipients codex-7 and codex-15 were both clocked to
   `M-futonzero-prelim-practice` from earlier work, so the jobs were filed
   there.
2. **A requisition only has to resolve.** The Kimi rule checks that
   `holes/**/<name>.md` exists. An excursion created for the dispatch
   satisfies it, so the result is filed under an item nobody else selects
   from or verifies. That is the workaround this excursion turns off.
3. **Returns can land where nobody reads them.** The feedback probe's first
   live run (2026-11-03) found 3 `wm-full-loop` returns "delivered" to
   `~/.claude/agency-inbox/wm-full-loop/`, which holds 507 files back to
   10-01 and is not drained. It also found 55 of 247 bells sent with no
   return path, all from one emitter (the futon2 consent-gate sweeper).

## The Way of Working

**Invariant:** every work request carries a requisition naming an M-, E-, T- or
A- item that the operator has agreed to. A request without one is refused
before a job exists. The refusal reaches the operator as a question.

- **Requisition line**, as now: `Requisition: <M-*|E-*|T-*|A-*> — <purpose>`.
  It sits in the prompt, so every transport carries it, and the caller
  restates its target on every call (§INSTANTIATE-8 point 4: why this beat
  inferring the target, a payload field, or the caller's clock).
- **Agreed** means one of: the operator clocked the caller onto the item; the
  operator named it in a turn; or the caller's clock came from one of those.
  An agent-minted item with no operator turn behind it does not count.
- **Refusal is a question, not an error.** "claude-5 is on 'no mission' and
  wants to dispatch for X: clock on what?" The operator's answer clocks the
  session, records the agreement, and releases the request.
- **Inheritance.** A recipient takes its clock from the requisition, never
  from its own stale session. This replaces §INSTANTIATE-7a's "unclocked
  callers supply no inheritance": an unclocked caller can no longer dispatch.
- **Notifications are exempt only if typed as such.** A brief-mode bell that
  asks for no work needs no requisition, but cannot carry a request either.

## Feedback: up, down and across a dispatch stream

The requisition is the address that feedback travels to. Today feedback comes
back one way, as the seat's bell-back to its dispatcher ("codex-12 stopped
where the claims could not hold"). Once every dispatch names its item,
feedback can travel in three directions:

- **Down, from the item to the seats.** A mission's VERIFY section, a ticket's
  acceptance line, or an algorithm's checks are requirements on whoever works
  under that item. They can be attached to the dispatch automatically instead
  of being retyped into each packet. Author ≠ reviewer is one of these
  requirements, and today it is enforced by habit.
- **Up, from the seats to the item.** A seat's finding ("the claim cannot
  hold") lands on the item, where Giraffe and G can read it, not only in the
  dispatcher's buffer. The item's history then records what each dispatch
  found.
- **Across, between seats on the same item.** Two seats working the same item
  can see each other's findings, because both are filed under one address.
  Today they meet only if the dispatcher relays.

## An unranked item jumping the queue is surprisal (Joe, 2026-11-03)

Joe: "E-autoclock-wow is jumping the queue relative to any analysis from
Giraffe. That's fine and appropriate to how it should be, but this is another
form of 'feedback' ... a form of surprisal for the system and should be
accounted for appropriately."

The requisition makes this measurable. Each dispatch names an item, so the
system can compare the item worked on with what selection would have
proposed:

- **The observation:** "work began on item X, agreed by the operator".
- **The prediction:** the latest click's field (M-giraffe HEAD: 786 M/E/T/A
  items, 109 eligible) and G's ranking over it.
- **Surprisal** is graded by where X sits in the graph, not only in the
  ranking:
  1. ranked: the usual −log p under G;
  2. eligible but unscored;
  3. excluded, with a typed reason;
  4. **new, but linked to an item in the field: a latent adjacent possible.**
     This is E-autoclock-wow. Joe asked for it "linked from M-autoclock-in",
     so relative to the previous state it sits one edge from a known node.
     Its surprisal is the parent's position plus the cost of opening that
     edge, not an unbounded jump.
     (Joe, 2026-11-03: "'outside the model' is only weakly true in this case
     b/c I said 'linked from M-autoclock-in' ... something more like a 'latent
     adjacent possible'.")
  5. new and unlinked: fully outside the model, a typed record, not an
     infinite number.

  The link is operator evidence too. "Linked from M-autoclock-in" in the turn
  is an edge with the operator as source, and ingest should record it with
  the item. Grade 4 then tells Giraffe where the field was under-explored,
  next to M-autoclock-in, which is more useful than "something was missing".

What to do with it, without overriding the operator:

1. **Ingest.** The item enters the pool with its provenance ("operator,
   2026-11-03, by turn"), so the next click can see it. Joe 10-18: a selection
   tie means missing ingest. An operator pick from outside the field is the
   same kind of miss.
2. **Record the order.** The jump is an application-order edge whose evidence
   is the operator, like "APM after prelims" (M-giraffe §Overlap: "Joe's
   APM-after-prelims is the first such edge, evidence = operator").
3. **Learn from the residual.** Over time, operator picks outside the field
   say what the pool lacks. That is a signal for Giraffe's ingest, and for
   G's prior, not a fault in the operator.

Probe count 5: requisitions in the last 24 hours whose target was outside the
last click's field, graded as above. This is not a violation; it is the
surprisal the system should account for. Its failure condition is a jump
that never gets ingested: an item still outside the field after the next
click.

## Patterns (futon3/library/creativity-commons)

The creativity-commons patterns are relevant, read with dispatches as
contributions and META items as places:

- `contributor-boundaries`: who may add to an item is defined and recorded
  where a test can read it. Here the requisition is that record.
- `nested-places`: contribution, testing and enforcement sit in nested
  layers, here campaign › mission › excursion › ticket, with A- items as
  shared method.
- `distributed-monitoring`: the producers monitor each other. Here the
  feedback probe and the reviewer seat do the monitoring, and the requisition
  is what joins them to the same item.
- `producers-can-modify-the-tests` and
  `affected-participants-change-the-rules`: a seat that finds a VERIFY item
  wrong can propose a change to it. This needs the up direction above.
- `formative-sanctions`: graduated responses, e.g. an unagreed requisition
  becomes a question first and a refusal only after that.
- `low-cost-local-conflict-arenas`: a disagreement between author and
  reviewer is settled on the item, with the operator as the higher authority.

## Probe

This extends `feedback-reaches-every-participant`; it is not an eleventh way.
Counts over the last 24 hours:

1. work requests with no requisition (should be refused, so after the build
   any count above zero means the gate is being bypassed);
2. requisitions whose target has no operator turn or operator clock behind it
   (catches the workaround);
3. refused requests still waiting for the operator's answer (pending, not a
   violation);
4. recipient jobs whose clock differs from their requisition.

Each count needs a constructed bad case in its test, as in
`scripts/test_ways_of_working_feedback.py`.

## Observed: mention-autoclock, 2026-11-03

Joe: "I think we should autoclock if I just mention E-autoclock-wow." It does,
when the mention can be resolved. The clock decisions for claude-5's session
`dd552609` show:

- turn 13 (Joe: "let's create a new excursion linked from M-autoclock-in called
  E-autoclock-wow"): `unresolvable-target`, unclocked. Two targets were named,
  and the excursion did not exist yet.
- 22:34:51, activity phase after the file was written: `ambiguous` between
  M-autoclock-in and E-autoclock-wow, unclocked.
- turn 14 (Joe names only E-autoclock-wow): `clocked`, `excursion-id
  E-autoclock-wow`, source 1 (operator).

Gap: when the operator names an item *to be created*, the agreement is
already there, but the decision is lost because the target does not resolve
yet. Proposal: keep an operator-named, unresolved target as a pending
agreement on the session, and promote it when the file appears. Then the
refusal question in this excursion would not need asking at all.

## Open questions

- Where does "agreed" live as data: the clock-decision store
  (`agency/clock_store.clj`) with `:source :operator`, or a new record?
- Is A- resolved the same way (`holes/**/A-*.md`, today under
  `futon2/holes/labs/`), and should campaigns stay excluded?
- Which machine emitters (consent-gate sweeper, wm-full-loop, work-orders)
  should get standing requisitions, and who agrees to those?
- Can the down direction reuse Giraffe's locators (M-giraffe: "how the want
  is checked") rather than re-reading VERIFY prose?

## Unwatched runs compact themselves (Joe, 2026-11-03)

Joe: "since we don't want *usage* to explode in the process ... if an agent
runs more than 5 turns without an operator turn, we switch on autocompact at a
reasonable level. That will prevent Claude Opus and Claude Fable sessions
drifting to 1m tokens in long running sessions."

This feeds the same picture. Operator turns are the feedback that keeps a
session anchored. A run with no operator turns is a dispatch stream with no
feedback from the top, so its context should not grow without limit.

**Today** (`src/futon3c/agency/agent_pouch.clj:285-363`): before every
Claude turn, `dev/futon3c/dev.clj:3974` calls `joey-eligible-or-compact?`.
That function runs Claude Code's own `/compact` (warm in the pouch, or cold)
once the session passes `joey-max-tokens` (200k,
`FUTON3C_KANGAROO_JOEY_MAX_TOKENS`). It does so only for seats whose roster
metadata says `auto-compact? true`, a static flag for "a seat nobody
watches". Every other seat waits for its operator's C-c C-z.

**Change:** "nobody watches" is decided by behaviour, not by a flag. Per
(agent, session), count consecutive turns whose `CURRENT TURN` header is not
`Origin: operator`: bells, parked-resumes, work orders, auto-bellbacks. Reset
the count on an operator turn. When the count exceeds 5, the seat is treated
as `auto-compact?` for that session, at the existing 200k threshold, until
the next operator turn. The static flag still forces it on. Each switch is
logged once per run, `[kangaroo] <agent> <n> turns without an operator turn
— auto-compact on`, so the operator can see why a session was compacted.

## Autorunner on the Cooked line (Joe, 2026-11-03)

Joe: "instead of Compaction recommended (C-c C-z) we get a visual reminder that
the agent is running as an autorunner until its dispatch/bellback pattern
stops. That's a little subtle because knowing that a dispatch pattern *has*
stopped requires a continuation, i.e., an autorunner that returns to manual
control should go back to the usual Cooked line style."

The JVM's counter is the source of truth. `GET /api/alpha/agents/:id/pouch`
takes `?session=` and returns `:autorunner` and `:turns-without-operator`.
While the count is above 5, the Cooked line shows `→ autorunner (N turns
without you; auto-compact on)` in place of the compaction hint.

Stopping is not detected by a timer. The last line of an autorun keeps saying
autorunner while it sits at the prompt, which is true: a park may still wake
it. The operator's next turn is the continuation. It resets the count, and the
Cooked line after it is back to normal.

## MAP (2026-11-03, codex-13; reviewed by claude-5)

Report: `~/notes/packets/ways-of-working/autoclock-wow-map-REPORT.md`. It was
the first requisitioned dispatch: codex-13's job was clocked `inherited` to
`E-autoclock-wow` from claude-5's operator-sourced decision
(`clock-decision-6eda1152…`).

- **One seam.** Every durable job goes through `create-invoke-job!`
  (`transport/http.clj:2277-2311`), which already reads the caller's clock
  decision to snapshot inheritance, before `create-invoke-job-ledger!` appends
  the job. Bell, `/invoke`, both whistle paths, announced invoke (the gate runs
  at announcement, not activation), auto-bellback, work-order nudges, headless
  park resumes and social-mesh dispatch all reach it. Buffer park resumes are
  not jobs and need no gate.
- **Reusable from Kimi:** `parse-requisition` and the admission shape in
  `requisition-decision` (`agents/zai_api.clj:1333-1468`). The resolver regex
  is `[MET]` (`:1330`), so A- needs adding, and it checks existence, not
  agreement.
- **Agreement is already data.** Clock-decision `source 1` is an explicit
  operator target or mention (`agency/clock_decision.clj:253-327`), and the
  live projection keeps the latest decision in memory
  (`agency/clock_store.clj:236-261`). Inherited agreement follows
  `:caller-decision-id`. That is one point read, or none if the snapshot
  carries the origin's source.
- **Machine traffic, last ~6 days:** `auto-bellback` 471 (17 work),
  `http-caller` 326 (all brief), `wm-full-loop` 179 (113 work), `work-orders`
  43 (all brief), plus `ma-reading` 805, `turn-capture` 495 (all work),
  `lu-rerank` 493 and `ma-occurrence` 467. Caller names don't separate work
  from notification. The ledger's validated `mode` does.

**The workaround is bigger than the report counted.** codex-13 found 16
October files (`E-kimi-task-171`…`186`). There are **260**
`futon2/holes/excursions/E-kimi-task-*.md`: 150 added in September, 36 in
October and 74 in the first three days of November. They are minted by
`futon3c/scripts/kimi-task.sh`. That script's header says the purpose is
"clock a Kimi seat in on a UNIQUE task, so its conversation starts fresh"
(Joe, 2026-09-25). The need it serves is real. Kimi's context carries over
until the target changes, so minting a target was the only way to force a
fresh start. Spot-checked: `E-kimi-task-181` (`ea1304559`) and `-186`
(`914d0ab95`) are both "prove lemma apm_t91J03_degree … one leaf", one per
seat. Their real item is the APM problem, not a new excursion.

### Gate design (draft)

A pure `admit-request` in `create-invoke-job!`, after the clock snapshot and
before the ledger append. Its inputs are the parsed requisition, the
validated mode, the continuation type, caller, surface and the caller's
clock decision. It returns one of:

1. **admit work** with the resolved target, when the requisition names an
   M/E/T/A item and agreement holds: the caller's decision is source 1 on it,
   or inherited from one that is, or the target lies under the agreed item
   (decided below);
2. **admit continuation**: auto-bellback or parked-resume that answers an
   admitted job (joined by job id, not caller name);
3. **admit notification**: `brief` mode with no work payload;
4. **refuse**: no job id is allocated. The refusal goes to the operator as
   the question "<caller> is on <clock> and wants to dispatch for <target>:
   clock on what?". Machine callers get a typed error.

**Fresh context keeps its own item, linked to an agreed parent** (Joe,
2026-11-03: "we could still require it be linked back to an existing or at
least acknowledged META item ... E-autoclock-wow → T-kimi-task-181 — though
it would be better if the tickets had semantic names not just numbers").
Minting a new item per task stays, because a target change is what gives a
Kimi seat a fresh conversation. Two things change:

- **The minted item is a ticket under a parent.** Its requisition is written
  as a chain: `Requisition: E-autoclock-wow → T-apm-t91J03-degree-leaf —
  prove one leaf`. The gate checks agreement on the **parent**. The child only
  has to exist and to name that parent, in a `**Parent:**` line. This also
  settles the open question "does a ticket under an agreed item count?": it
  does, when the link is declared on the ticket and the parent is agreed.
  It is grade 4 of the surprisal list, a latent adjacent possible, by
  construction.
- **Semantic names, not counters.** `kimi-task.sh` mints
  `T-<slug of purpose>` (for example `T-apm-t91J03-degree-leaf`), with a
  short suffix only on collision, and files it under the parent's repo
  `holes/tickets/`. That follows the 685 existing `T-*.md`, which already
  have names like `T-arxana-direct-reads`. The 260 `E-kimi-task-N` files
  stay as history; they are not renamed.

Fresh context then needs no new flag: `context-carry-decision` already
resets on a target change, and every minted ticket is a new target.

**Rollout:** first log only (the five probe counts, no refusals); then refuse
`work` jobs from agent callers; then give each machine caller a standing
requisition that Joe agrees once, recorded as a source-1 decision.

Open for DERIVE:
- ~~Does "under the agreed item" hold?~~ Yes, when the ticket declares its
  parent and the parent is agreed (Joe, 2026-11-03, above).
- ~~Is one level of chain enough?~~ Chains are allowed (Joe, 2026-11-03,
  "in an homage to William Burroughs, we can allow tickets to explode"). The
  gate walks `**Parent:**` links up until it reaches an agreed item. A cycle,
  or a chain that never reaches one, is refused.
- Should `turn-capture`'s 495 work jobs really be work?

## Next

1. ~~MAP~~ done, above.
2. Agree the first standing requisitions with Joe.
3. Then build the gate, with claude-4 for M-autoclock-in.
