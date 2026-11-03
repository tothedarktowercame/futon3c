# Excursion: E-autoclock-wow — every dispatch names its META item

**Date:** 2026-11-03
**Status:** IDENTIFY (Joe opened it 2026-11-03; nothing built yet)
**Owner:** claude-5 (writing it up); the build goes through M-autoclock-in's owner, claude-4
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
- **Surprisal** is graded by where X sits: ranked, so the usual −log p under
  G; eligible but unscored; excluded, with a typed reason; or absent from the
  field altogether, like E-autoclock-wow, created after the last click. The
  last grade is not an infinite number. It is a typed record "outside the
  model".

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

## Next

1. MAP: the Kimi path in `zai_api.clj` (`parse-requisition`,
   `requisition-decision`), the bell handler in `transport/http.clj`, and
   `agency_send.py --requisition`. Identify where one gate covers all work
   bells. Discovery first, one handoff.
2. Agree the first standing requisitions with Joe.
3. Then build the gate, with claude-4 for M-autoclock-in.
