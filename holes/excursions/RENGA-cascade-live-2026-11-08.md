# Cascade Live as a flexiarg renga (first pass, 2026-11-08)

DRAFT by claude-6 at Joe's direction (2026-11-08): "let's try and make some
sense of this in flexiarg form … start with a flexiarg-renga style chain, and
we can see where its topology breaks down."

Rule of the chain, as in renga (Winterstein & Corneli 2016, "Writing rengas
with web services"): each link answers only the link before it. Its IF takes
up the previous THEN; its HOWEVER is the shift. Where no honest IF can be
written from the previous THEN, the chain breaks, and the break is recorded
rather than smoothed over. Each link names the existing pattern or way of
working it stands on, and that way's probe state today (Cascade Live §3,
probed 2026-11-08).

---

## 1. Opening verse (the crown)

```
@title Close The Loop Over The People In It
+ IF:       People do open-ended work alongside machines (software, mathematics).
+ HOWEVER:  Help that is rented and opaque gets work done without the people
            getting better at it, and nobody can say why it chose what it did.
+ THEN:     Close a considerably faithful active-inference loop over the people
            in it: their turns are its observations, its choices come back
            explained, and their capability is what it is judged on.
+ BECAUSE:  p(bloom) is people made more capable, not work made cheaper; a
            free-energy-minimising loop is both frugal (complexity is a cost)
            and explainable (G separates into risk and ambiguity).
```
Stands on: apex-thesis.json; aif/niche-construction; aif/grounded-actuation-not-reobservation. Joe 2026-11-08 (bet, tutor, p(bloom)).

## 2. Turns become observations

```
+ IF:       The loop takes the people's turns as its observations.
+ HOWEVER:  Turns are spoken and then gone; what was said cannot update
            anything that never received it.
+ THEN:     Log every operator turn, with its intent marked, where the loop
            reads from.
+ BECAUSE:  An unrecorded ask is not an observation.
```
Stands on: way operator-turns-logged (**holding**); reply proforma (象 marks); problems/operator-turns-become-inference-observations.

## 3. What was asked stays open

```
+ IF:       Turns are recorded with their intent.
+ HOWEVER:  A record of what was asked is not a record of what was done about
            it; asks sink into the log.
+ THEN:     Keep each intent open until it is met or withdrawn, linked to
            what met it.
+ BECAUSE:  A loop that loses the ask cannot be judged against it.
```
Stands on: way intents-are-tracked (**failing**); minus40 "open asks".

## 4. The asks point into a backlog

```
+ IF:       Intents are tracked against the work that meets them.
+ HOWEVER:  The work is a pile of ~800 items nobody has read, in four
            vocabularies, much of it stale or done.
+ THEN:     Read the pile into one queryable self-account: every item placed,
            stale items closed, waits on the operator typed as such.
+ BECAUSE:  You cannot steer by an account you cannot query.
```
Stands on: workshop/one-queryable-self-account; ways backlog-understood (**failing**), status-is-known (**holding**); Giraffe; Plane view.

**Fork here.** In mathematics the pile is a problem's blueprint (lemmas,
routes, blocked-on notes), not a backlog. The same verse holds with
"blueprint" for "pile"; from here the chain runs twice (see break B).

## 5. Choose from the account

```
+ IF:       The work is one queryable account.
+ HOWEVER:  Reading it in shape (what blocks what) ranks by file position and
            tie-breaks; it says nothing about worth.
+ THEN:     Choose the next action by expected free energy over policies, with
            preferences learned from the operator's rulings.
+ BECAUSE:  A choice is accountable when it can say why this and not that, in
            terms the operator set.
```
Stands on: workshop/accountable-next-action-choice; cascades/select-tasks-from-cascade-live; way selection-gives-a-reason (**failing**).

**Break A, here.** "Preferences learned from the operator's rulings" has no
mechanism today: rulings live in memory files and notes, and nothing carries
them into C. Verse 5 asserts a link verse 3 does not supply. The 38-item
audit (2026-11-08) is the symptom: shape ranked two unsuitable items first.

## 6. The outcome carries its warrant

```
+ IF:       A choice is made and acted on.
+ HOWEVER:  The report of what happened can be made up, trimmed, or true only
            of a copy of the machine.
+ THEN:     Every relied-on outcome carries the record it came from.
+ BECAUSE:  A loop that learns from unwarranted outcomes learns its own story.
```
Stands on: workshop/records-carry-warrant; aif/no-self-certification; way records-carry-warrant (**failing**; its invariant covers test runs only, and 1 of 74 wing-A items builds it).

## 7. Capability, not only output

```
+ IF:       Outcomes are warranted.
+ HOWEVER:  A warranted record of machine work does not show that any person
            got better at anything.
+ THEN:     Record the moves and rulings a person reuses without being told
            (a proof move carried to the next problem; a ruling the next
            choice follows unprompted).
+ BECAUSE:  The opening verse judges the loop on capability.
```
Stands on: holes/labs/tutor/lessons (a96A07, t98J02: "find a quantity locally constant off the curve and bound its jump"); draft D3 (a page is read when a decision cites it). No way of working covers this yet.

**Break C, here.** Verse 7's THEN feeds verse 1's IF: the chain closes into
a cycle. The structure is a loop with branches, not a tree with a single
crown; the "crown" is where one chooses to start reading the loop.

---

## Where the topology breaks

- **A. Rulings → preferences (between 3 and 5).** The chain needs the
  operator's rulings to become the loop's preferences; nothing does that.
  This is the gap in "closing the War Machine over the operator".
- **B. Fork at 4 (software / mathematics).** One chain, two domains: the
  tutor and Cascade Live run the same verses 4–7 over different piles. Either
  two strands that rejoin at 7, or one chain with a typed parameter.
- **C. Cycle at 7 → 1.** Renga does not close; this chain does. A cascade
  read top-down hides that the loop is the object.
- **D. Viability will not link.** No verse's THEN gives "machines and money
  must keep running" as its IF: stays-alive-and-paid is a precondition of
  every verse, not a step in the chain (draft D2: a foundation is cited by
  every wing, not placed beside them).
- **E. Wing C (feedback reaches every participant) appears nowhere.** Verses
  2–3 are about the operator only; other participants (agents, learners,
  Murch, Peeragogy) never enter. Either a strand is missing or C belongs in
  verse 1 ("the people in it", plural, is asserted and never followed).
