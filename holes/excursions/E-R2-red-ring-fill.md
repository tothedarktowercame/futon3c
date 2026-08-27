# E-R2-red-ring-fill — the operator's turns, and the one missing edge

**Opened:** 2026-08-26 · claude-13 at Joe's direction, *"which I think would round
out the collection"*. Excursion from `futon2/holes/missions/M-formal-war-machine.md`.

**It does round it out, and in a specific way: R2 is R14's mirror.** R14 —
a quantity is computed, recorded, and cannot reach the **action**. R2 — a
quantity is computed, recorded, and cannot reach the **belief**. Same defect at
opposite ends. **The loop is open at both ends.**

## Status at 2026-08-26

| | state |
|---|---|
| the ring's claim | **CONFIRMED** — and for the first time today, checking it strengthened rather than falsified it |
| the ring's citation | **WRONG FILE**, harmlessly — corrected below |
| the gap | **one edge**, not a corpus and not a mechanism |
| salience | **already filled** — 香, dated, with a measured cost. Unlike R14 this does not stall |

## What checking found

R2's pattern and the overlay both cite
`futon3c/src/futon3c/aif/observe.clj` — *"ten named channels; all ten read
mission state and none reads an operator turn"*. That is **true of that file**:
ten channels, `:phase-progress` … `:days-since-last-activity`, and the nearest to
the operator measures his **absence**.

**But it is not the War Machine's observation vector.** That file is dated
2026-03-20 and is required by exactly one namespace,
`futon3c/src/futon3c/aif/mission_head.clj`. The WM's vector is
**`futon2/src/futon2/aif/observation.clj`** — *"The war machine's observation
channels, harmonized from all vocabularies"* — and it carries **fourteen**:

    loop-health · support-coverage · attack-coverage · mission-health ·
    stack-pct · consulting-pct · portfolio-pct · mathematics-pct ·
    active-repo-ratio · sorry-count-norm · coupling-density ·
    ticks-firing-ratio · depositing-signal · annotation-health

Sources: holistic argument, peripheral-aif, logic model / joe-hud, JSDQ, sorry
topology, temporal analysis, daily scan frames, `stack-annotations.edn`.

**None of the fourteen reads an operator turn either.** So the ring's claim
survives the correction and is in fact broader than stated: **two independent
observation vectors, twenty-four channels between them, zero operator-turn
channels.** *(`aif-r1-r16-pattern-map.md` records R2 as "✓ 13 harmonized
channels. Real." — the count is now 14, `:annotation-health` added at v0.10, and
the ✓ is about schema quality, not operator coverage. The same two-axis
divergence as R6 and R8.)*

## The gap is one edge, and both of its ends already exist

**The turns exist and are queryable.** `futon1b :7073 /api/alpha/evidence`
carries operator turns and context-retrieval events.

**The turn→pattern association exists, is computed live, and is in the paper.**
`p4ng/empirics-futon/gen_turn_chain.py` performs the join and emits both the
figure and its numbers — *"so the figure and the counts in the prose can never
drift apart (WR-8: no number in this paper is typed by hand)"*. Current counts
(`turn-chain-counts.tex`): **27 turns, 27 turn→pattern edges, 24 patterns, 2
missions, 323 curated, 11 agree / 16 new.**

So R2's `THEN` — *"the turn→pattern association will provide enriched material"* —
is **already built**. What is missing is not the corpus, not the association, and
not the search: it is **a channel in `observation.clj` that reads it.** One edge,
between two things that both run today.

That materially changes R2's own `NEXT-STEPS` (*"since we have full text search
via an XTDB sidecar, this can now be piloted"*). The pilot does not need to build
the association. It needs to wire one that exists.

## The measured cost, which R2 has and the others do not

From the pattern's salience, and it is the best-evidenced redness of the five:

> the operator lane classifies every item into nag/brief/silent and on the live
> bulletin read **nag 0 / brief 60 / silent 51 of 111** — the strongest
> representation the loop has of the operator fired **zero times in a hundred and
> eleven opportunities**. The per-class credit collapsed to **0.009**, roughly a
> sixteenth of where an action starts, because a model with no representation of
> the operator's alternatives cannot tell *"he considered this and declined"*
> from *"he was doing something else"* and records both as non-follow-through.
> **That collapse is not a measurement of the action class; it is the shape of a
> missing variable.**

Two things to check before this is cited further — the same discipline that
corrected the observe.clj citation: **which bulletin, on what date, and is 0.009
the current credit or the value at the time of writing?**

## The requirement, in the family vocabulary

**Family 2 (non-empty handle), at the perception stratum, with R14's clause
attached.**

> A channel that is never read is not a channel. An operator turn must enter the
> observation vector as content, and must be shown to change an inference result.

R2's pattern already carries the second half — `?evidence(required)`: *"A pilot
has not yet shown that processed operator turns enter the inference vector **and
change an inference result**."* **That is the R14 lesson, pre-recorded.** R14 had
to discover the hard way that entering the record is not enough; R2's author
wrote the behavioural clause into the requirement from the start.

**❌ The naive fix: add an `:operator-turn-count` channel.** It recreates the
defect exactly — it is `:days-since-last-activity` again, a measure of the
operator's *presence*, not of what he said. And by R14's argument a scalar that
nothing downstream is sensitive to changes no inference. Counting the turns is
not reading them.

**✅ The requirement-satisfying fix:** the channel carries the **association's
content** — the turn→pattern edge, which is what makes a turn machine-legible —
and the pilot's acceptance is a *changed inference*, demonstrated, not a
populated field.

**Acceptance.** One tick where the observation vector differs with and without
the operator channel, and the belief or ranking differs as a result. Anything
less is `record_sensitivity_is_not_governance` at the other end of the loop.

## Why this one straddles the blanket

The pattern says it, and it is worth carrying into the formalisation: *"Unlike
the other four rings this one straddles the blanket: the turns exist, are
persisted, and are the one channel the apparatus cannot fabricate."*

R8, R14, R6 are all defects the machine could in principle repair alone. R2 is
the only ring whose content originates **outside** the Markov blanket. That makes
it the load-bearing ring for *"closing the AIF over the operator"* — and the
`NOTE-thirtyfour-steps-both-levels.md` finding applies: the operator half has the
feedback the WM lacks, and R2 is the edge that would carry it.

## Slices

1. **Verify the cost figures** — which bulletin, what date, is 0.009 current.
   *(cheap; do first, per `T-wm-wrong-corpus-26082026`)*
2. **The one edge** — what a `:turn-pattern-*` channel would carry, and whether
   `gen_turn_chain.py`'s join is the right shape to normalise into `[0,1]`.
3. **The pilot, with R14's acceptance bar** — a demonstrated inference change,
   not a populated field.
4. **Formalisation** — family 2 at the perception stratum; likely shares
   `inhabitedHandle` and `typedAbsence` with `GainChain.lean` rather than needing
   new vocabulary. Confirm before writing a module.

## Related

- `E-R14-red-ring-fill.md` — the mirror; a quantity that cannot reach the action.
- `E-R6-red-ring-fill.md` — the SELECT-stratum sibling.
- `futon2/src/futon2/aif/observation.clj` — the WM's fourteen channels.
- `futon3c/src/futon3c/aif/observe.clj` — the ten the ring cites; a different vector.
- `p4ng/empirics-futon/gen_turn_chain.py` + `turn-chain-counts.tex` — the association, built.
- `p4ng/empirics-futon/NOTE-thirtyfour-steps-both-levels.md` — why this ring carries the join.
