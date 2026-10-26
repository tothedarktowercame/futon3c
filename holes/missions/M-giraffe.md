# M-giraffe — start from the data structure: a work pool the War Machine can read

**Date:** 2026-10-25
**Status:** OPEN — HEAD (raised by Joe in claude-11's REPL)
**Owner:** Joe + claude-11
**Follows:** M-kangaroo (warm pouches, i.e. agents that persist; this mission is about the work those agents are given).
**Serves:** our own War Machine first. Second, the consulting offer: "show me your agent logs and your ticket tracker, and I will show you how to make it work better" (pudding-prover T2.4/T2.5, `warm-customer-pays`; T∞).

## HEAD

"In programming it's good to start with the data structure" (Joe). We have a War Machine that is said to be fully functional, sitting over a pool of work so unstructured that "pool" may give it too much credence. Measured on 2026-10-25 (claude-11's REPL; data under `~/notes/experiments/2026-10-25-wm-wants/` and `~/notes/experiments/2026-10-25-mission-star-audit/`):

- The latest click's field holds 786 M/E/T/A items: 109 eligible, 677 excluded. All 171 tickets are excluded: 102 repair findings with no root (now typed: 92 machine-owned, 10 no-task-selected; futon2 `03fdb29ee`), 30 not on the current map, 26 attached to a root, 13 with a root that is not current.
- G scored 4 of the 109 eligible items. Two tied exactly, and the tie was broken by a random draw. G has no input for dependencies or readiness: a blocked item is excluded before scoring, never rated "good, but not now".
- Wants are computed per mission when it is flown. The 109 eligible items yield 636 wants. 556 of them are checked by whether a checkbox in the doc is ticked (C4). 80 have no stated verdict and so no locator; flight and the autonomous selector treat these differently. Wants are recorded at selection but have not reached the brief after the run (fix 1 in progress).
- 422 mission docs exist. Only 13 had a real commit this month; 306 link to no capability star.

## Question

What should one work item be, as data, so that a War Machine (ours, or one reading a client's tracker) can select from the pool, check its progress, and say why it chose it? Candidate fields, to test against the pool rather than assume:
- the stated want, in the owner's words, with its source;
- how the want is checked (its locator), or a typed reason why it cannot be checked yet;
- dependencies on other items: what must exist first;
- owner and state, with dated transitions, including "dropped";
- the capability or goal it serves;
- provenance: who said so, and when.

## Approach (to argue later)

- Start from our own pool: describe the items we have in the candidate structure, and count what is missing for each field.
- A source adapter per tracker. Our holes/ documents are one source, a Jira export would be another. Do not download Jira yet: the adapter should be easy, and the published public-Jira datasets (e.g. Montgomery et al., MSR 2022; availability and licence not yet checked) can be looked at as a second source.
- The historical datasets have no agent logs. For the part about logs, our own Agency evidence is the only source we have.

## Not yet decided

- Where the structure lives (substrate, files, or both), and whether it replaces or projects the existing M/E/T documents.
- What "the machine is runnable" requires of it first (Joe's invariant, 2026-10-25): no large redesign before a run is demonstrated.

## Giraffe in futonic logic (Joe, 2026-10-26)

Source: `futon3/library/futon-theory/futonic-logic.flexiarg` §4.9–4.11. The essay port in `futon7a/essays/futonic-logic/futonic-logic.md` is identical in these sections.

- 間 (honest interval) reads the gap without filling it. 関 (gate/relation) names the gate. 間 + 関 → 無門関: "the gate dissolves when the interval is honestly read … The tension IS the gate: traversing it (proposing the mission, tracing the path) is how it opens."
- **馬 is the 無門関.** It reads the interval between a tension and the later turns, with quote-checked evidence (no filling), and names the gate: resolved, re-raised (not-fixed or variation), dropped.
- **Giraffe is the traversal.** It takes 馬's honestly read tensions and the mined pattern cascade, and proposes the ticket that opens each gate: an attested want, its check, its traced dependencies. The existing operations it starts from are `query-uncovered-components` (reads 間 relationally) and `trace-tension-path` (traverses 関).
- The loop: 驴 (recall) → 象 (read the operator's turn) → 狗 (act) → 馬 (無門関) → Giraffe (traverse into tickets) → War Machine (select from ready tickets).

## The Jira lens

What a tracker people have used for years would give the gate, if we did this in Jira. This is a map of terms, not a dependency.

| Jira | Here |
|---|---|
| acceptance criteria | a want with a locator |
| "blocks / is blocked by" links | the dependency input G lacks |
| epic → story → sub-task | breakdown into doable tickets |
| resolution "won't do" | 馬's "dropped" |
| Definition of Ready | the gate before selection: criteria present, blockers resolved, small enough |
| Definition of Done | the after-run check (futon2 `368079c98`: wants re-read at the reviewed commit) |

Jira shows whether a ticket can be done; it does not show whether it is worth doing. That comes from the capability stars and from wants Joe has attested.

## Stop-the-line findings for the War Machine (Joe, 2026-10-26)

Each of these is a stop-the-line finding in its own right. They are not data to tidy later.
1. **Eligible missions the War Machine cannot reach.** 101 of 109 eligible items have never been read. The read set is frozen at the previous selector's last choices: the 9 reading records were written between 10-16 and 10-18, and no reading has been written since, across 15 clicks (6–7 read per receipt, never growing). The canonical G frontier is bound to no Lean frontier statement, so `IncrementalTaskFrontier.lean:126` (ranked frontier = open tasks) is never checked. Source: claude-9, `futon2/holes/labs/wm-contract/E-outer-selection-landscape.md` §10, `6a373d3e4`; this corrects the earlier wording "3 per click, in field order".
2. **Tickets that cannot be selected.** All 171 tickets are excluded before scoring. A ticket's wants found on the fly have no locator.
3. **G cannot see dependencies on other missions.** Inside a cascade, step order is derived from what each step needs (`construction.clj:125`), so G never scores an impossible internal order. What it cannot see is a need that nothing inside the cascade satisfies, i.e. a dependency on another mission, and no stage reads readiness. (Narrowed from "G has no dependency input", per claude-9 §10.3.) Two cascades of 27 and 12 units scored identical G to 16 digits (claude-9's H1: only novelty separates candidates).

## Overview draft (claude-11, 2026-10-26)

Joe: "the future becomes a big river delta, not a focused or articulated themed set of work packages." The draft groups all 789 items in the field (receipt of 10-25) into Cascade Live's 12 historic basins, 14 new text clusters, and a residual: `~/notes/wm-next/giraffe-overview-draft.md`, with scripts and data in `~/notes/experiments/2026-10-26-giraffe-overview/`. The basins take 161 items, the new groups 450; 141 machine-made repair tickets and 37 weak fits are left over. Next: Joe names, merges and orders the groups and says what waits on what; that ordering is the input G lacks.

## Two ideas from Joe (2026-10-26, after a Claude.ai conversation)

1. **Reference implementations instead of Jira.** Jira is not open source, so it cannot be downloaded and read, but Plane can, as can other open-source trackers (OpenProject, Taiga, Redmine). Read how they model a work item: issue relations (blocks / blocked by, relates, duplicates), parent and sub-issues, cycles and modules, states, estimates. Use them as reference implementations for the work-item structure, the way DiagramProver was checked against reference reasoning systems; possibly wire them together.
2. **Ordering is the structure (Gray's 3/2-categories, per Joe).** The backlog problem is choosing among orderings of ~700 targets, and some orderings are better than others: custard versus sweet scrambled eggs, same ingredients, the order of steps decides the result. Joe's instance: the futon0 APM work (`M-apm-capability-ratchet`) does not make sense before the "AI can pass prelims" capability (`M-futonzero-prelim-practice`). This is the first cross-mission dependency stated in Joe's words, and it is exactly finding 3's gap: G scores each item alone and cannot see it.
   Two levels to keep apart: (a) a step that cannot be composed yet, because what it needs does not exist (APM before prelims); (b) among orders that can all be composed, which is better. Inside a cascade, (a) is already derived from needs (`construction.clj:125`); between missions, neither exists yet.
