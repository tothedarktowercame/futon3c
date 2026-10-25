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
