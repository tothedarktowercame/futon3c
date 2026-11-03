# M-daydreamer-redux: agents that dream up features

**Status:** OPEN — IDENTIFY (drafted 2026-11-02 by claude-16 from Joe's idea; no owner yet)
**Date:** 2026-11-02
**Wing:** D, goals and structure can change (proposed)

## IDENTIFY

Joe, 2026-11-02: "a version of the t4r infrastructure in which agents ideate about new features to build ...
a bit like the War Machine but without the burden of actually building anything, just telling stories." The
system could dream in Chinese, helping explore Joe's view that Chinese is a better language for agents to
communicate with each other. It gets a bounded number of clicks (t4r rounds). "On waking it could 'recollect'
its dream in a short English summary for me."

**Problem.** Goal formation has no cheap path. New goals arrive from Joe, or from gaps found in work. The War
Machine selects among existing items, and every click pays for building. Nothing here imagines what is not yet an
item, at low cost and with no risk to the record.

**Prior work in the stack.**
- `futon3/library/t4r/`: talk4real, a Read, Evaluate, Print, Loop workshop with facilitator, recorder, reporter and
  persona roles (`supplement.flexiarg`).
- VSATARCS "lucid view" (`futon4/docs/VSATARCS.md:9650`; source `futon5a/holes/stories/war-machine-lucid-dreams.md`):
  first-person scenes marked `:imagined`, serving as roadmap, audit complement and memory. Its rule: "a dream earns
  the right to be told as fact exactly when the field it predicts actually moves."
- `futon3c/holes/missions/E-storyteller.md`: scoped, not started.

**Outside source.** Erik Mueller's DAYDREAMER (GPL-2.0, Common Lisp, about 18,000 lines), cloned read-only to
`~/code/refs/daydreamer`. Study the ideas; any copied code carries GPL-2.0 terms.

## Proposed correspondence, to be tested in MAP

| DAYDREAMER | Here |
|---|---|
| Personal goals | Open needs: failing ways, `open-unplaced`, bare bones |
| Realistic goals: recovery, rehearsal, reversal, repercussions | Repair a failing way; rehearse a feature; replay a failed mission differently; imagine what a change would break |
| Fanciful goals: rationalization, revenge, roving | Roving at least: wander from a pleasant or finished item |
| Episodic memory | Evidence store, finished items, minus40 |
| Serendipity | A retrieved memory happens to serve another open goal |
| Rule mutation when stuck | Three-halves blends over patterns |
| Daydreaming mode and performance mode | Dream and the War Machine: same field, no effects |

## Shape of a dream

One click is one t4r round. Read: seed plus retrieval. Evaluate: personae respond. Print: a scene. Loop: choose the
next daydreaming goal. A dream gets N clicks (start with 6), read-only tools, and no writes except its own dream
file. Waking produces a short English recollection for Joe.

Invariants: a dream changes nothing but its own record; it stops at its click budget; a recollection is marked
`:imagined`; nothing becomes an item unless Joe picks it.

## The Chinese question

Paired arms: the same seeds, dreamed in Chinese and in English, each recollected in English. Compare tokens per round,
and which recollections Joe picks, read blind to the arm. Token counts alone do not settle "better": the question is
whether agents reach usable ideas for less, or reach different ones. Few dreams will only give a hint.

## Open questions

1. Is a dream a seat (Kimi, a Haiku call) or a subagent? Joe's hold on Zai dispatches applies.
2. Where do dreams live, and does Cascade Live show them?
3. Should a dream be seeded by Joe, by Giraffe, or by nothing?
4. What would count as a good dream? Joe picking it is a start; Lucid-scene rule above is the long test.

## Exit from IDENTIFY

Joe confirms the problem and an owner is named.

## Tracer (Joe, 2026-11-02)

Joe: flow on Cascade Live runs bottom to top, like a hexagram. New work is noticed and assimilated at the bottom, moves up
through the Uxbridge layer to become a completed mission, and possibly a way of working or evidence for or against the
thesis. This mission is the tracer for whether that happens. Each line records the date passed and what carried it, or
where it stuck.

| Line | Rung | Passes when | State |
|---|---|---|---|
| 1 | Noticed | Item is on the page's bottom layer | Stuck, see below |
| 2 | Assimilated | Has what, status, way, cluster or `unplaced` | Not reached |
| 3 | Uxbridge | Card moves plan, built, ran, live; the effect is produced and a matched null control fails | Not reached |
| 4 | Mission | Lifecycle completed, finished item attests upward | Not reached |
| 5 | Way | A way of working declared or changed because of it | Not reached |
| 6 | Thesis | Cited as evidence for or against the thesis | Not reached |

Candidate promotion test for line 3: a dream with retrieval yields a recollection Joe picks more often than a matched
dream with retrieval held constant.

First reading, 2026-11-02 21:48Z. The file exists and mission sync saw it at 21:31Z, but it is not in the draw pile.
`tickets-section` lists docs minus clocked stems, so a clock must have taken it out; the clock edge read is capped at 200, and
none was visible. An item leaves line 1 when anyone clocks it, with no placement and no row anywhere else on the page.
