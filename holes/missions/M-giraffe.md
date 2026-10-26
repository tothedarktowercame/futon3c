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
2. **Ordering is the structure (Goguen's 3/2-categories, per Joe. Checked 2026-10-26 against Diaconescu, *3/2-Institutions: an institution theory for conceptual blending*, arXiv:1708.09675, §1 and §2.3; text in `~/notes/wm-next/goguen/`: a 3/2-category is a category whose hom-sets are partial orders preserved by composition (also called ordered or locally ordered categories; Poset-enriched), "half-way between ordinary categories and 2-categories; according to Goguen, this is what motivates the term". Gray is not cited for it; claude-11's earlier attribution to Gray is withdrawn. Goguen's blend is a lax cocone in which the arrows are partial theory morphisms; his 3/2-pushouts are Jay's near pushouts, and Goguen argues their lack of uniqueness is what makes them fit blending. Sources: Goguen, *An Introduction to Algebraic Semiotics* (1999); *What is a concept?* (2005); also *Categorical approaches to merging software changes* (unpublished draft, 1995)).** The backlog problem is choosing among orderings of ~700 targets, and some orderings are better than others: custard versus sweet scrambled eggs, same ingredients, the order of steps decides the result. Joe's instance: the futon0 APM work (`M-apm-capability-ratchet`) does not make sense before the "AI can pass prelims" capability (`M-futonzero-prelim-practice`). This is the first cross-mission dependency stated in Joe's words, and it is exactly finding 3's gap: G scores each item alone and cannot see it.
   Two levels to keep apart: (a) a step that cannot be composed yet, because what it needs does not exist (APM before prelims); (b) among orders that can all be composed, which is better. Inside a cascade, (a) is already derived from needs (`construction.clj:125`); between missions, neither exists yet.
   **Already built (Joe, 2026-10-26: "we have it in Lean").** The 3/2-blend carrier is `mathlib4/DarkTower/WarMachine/ThreeHalvesBlend.lean` (theories, partial maps under the quality order, monotone composition, cones, 3/2-pushouts), with `ThreeHalvesRuntimeCertificate/Witness/CascadeBridge.lean`; the runtime records occurrence squares (`:wm/three-halves-square-v1`, IF = I₁, HOWEVER = I₂, BECAUSE = G, THEN = B, Joe 2026-10-16). Goguen 1999 and 2006 are filed in `futon2/holes/labs/wm-contract/refs/`. `NOTE-three-halves-blend-carrier-2026-10-06.md` §2 already separates the two orders: construction order is derived from the diagram; application order is a separate field with its own evidence per edge, never derived, and obligation 17's witness is two policies with identical squares and different application orders (custard and scrambled eggs). `NOTE-outer-cascade-as-pasted-blends-2026-10-05.md` reads the outer loop this way, but over four meta patterns, not the field's items. So the gap for the backlog is only the outer level: application-order edges between missions, with evidence. Joe's APM-after-prelims is the first such edge, evidence = operator. The Diaconescu check above was redundant with this.

## Tracker survey (codex-14, 2026-10-26)

Report: `~/notes/wm-next/tracker-survey/report.md`; read-only checkouts with sha and licence in `~/code/reference/README.md` (Plane AGPL-3.0, OpenProject GPL-3.0, Taiga MPL-2.0, Redmine GPL-2.0+). claude-11 spot-checked three cites (Redmine `issue.rb:1061-1077`, Plane `issue.py:286-293`, OpenProject `relation.rb` lag).
- **Dependencies range from a flag to an operational graph.** Taiga: a boolean `is_blocked` plus a free-text note. Plane: typed relations (blocked-by, start-before, finish-before, implemented-by …) that the core enforces nothing from. OpenProject: precedes/follows with a signed lag, cycle rejection, rescheduling of the successor. Redmine: cycle rejection, delay on precedes, and `closable?` refuses to close an issue while a blocker or a child is open.
- **None has our want + check pair**: no field for the claim to be checked, its locator, or a typed reason it cannot be checked; acceptance criteria live in free text everywhere. None records a reason for "dropped".
- **Worth taking:** keep hard readiness (blocks/requires) apart from schedule order (precedes), as Redmine and OpenProject do; data on the relation itself (lag, and for us the evidence tag the blend carrier already requires on application-order edges); an explicit cycle policy before G reads the graph; who may attest ready / done / dropped.

## Giraffe is the anticipatory analogue of m40 (Joe, 2026-10-26)

m40 crystallises what has been said: memories arrive, become dated facts, and are combined as admissible cones in the 3/2-blend carrier (`mathlib4/DarkTower/MemoryCones.lean`; minus40 `docs/SPEC-open-items.md` §6), keeping the maximal antichain rather than one best merge; a pair with no admissible cone is sorry-certified. Giraffe would do the same for what is to be done (claude-11's reading, for Joe to correct):

| m40 (past) | Giraffe (anticipated) |
|---|---|
| a memory arrives (write hook) | a want arrives: an operator turn read by 象/馬, a mission doc, a ticket |
| crystallised into dated facts | crystallised into a ticket: want, check, needs |
| combined memory = admissible cone | work package = admissible blend of tickets, with application-order edges |
| conflict on a claim schema (corner store: 23rd vs 40th) | conflict on a plan: two packages claiming the same capability, or orders that cycle |
| maximal antichain of merges, no unique best | maximal antichain of orderings, no unique best; G chooses among them |
| sorry-certified: no admissible cone | an open gate: no admissible order yet (APM before prelims) |
| `asof`: what was true then | "ready as of": what can start now, what waits on what |

## Lynn (2026): blending as predictive processing, and what her code does not reproduce (claude-11, 2026-10-26)

Source: Lynn, "Conceptual Blending in Humans and Language Models", *Frontiers in Psychology* 17, doi 10.3389/fpsyg.2026.1849083. Full notes, the supplementary script (`osprey.py`) and the run are in `~/notes/wm-next/anticipation/parr-friston-and-futon-theory.md` and `lynn-supp/` beside it.

**Claim in the text.** The goal "functions as a control constraint that selects among possible source concept elements": source concepts send their features as predictions, prediction error flows back, and the blend is the selective combination that stabilises.

**What the open code does.** Four aircraft prototypes as role:filler bundles in a vector-symbolic model. The posterior over prototypes is a softmax of log prior − 4·error (helicopter 0.59, airplane 0.23). The blend takes each role's filler from whichever of the top two prototypes fits the observation better. Blend error 0.17 against 0.38 for the best single prototype (numpy, seed 13; the paper reports no numbers to compare with).

**What does not reproduce.**
- There is no goal term. The script's docstring says "No synergy terms and no goals." In the worked example this follows from the set-up: the goal is to understand the percept, so "the node representing the goal is the same as the node that comes to have the blended knowledge". The goal and the observation are the same node, and fitting the observation is pursuing the goal. The general claim, that a goal *different from* the observation selects what is inherited, is never exercised.
- The test is close to circular. Each filler is chosen by fit to the observation and the blend is then scored on that observation, so the blend almost has to win. A held-out check would score features the blend predicts but was not chosen for (hover, cruise efficiency) against later observations.
- No relations between roles, so nothing like Goguen's axioms is carried or broken.

**Why this is the question for Giraffe.** Giraffe is exactly the case Lynn's code leaves out: offline, between clicks, there is no observation (Parr–Pezzulo–Friston: the present is switched off for recall and planning), so the goal cannot coincide with the observation. Something else has to be the target. In active inference that is the preferred outcomes, the C term of expected free energy (Parr–Friston 2018); for us, the attested wants. So our version has to say what Lynn's does not: how a target that is not an observation selects which features of each input a work-package blend inherits.

**Against the carrier.** Lynn's set-up is a `Span` in `mathlib4/DarkTower/WarMachine/ThreeHalvesBlend.lean`: generic theory = the shared role vocabulary, inputs = the two prototypes, blend = a `Cone` with partial legs, one source per role. The two rankings differ in kind. K2' (`Cone.qualityLE`) is structural and partial, uses no evidence, and `MemoryCones.lean` keeps the maximal antichain. Lynn's is evidential and total (cosine to an observation, argmax).

**Proposed reading (not yet argued against the Question's field list).**
1. K2' prunes candidate work-package blends to the maximal antichain.
2. Fit to the attested wants ranks within that antichain.
3. Both scores are recorded on the package, never merged into one number.
4. The package is marked *imagined* until a real observation (a click's result) scores it; then *attested*. This mark goes in the provenance field ("who said so, and when").

Test that would separate this from Lynn's fitting step: rank packages by want-fit, run the top one, and score the features it predicted but was not selected for against the click's result.
