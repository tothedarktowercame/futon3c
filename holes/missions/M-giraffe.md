# M-giraffe — start from the data structure: a work pool the War Machine can read

**Date:** 2026-10-25
**Status:** OPEN — VERIFY planned, ARGUE approved provisionally pending the mock-up (2026-10-27; ARGUE open items listed in §Still open in ARGUE; DERIVE open from 2026-10-26, items still unchecked; HEAD, IDENTIFY and MAP exits recorded in §MAP close-out)
**Owner:** Joe + claude-11
**Follows:** M-kangaroo (warm pouches, i.e. agents that persist; this mission is about the work those agents are given).
**Serves:** our own War Machine first. Second, the consulting offer: "show me your agent logs and your ticket tracker, and I will show you how to make it work better" (pudding-prover T2.4/T2.5, `warm-customer-pays`; T∞).

## HEAD

"In programming it's good to start with the data structure" (Joe). We have a War Machine that is said to be fully functional, sitting over a pool of work so unstructured that "pool" may give it too much credence. Measured on 2026-10-25 (claude-11's REPL; data under `~/notes/experiments/2026-10-25-wm-wants/` and `~/notes/experiments/2026-10-25-mission-star-audit/`):

- The latest click's field holds 786 M/E/T/A items: 109 eligible, 677 excluded. All 171 tickets are excluded: 102 repair findings with no root (now typed: 92 machine-owned, 10 no-task-selected; futon2 `03fdb29ee`), 30 not on the current map, 26 attached to a root, 13 with a root that is not current.
- G scored 4 of the 109 eligible items. Two tied exactly, and the tie was broken by a random draw. G has no input for dependencies or readiness. Correction (claude-9, 2026-10-26): nor does anything before G exclude for an unmet dependency. The S1 exclusions are ownership, map membership and repair-finding reasons only, so a dependency-blocked item is eligible and can be chosen. A readiness field would add an exclusion (or a rating), not make an existing one visible.
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
- **Also worth taking:** a reason on `dropped` (obsolete, duplicate, declined, impossible, superseded); separate roles for source/attester, doer and accountable owner, rather than one `owner`; source system + immutable external id kept apart from the prose attribution (Plane, Taiga), which a tracker adapter needs; a revision number (Redmine `lock_version`) so two agents cannot silently overwrite each other's state change.

Candidate fields against the four trackers ("—" = no core field; cites in the report's table):

| Candidate field | Plane | OpenProject | Taiga | Redmine |
|---|---|---|---|---|
| Stated want | name + description | subject + description | subject + description | subject + description |
| Want in owner's words, with source | — (import id only) | — (author only) | — (owner only) | — (author only) |
| Check / locator | — | — (`done_ratio` is progress) | — | — (`done_ratio` is progress) |
| Typed reason no check yet | — | — | — | — |
| What must exist first | typed edges, not enforced | typed edges, lag, cycles rejected, rescheduling | blocked flag + note | typed edges, cycles rejected, `closable?` refuses |
| Owner, state, dated transitions | assignees; state; activity log | assignee + responsible; status; journals | owner + assignee; status; history | assignee; status; journals |
| Dropped, with reason | cancelled state, no reason | closed, no reason | closed, no reason | closed, no reason |
| Capability or goal served | module (closest) | parent / version (closest) | epic (closest) | version / parent (closest) |
| Provenance: who and when | created_by + timestamps | author + journal | owner + history | author + journal |

## Overlap with the outer-selection work (claude-9, 2026-10-26)

claude-9 owns the selection side of the same question: `futon2/holes/labs/wm-contract/E-outer-selection-landscape.md` and the defect list `~/notes/wm-next/outer-selection/DEFECTS-reading-era.md` (R1–R14, U1–U4). Joe's standing course there: no clicks until that list is closed. Reply on thread `invoke-1792990514863-4165-a2abaa2f`. claude-11 confirmed by grep that the three selector namespaces (`meta_live_outer_selector.clj`, `target_reading_registry.clj`, `family_selection.clj`) read no dependency, blocked or readiness key.
- **Owner and state.** The selector's only notion of state is the typed S1 exclusion reasons (`:ownership/actively-held`, `:pipeline/not-on-current-map`, `:repair-finding/root-missing`, …). They are computed per click; none is a dated transition.
- **Why it chose.** `:wm/outer-selection-trace-v1` (stages, counts, dropped-by-reason) is kept beside the receipt. It records the funnel, not a reason derived from the item's own fields.
- **What must exist first.** Inside a cascade, `construction.clj:125` derives an edge A→B when A produces a token B's guard needs, and canonical G refuses a cyclic descent. The stated `:precedence` order is only checked against these edges. That is the tracker survey's split between hard readiness and stated order, already built at the cascade grain. Nothing represents a need that no step in the cascade and nothing in q0 satisfies, i.e. a dependency between items. If giraffe's dependency edge names *the thing needed* (artifact, token, landed commit) rather than only the other item's id, the same derivation can be reused at item grain, with no second edge kind.
- **Specified twice, built never.** `NOTE-task-manager-outer-loop.md` (Joe + zai-7, 2026-09-12): stageable = fresh ∧ owned ∧ no live conflict ∧ prerequisites done; "urgency is computed, priority is received". `futon3/library/futon-theory/mission-lifecycle.flexiarg` has `:blocked` with a link to the blocking mission. `M-war-machine-pilot.md:337` has a "Depends on" block no code reads.
- **Guard vs record.** Redmine's `closable?` is a refusal. House convention (Joe 2026-09-19; PROOF-2a red-tape rule) is a typed fact beside the choice, not a block, until a ruling says otherwise (cf. claude-9's U4).
- **Readiness is computed, not stored (Joe, 2026-10-26).** Readiness is contextual: an annotation over the item, drawn from a survey of what it is connected to, not a `ready` flag written on it. Joe's analogy is the HTM neuron: its predictive state belongs to the cell, but it is set each step by the activity of the other cells its distal synapses connect to. What the cell stores is the synapses. So the item stores its dependency edges (what it needs, by name), and readiness is recomputed at each selection from whether those needs are met now, with a typed reason when one cannot be determined. A stored flag would go stale as soon as a needed thing landed or was reverted. The selector already works this way: its exclusion reasons are recomputed every click. HTM segments also fire when *enough* synapses are active, not all of them. That suggests readiness could be a degree that feeds the score, rather than a filter. Open question: is a hard "must exist first" need a threshold of 1, or a different kind of edge?
- **Sequencing.** A giraffe structure that lands as a key `evaluate` never reads satisfies claude-9's §6 rule and need not wait on the defect list; one that changes what `evaluate` reads does. Files are disjoint so far: claude-9's packets touch `outer_canonical_selection.clj`, `outer_canonical_g.clj`, `meta_live_outer_selector.clj`, `full_loop_runner.clj`; a giraffe adapter over `holes/` touches none of them.
- **H4, done by hand (claude-11, 2026-10-26, at Joe's request).** The four scored missions, G from `attempt-002/002-selection.edn`, docs in `futon5a/holes/missions/`. Each need the text names is listed with whether it could be checked against the map today. "Internal" means the mission's own work (its Missing column), not a dependency.

  | Mission (G) | Needs named in the text, outside the mission | Checkable now? | State on 2026-10-26 |
  |---|---|---|---|
  | M-war-machine-wiring (−3.95602…, chosen by draw) | substrate-2 commit-vertex coverage current for the target futon (V-1, l.1844) | by status line only | `futon3/holes/excursions/E-live-means-live.md` Status: open. Whether the gap persists was not checked. |
  | | WM-I4 / Hobbes: "WM does not act" (l.216, 291) | no: needs a ruling, not an artifact | `M-first-flights.md:561` gives it a formal home (consent gate as categorical discard); whether that settles the wall is a judgment |
  | | action-vocabulary mapping against `futon3c.portfolio.policy/expected-free-energy` (V-1.1) | yes: function exists (`policy.clj:155`) | met as a precondition; the mapping itself is internal work |
  | M-recommendation-bindings (−3.95602…, tied) | `devmap_readiness.py` must serialize per-prototype entries (l.480, 528) | yes: read the script | unmet: `futon3/scripts/devmap_readiness.py` still emits counts and missing titles only |
  | | M-devmap-revival, for full boundary coverage (l.1137) | no: name resolves to no file under `~/code` | dangling |
  | | M-portfolio-inference stepped (named as *enabled by* this mission, l.221) | yes: status line | `CLOSED — DONE (TESTING)`; this is a reverse edge, not a need |
  | M-self-improvement-loop (−3.94430…) | none: `**Blocked by:** Nothing` | yes | what remains is Joe's weekly practice, i.e. operator acts the machine cannot do |
  | M-stack-geometry (−2.00427…) | PI recommend-by-mission API; Candidate Queue peripheral (l.1764); `profile:seed:futon` loader, "Codex's territory, not this mission's" (l.854) | API: yes, by grep, absent from `futon3c/src`; peripheral: unclear what to look for | unmet |

  What this says about the field:
  1. **H4 itself: probably not.** The tied pair each have one unmet need that can be checked (an open excursion vs a missing serialization) and one that cannot (a ruling vs a dangling name). A count of unmet needs leaves the tie as it was. A threshold term would rank self-improvement-loop first, but nothing it still needs is something the machine can do. Readiness against dependencies and whether the machine can act on the mission at all are separate questions.
  2. **Needs come in five kinds**, and only the first two can be checked mechanically: an artifact or field exists (script, function, key); another item's status; a ruling (WM-I4); a dangling name; an operator practice. A ruling or a dangling name needs a typed "cannot determine" reason, not a guess.
  3. **Telling internal work from a dependency took reading.** Only stack-geometry marks the line in its own text ("not this mission's to build"). Every other need here was sorted by judgment. The edge has to say which side owns the need, or the field will count every mission's own Missing column as a dependency.
  4. **Edges point both ways.** recommendation-bindings names PI as something it *enables*. A field that records only "needs" loses that, and "enables" is what would push an item up when a lot is waiting on it.

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

## MAP close-out (claude-11, 2026-10-26)

Joe, 2026-10-26, in claude-11's REPL: "we should move from MAP to DERIVE in line with the futon4/holes/mission-lifecycle.md". The sections above are the HEAD, IDENTIFY and MAP work, written before this mission had phase headings. The verdicts below record Joe's judgment; the table adds the one MAP count still missing.

**HEAD exit: Met.** The anchor is Joe's "start with the data structure" turn and the 2026-10-25 measurements in §HEAD.

**IDENTIFY exit: Met.** The gap and scope are §Question and §Approach; Joe raised them and has directed every section since.

**Candidate fields counted over the pool.** This is §Approach's first step, done for all 109 eligible items of attempt-002 rather than the four of H4. The script and per-item rows are in `~/notes/experiments/2026-10-26-giraffe-map-close/` (`field_counts.py`, `field-counts.json`). The regexes read each item's own document, so the numbers are a floor.

| Candidate field | Items carrying it (of 109) | How counted |
|---|---|---|
| Stated want | 106 | ≥1 admitted want in the 10-25 wants view; the 3 without are E-kimi-task-28, E-kimi-task-103, E-convert-operator-turns-to-patterns |
| Every want has a locator | 97 | all admitted wants carry `:locator` |
| Want in the owner's words, with source | 2 | a checkbox line carrying "(Joe … date)" or "(operator … date)" |
| What must exist first | 27 have a `**Blocked by / Depends on / Requires …:**` line; 17 name something | about half of the 17 name another item by id; the rest name an artifact or a decision. The rest of the 27 say "None" or "Nothing" |
| Owner | 67 | `**Owner:**` line, not "unassigned" |
| State | 106 have a Status line; 54 date it | no item records dated transitions apart from git history |
| Phase exit verdicts | 3 | `**<PHASE> exit: …**` lines |
| Dropped, with reason | 0 | no Status line among the 109 says dropped, nonstarter or superseded (one says phases deferred). Whether dropped items leave the field, and where their reason would be kept, was not checked |
| Capability served | 9 | declared star links (star audit, 10-25) |
| Provenance: when | 65 | `**Date:**`, `**Created:**` or `**Opened:**` line |

**Ready vs missing.**

| Ready (no new code) | Missing (the work) |
|---|---|
| Wants with C4 locators for 97 of 109 items (`target_reading_registry`, the 10-25 wants view) | Attribution of a want to who said it and when: 2 of 109 |
| Need-derived step order inside a cascade (`construction.clj:125`), cyclic descent refused by canonical G | Edges between items: 17 of 109 state one, in prose; no code reads them (`M-war-machine-pilot.md:337` "Depends on") |
| Per-click typed exclusion reasons (S1), recomputed each click, i.e. readiness computed rather than stored, at the selector | A readiness reason for an unmet need between items; the five need kinds of H4 finding 2, of which only artifact-exists and item-status are checkable |
| Application-order edges with evidence, specified at the blend-carrier level (`ThreeHalvesBlend.lean`, `NOTE-three-halves-blend-carrier-2026-10-06.md` §2) | Application-order edges between missions: one so far (APM after prelims, evidence = operator) |
| `:wm/outer-selection-trace-v1` (why the funnel dropped what) | A reason for the choice derived from the item's own fields |
| Status lines (106) and git history | Dated state transitions as data; `dropped` with a reason; roles (source / doer / accountable); revision number |
| Capability stars (9 of 109 linked) | Star links for the other 100 |
| Overview draft grouping 789 items into basins and clusters | Joe's names, merges and order for those groups |
| Tracker survey (four reference trackers, cited) | An adapter: one per source; none yet, holes/ first |

**Surprises recorded before DERIVE.** (1) Wants are well located (97 of 109) but almost never attributed (2 of 109): the check exists and the "who said so" does not, which is the reverse of what the four trackers have. (2) A quarter of the items have a dependency line, but most of those lines say "None": the field is a habit of the template, not a record. (3) The field holds no dropped item at all, so "dropped, with reason" cannot be counted from it.

**MAP exit: Met.** Every candidate field has a count, and the ready/missing table is complete. One question stays open and moves to DERIVE as a design choice: is a hard need a threshold of 1, or a different kind of edge?

## DERIVE (open, from 2026-10-26)

Exit criterion (lifecycle): someone could implement the mission from this section alone. Each item below is a draft position taken from MAP, to be argued or replaced. None is settled yet.

- [ ] **Entity: the work item is a projection over the existing M/E/T documents, not a replacement.** IF the item should carry want, check, needs, roles, state and provenance, HOWEVER 422 documents already exist and Joe's invariant forbids a large redesign before a run is shown, THEN an adapter reads each document into an item record and writes nothing back, BECAUSE a projection can be checked against the pool today, and a structure `evaluate` does not read can land without waiting on claude-9's defect list (§Overlap, sequencing).
- [ ] **Relation: a need edge names the thing needed, not only the other item.** Fields: kind (one of H4's five: artifact, item-status, ruling, dangling name, operator practice); target (path / key / item id); owning side (this item's own work or someone else's, H4 finding 3); direction (`needs` or `enables`, H4 finding 4); evidence (who stated it, where). This reuses `construction.clj:125`'s derivation at item grain (§Overlap).
- [ ] **Relation: hard readiness kept apart from application order.** `needs` is readiness; `precedes` is order, with its own evidence, never derived (blend-carrier NOTE §2; Redmine and OpenProject keep the same split).
- [ ] **Invariant: readiness is never stored.** It is computed at each selection from the edges, and a need that cannot be determined (ruling, dangling name) gets a typed reason, not a guess (Joe, HTM analogy).
- [x] **Invariant: a cycle policy before anything reads the graph: record and flag, never refuse.** Joe, 2026-10-26: a cycle of hard needs is what a project manager would call "needs breakdown", and many natural systems have mutual dependency and feedback inside them, so a cycle is not an error in the field. The cycle is recorded beside the choice (house convention, §Overlap guard vs record) and its members are flagged `:needs-breakdown`: a prompt to split the items until the mutual need becomes edges between their parts, not a verdict that the work is impossible. Mechanics under §What the existing refusals mean here, `:cycle`.
- [ ] **Invariant: an item's own Missing column is never counted as its dependency.**
- [ ] **State: dated transitions, `dropped` with a reason** (obsolete, duplicate, declined, impossible, superseded), roles source / attester / doer / accountable, source system + external id, revision number (tracker survey).
- [ ] **Data flow:** holes/ adapter → item records (location open: substrate, files, or both) → read by a tool for Joe first (the overview groups, with what waits on what), and by the selector only after the defect list closes.
- [ ] **Open choice: a hard need as threshold 1 of a degree, or a separate edge kind** (HTM segments; H4 finding 1: a threshold term alone would rank M-self-improvement-loop first).
- [ ] **Open choice: the work-package ranking** — K2' prunes to the maximal antichain, fit to attested wants ranks within it, both scores recorded, packages marked *imagined* until a click scores them (§Lynn, proposed reading).
- [ ] **Wiring diagram:** triggers 2 and 3 of the lifecycle's list hold (adapter writes what the selector reads; it crosses futon3c/futon2), so one is drawn before VERIFY.
- [ ] **Fidelity contract (GF):** greenfield over the documents; declared commitments are the candidate-field list and the tracker survey's "worth taking" list.

### Sketch: items as open theories, glued with the pattern cascade (claude-11, 2026-10-26, for claude-9's comments)

Joe, 2026-10-26: mining relations between tasks is the task-side counterpart of the Giant Connected Cascade 馬 is building for patterns. Giraffe could reuse the same machinery over task space, and the notebook (`futon3c/notebooks/daxiang_live.clj` §7) already does most of the translating. This sketch takes up the Relation, Invariant and Open-choice items above. It does not replace them.

**Encoding, item by item, after `pattern-theory` (`daxiang_live.clj:399`).** Each item `k` is a small open theory built with `ot/theory`:
- `k-ready` = the `and` of its hard needs (the gate);
- `k-open` = `k-ready and not k-landed` (it can start now and has not landed);
- `k-landed` is the interface variable other theories read, the counterpart of a pattern's `-then`. It is always a leaf, observed from the status line; no theory owns it, since the Missing column may not be used to derive it (invariant above). For patterns, a non-leaf `-then` is owned by a link theory; items have no such owner.
- An item with no hard needs (e.g. M-self-improvement-loop, "Blocked by: Nothing") cannot have a `k-ready` mechanism: the grammar has no constant. Its `k-ready` is an observed leaf, value true, with reason `:needs/none-declared`. That keeps the "Blocked by: None is a template habit" finding visible as an observation rather than a derivation (claude-9, correction 3).
The two sides fit together: a pattern's `:why` says which `-then` answers its `-if` (the reason for doing something next), and an item's needs say which variables must hold before it can start (the gate). In the glued theory both kinds are mechanisms over the same variable names.

**Need kinds (H4 finding 2) as variables.**
| Kind | Variable | Comes from |
|---|---|---|
| artifact exists | `art:<path or key>` | a leaf, observed by checking the file system or the store |
| item status | `j-landed` | owned by item `j`'s theory |
| ruling | `rule:<id>` | a leaf, *undetermined* unless the operator has ruled |
| dangling name | none | refused at adapter time with a typed reason; never a variable |
| operator practice | `op:<id>` | a leaf, *undetermined* |
Every `art:`, `rule:`, `op:` and foreign `j-landed` variable is declared in the interface of each theory that mentions it: `glue` refuses `:undeclared-sharing` when two items both need `art:X` and either leaves it out (claude-9, correction 4).
An `enables` edge (H4 finding 4) makes `k-landed` an input of the target's need variable, through a link theory like `link-theory` (`daxiang_live.clj:414`). The equation grammar only has two-input connectives, so longer conjunctions get private intermediate variables, as the notebook already does for `or`.

**What the existing refusals mean here.**
- `:double-mechanism`: two theories both claim to establish the same variable, e.g. two items each say they produce `art:X`. Since `k-landed` has no owner, this can arise only from the `art:X = k-landed` mechanisms written for `enables` edges. This is the "conflict on a plan" row of the m40 table. Recorded as a typed fact (house convention), not resolved by guessing.
- `:cycle`: a cycle of hard needs. `glue` validates the whole glued DAG and refuses everything on any cycle, so no item would be evaluated and none would get a reason (claude-9, correction 2). So the adapter runs a cycle pass first: strongly connected components over the item-grain need edges. Members of a cycle are recorded with readiness `{:status :undetermined :reason :needs/cyclic :evidence [item ids on the cycle]}`, and their theories are withheld from the glue; their `-landed` leaves can still be observed by the other items. The record also carries `:flag :needs-breakdown` and the component as a whole (`:cycle/members`, the need edges that close it), so the overview can show the knot and someone can break it down. Nothing is dropped from the field or from S1 because of it. This is the cycle-policy invariant above (Joe, 2026-10-26: "record and flag but don't refuse").
- Matching by name will produce false sharings at item grain. The adapter emits a variable only from an explicit path, key or item id, never from prose, and keeps a list of the name collisions it saw.

**Readiness and the "what it unblocks" score, as computations.**
- Readiness of `k` at a selection is three-valued, computed by an outer loop around the two-valued `scm/evaluate`. `evaluate` reads a missing exogenous leaf as false (`(boolean (get assignment node))`, `scm.clj:91`), which would make a pending ruling indistinguishable from a refused need. So, before evaluating: take the exogenous ancestors of `k-ready` in the glued DAG; if any has no observation, `k` is `{:status :undetermined :reason :needs/unruled :evidence [rule:<id> …]}` (sibling reasons `:needs/operator-practice`, `:needs/dangling-name`, `:needs/cyclic`) and `evaluate` is not consulted for it. Otherwise `k-ready` is `evaluate`'s value. This mirrors the S1 membership-unavailable branch (`:eligibility-status :unknown` beside its reason). Never stored (Joe, HTM analogy).
- The readiness record is per item, not per choice: a map item-id → readiness covering every support and excluded row. It carries `:as-of` equal to the selection's (the focus receipt's `:selection-as-of-required` hazard) and the sealed leaf assignment it evaluated: every `art:`, `rule:`, `op:` and `j-landed` leaf with its observed value or `{:absent <reason>}`. Readiness is then replayable from the record alone, not from today's file system (claude-9, answer (a)).
- What landing `k` unblocks = the set of `j` whose `j-ready` changes from false to true when `evaluate` is called with the intervention `{k-landed true}`. `evaluate` takes the intervention map directly; `surgery/do-intervention` is only needed for d-separation questions. This set is "unblocked now": an item with two unmet needs does not flip when one lands. Record it, and beside it the items whose unmet-need count drops by one (claude-9, correction 6). This is a causal question asked of the glued model, not a weighting. The same question asked of a capability star is Joe's "joe-passes-prelims … depends on a calculation over the rest of the potential-capability landscape" (`M-象-2000/data/moves-scale3/shard-3/labels.py:100`).
- Threshold vs edge kind (open choice above): in this encoding a hard need is a conjunct of `k-ready`, and a soft one is not. A soft need could feed a degree later, once values stop being Boolean. Proposed: two edge kinds now, no degrees.

**Pieces that are not built yet (M-diagramprover.md:2005–2010), and what DERIVE requires of each.**
1. *Mechanisms written by hand.* Giraffe's adapter is the miner for task space: it writes the mechanisms from the documents' Blocked-by/Depends-on lines and declared enables. 馬 does the same for patterns. Both write `ot/theory` values, so they can be glued together.
2. *Boolean values.* Keep Boolean for the first test. *Undetermined* (rulings, practices, cycles) is a leaf with no observation, plus a typed reason, and it is caught by the outer loop above before `evaluate` would default it to false.
3. *Variables identified by name.* As above: explicit targets only; collisions are listed.
4. *Predictions not tested against the act history.* Every do(·) cone is a prediction: "if `k` lands, `j` becomes ready". The act and commit history says whether `j` was started after `k` landed. This is the falsification test, and the first one below is its smallest form.

**First test (small, existing code only).** Glue the roughly eight item-to-item need edges and the one enables edge found in MAP, plus the two prelims stars, with `ot/glue`. Compute the cone of do(`ai-passes-prelims-landed`). Compare it with what was actually started afterwards. Output: one receipt with the glue result (or its refusal), the cone, and the comparison. Pass: the glue succeeds or refuses with a typed reason that names the colliding variables; the cone is non-empty; every member of the cone is checked against history as either confirmed or not; the receipt lists the leaves it could not observe, so a cone that is empty because of undetermined leaves reads differently from one that is empty because nothing depends on the target (claude-9). Before running, the prediction is written down here.

**claude-9's answers (WM side, 2026-10-26).** claude-9 read the sketch against `outer_canonical_selection.clj` (`evaluate`), `meta_live_outer_selector.clj` (S1 block), `outer_selection_trace.clj`, `cascade_coapply_g.clj` and the three causal namespaces.
- (a) *Can the selector read `k-ready` and the cone without changing `evaluate`?* Yes. `evaluate` reads `:status`, `:G`, `:policy-id`, `:cascade-digest`, `:action-pin`, the menu pin and (since R17) `excluded-keys`; readiness and cones are keys it never reads, so they are captures beside the receipt, at `[<arm> :<name>]` on the selection cell next to `:outer-selection-trace` and `:reading-preparation`. That needs one retain-time call in `full_loop_runner.clj` (claude-9's file, a small packet like C4b). Until then the record is written to its own file keyed by run id and joined offline. Conditions: the as-of stamp and sealed leaf assignment, and per-item rather than per-choice (both above).
- (b) *Does `:needs/cyclic` go with the S1 exclusion reasons or in the trace?* Neither. The keyword has the right `:<stage>/<reason>` form, but `[:excluded]` would make it a guard that drops the item from support, against the "typed fact beside the choice until a ruling" invariant (U4 was ruled that way on the 26th). It goes in the readiness record as a status reason. If Joe later rules that unready items are excluded, the same keyword moves into S1 unchanged. The trace is a projection of the receipt and takes no new facts; counting readiness statuses there would be a trace v2, not now.
- (c) *Does a cone feed G or only the brief?* Brief and receipt now. G is the per-cascade score (`scorer/rank-cascade-actions` minus the frontier-beta novelty term; Lean `CascadeCoApplyGNovelty`, `classKernel`), and a cone is per item. Any new term changes the ranking law, and Lean specifies, code conforms: a cone enters G only after a Lean clause defines it and Joe rules on it. The first test above is the case for that clause. Until a recorded cone has predicted history better than no cone, it is a computed urgency shown in the brief and retained beside the receipt.

## ARGUE (open, from 2026-10-27)

Joe, 2026-10-27, in claude-11's REPL, opened ARGUE and gave its outline: the theory (papers, the landscape map, a category-theoretic treatment, the design patterns we already have) has to be distilled into one common-sense paragraph. The thread through it: the War Machine may select any task, but for its selections to mean anything it should prefer tasks that can be done now and tasks that unblock others. The proof work is the evidence that this works. DERIVE's unchecked items stay open: per the lifecycle, the pattern survey below may revise them, and DERIVE's exit is recorded when they close.

### Plain-language argument (draft)

A list of work tells you what someone wants done. It does not tell you what to do next. To decide that, you need two more facts about each item: can it be started now, and what does finishing it let other people start? When we gave our proof seats exactly that (each problem broken into small lemmas, each lemma saying which others it needs, a lemma that turns out too big split rather than retried, and the pieces many others wait on done first), 72 problems were closed in two weeks. Where the same seats worked over a dependency map that had no readiness and no ordering by what unblocks what, two chains grew for weeks without closing anything. Giraffe gives every item in our pool, and in a client's tracker, those two facts: computed each time a choice is made, never stored, and recorded beside the choice so it can be checked afterwards. An item that turns out to depend on itself is not refused: it is marked as needing to be broken down, the same thing a proof seat does with a lemma it cannot finish.

### The evidence from the proof work

- **Lemma rounds (claude-14, 2026-10-13 to 2026-10-26; `~/notes/packets/lemmas/STATUS.md`).** Each problem was given a blueprint: lemmas in dependency layers. The seat brief has a `BLOCKED-ON <name>` rule (`preamble.md:18`): a seat that cannot prove its lemma without one that is still `sorry` stops and the lemma is re-queued after that one lands, i.e. readiness is checked when the seat starts, not assumed. A partial must bank its proved steps as `_aux_` helpers and leave the rest as separate lemmas for other seats (`preamble.md:21`). Ledger counts: 72 distinct problems recorded CLOSED, 1442 lemma rows PASS, 289 rows PARTIAL, each split rather than sent out again. Merged to master by `merge_lemmas.sh` (last 2026-10-26, `fe63b1840`).
- **Topology loops (from 2026-09).** `apm-lean/TOPOLOGY-ROUTES.md` records that the earlier `TOPOLOGY-DEPENDENCY-DAG.md` "named 31 of the open problems and let two chains grow for weeks without touching a `sorry`" (`/home/joe/topo-watch/dag-audit.md`). That map had dependencies but no readiness and no rule for which piece to build first; the routes file that replaced it added election rules ("infrastructure steps shared by two or more of its problems first").
- **What this comparison is not.** The problems, seats and dates differ, and the lemma rounds also had a reviewed decomposition and mechanical acceptance (`lemmatool.py check`). So it shows that breakdown + readiness + unblock-first ordering can carry a large body of work to closure here, not that ordering alone caused the difference. The VERIFY test for Giraffe is the one in DERIVE: a recorded cone predicts what is started next.

### Pattern cross-reference

| Pattern | Where it applies in DERIVE | How |
|---|---|---|
| `cascade-construction/order-by-what-each-step-needs` | need edges; `k-ready` as the `and` of hard needs | the same rule at item grain: needs in the guard, contributions in the effect, order follows (`construction.clj:125`) |
| `efficiency/order-by-what-it-unlocks` | the "what landing `k` unblocks" set | its rule ("rank shared pieces by how many consumers each one closes … recompute whenever the set of statements changes") is the do(`k-landed`) cone, recomputed at each selection rather than stored |
| `efficiency/split-the-partial-not-retry-it` | `:needs-breakdown` flag on cycles | a cycle, like a partial, is split into parts whose needs become ordinary edges; the item is not sent round again unchanged |
| `efficiency/give-the-dependent-piece-its-hypothesis` | breaking a cycle | the proof-side way to split mutual dependency: B takes A's conclusion as an explicit hypothesis, so the edge becomes an `enables` with a stated assumption instead of a hard need. Proposed as the default suggestion attached to a `:needs-breakdown` record (DERIVE revision, for Joe) |
| `efficiency/split-by-shared-dependency` | `art:` variables shared between items | split by the facts pieces would each need; each shared `art:X` becomes its own item others wait on, which is what `:undeclared-sharing` forces the adapter to make explicit |
| `efficiency/review-the-decomposition-before-dispatch` | the overview for Joe before the selector reads anything | DERIVE's data flow (Joe's tool first, selector later) is this pattern: judgment spent on the breakdown once, leaves accepted mechanically |
| `futon-theory/mission-dependency` | the whole mission | "represent missions as an explicit dependency graph so blocked work, leaf work, and high-value unblocks are visible"; MAP measured that it is not yet (17 of 109 items state an edge, in prose, and no code reads them) |

The memory side: m40 combines memories as admissible cones in the 3/2-blend carrier (`MemoryCones.lean`), and our design patterns are increasingly written as blends (`ThreeHalvesBlend.lean`). Giraffe's work packages are blends of items under the same carrier (§Giraffe is the anticipatory analogue of m40), so the memory of what was done and the plan of what to do are combined by one construction.

### Rationale citation

- `@why war-room/wr-10-next-move-surface-is-recursive-closure`: the next-move recommendation was ranked on three criteria, "direct leverage, unlock cascade, and self-referential closure". The unlock-cascade criterion was ruled in April; MAP found that the live selector has no input for it (G reads no dependencies or readiness, §HEAD). Giraffe supplies the data that criterion needs.
- `@why war-room/wr-13-futonic-debt-is-paid-in-order`: "complete work that unblocks the pilot before undertaking deferrable library hygiene", i.e. unblock-first ordering, ruled for debt.
- `@why war-room/wr-14-predecessor-completion-pull-is-a-fourth-next-move-criterion`: an item whose predecessor is nearly done is pulled forward; in DERIVE this is the "unmet-need count drops by one" record kept beside the unblocked-now set.

### Theoretical coherence

IDENTIFY's anchor was "start with the data structure". The design keeps to it: the change is a record per item (needs, readiness, unblocks, flags), and the selector's ranking law is untouched until a Lean clause and Joe's ruling admit a cone into G (claude-9's answer (c)). The active-inference reading from MAP still holds: offline there is no observation, so the attested wants are the preferred outcomes, and readiness plus unblocks says which of them can be pursued now.

Category-theoretic reading (Joe, 2026-10-27: beyond the usual standard of a mission, though the work is heading that way). Each item is an open theory with an interface (its `-landed` variable and the leaves it needs); the field is their gluing, a colimit over shared interface variables, which is what `ot/glue` computes and what its refusals (`:double-mechanism`, `:undeclared-sharing`, `:cycle`) report as failures of the colimit to exist in the DAG category. "What landing `k` unblocks" is the image of an intervention on one interface variable under evaluation of the glued theory. The same gluing over patterns is 馬's Giant Connected Cascade, so tasks and patterns land in one category and can be glued to each other. The self-ordering Joe names is this: the order of work is not supplied from outside but read off the glued graph each time. Writing this out properly (the category, the functor to evaluations, and where cycles sit) is a candidate paper, not a mission exit.

### Trade-offs

- Readiness is recomputed at every selection rather than stored: costs a pass over the glued graph per click, and buys records that cannot go stale (Joe, HTM analogy).
- Cycles are flagged, not evaluated: items on a cycle get no readiness until split. A fixed-point treatment of feedback would evaluate them, and would mean changing `glue` (claude-9's code); not taken.
- Edges come only from explicit paths, keys and item ids, never from prose: most of the 17 prose dependency lines will be missed at first, in exchange for no false edges.
- The cone goes to the brief, not into G, until it has predicted history better than no cone: the selector does not improve from this mission alone.

### Generalization

For a client's tracker the same three facts are needed, and the trackers surveyed already store the edges (Jira links, Redmine/OpenProject `blocks` vs `precedes`) but not readiness-as-of-a-choice or the unblocked-now set. The adapter is per source; the gluing, readiness and cone computations are not. What would change: need kinds (a ruling has no Jira counterpart; a "blocked by" link does), and attribution, which trackers keep and our pool lacks (2 of 109).

### Still open in ARGUE

Joe, 2026-10-27: approves the argument provisionally and will rule on it only after seeing a mock-up that works: "I don't want to trust the paragraph until I see the proof". Clarified later the same day: what waits on the mock-up is the ruling on the argument, not trust in a reading of the tickets (claude-11 had conflated the two). So the items below stay open until VERIFY has run, and are then settled against its result.

- [ ] Joe's correction of the plain-language paragraph.
- [ ] Whether `give-the-dependent-piece-its-hypothesis` becomes the suggested breakdown on a `:needs-breakdown` record (DERIVE revision).
- [ ] The DERIVE items still unchecked.
- [ ] Joe's addition (below): named, nested clusters of pending work, with needs between clusters.

### Joe's addition: clusters of pending work, with needs between them (2026-10-27)

Joe, 2026-10-27, before ruling: "one of the criteria here is to appropriately cluster items in the Pending work list, much as we've already done in the work list that was completed ... it's not necessarily just that this ticket depends on that other ticket, there's a kind of hierarchical clustering that has to go on so we can say, well, this cluster of tickets depends on that cluster of tickets, and these clusters should kind of have meaningful names."

**Where this was lost.** MAP had it in part. The overview draft grouped all 789 items (Cascade Live's 12 basins for past work, 14 text clusters for the rest), and its next step was "Joe names, merges and orders the groups and says what waits on what". The groups had one level only, there were no edges between them, and several were named "Mixed: ...". Neither DERIVE nor the plain-language paragraph took the step over. The argument now talks only about single items.

**Proposed DERIVE item (for Joe).** *Relation: clusters form a hierarchy over items, and a cluster's needs are the item needs that cross its boundary.* A cluster is a named set of items or of smaller clusters. Cluster A needs cluster B when some item in A needs some item in B. Readiness and "what it unblocks" are computed at every level by the same rules: a cluster is ready when every need crossing into it is met, and landing B unblocks the clusters whose last unmet crossing need was into B. In the open-theory encoding, a cluster is the glue of its members' theories, and its interface is the set of variables that cross its boundary. So clustering is gluing done in stages, and the computations above need no new code. The item graph can be acyclic while the cluster graph has a cycle. In that case the clusters are flagged `:needs-breakdown`, like an item cycle: either the cut is wrong, or the two clusters really do depend on each other. Names come from Joe, or are proposed and then corrected by Joe. The text-cluster labels are placeholders.

**Two kinds of grouping, which can disagree.** One groups by theme (what the overview draft did: patterns cited, missions named, text). The other groups by dependency (cut the need graph so that few edges cross). Joe's "this cluster depends on that cluster" requires the dependency kind to be readable off the themed kind. Whether the two agree is something we can measure.

**Proposed sentence for the plain-language paragraph (for Joe's correction).** After "...what does finishing it let other people start?": "The same two questions apply to groups of items. Pending work is grouped into named clusters, nested where that helps, and a cluster can be started when the clusters it needs have landed, so a plan can be read at the level of its clusters before anyone reads the tickets."

**First look on the VERIFY tickets (claude-11, from `edges.json`; names proposed, not ruled).** The 22 tickets fall into six themed groups: *queue routing and addressing* (`T-car3-*` ×3, `T-mq7-unaddressable-caller`, `T-durable-queue-rename-and-tidies`, `T-codex-auto-bellback`), *mesh delivery* (`T-mesh-qa-misrouting`, `T-mesh-edge-coverage`), *Agency job failure classes* (`T-fail-*` ×7), *pouches* (`T-kangaroo-*` ×2), *bell writes and client surfaces* (`T-typed-bell-arse-write-async`, `T-cx-new-blocks-emacs`, `T-zai-chat-transient-timeout`, `T-agency-desktop-save`), and *federation* (`T-matrix-federation-proof`). Of the 8 ticket-to-ticket edges, only one crosses between groups (`T-mq7-unaddressable-caller → T-mesh-qa-misrouting`: queue routing needs mesh delivery). Mesh delivery is done, so that edge is met. The other 7 edges stay inside their group, and every remaining need is external. The failure-class group has 7 tickets and no edges between them, so it is a group by kind, not by dependency. On this sample the cluster graph has a single edge. In other words, the tickets record needs inside a theme and hardly ever between themes. That is the H4 gap again, one level up. A VERIFY step 3c (after 3b, which is in flight and unchanged) would compute this with a checker, using the same quotient rule over the whole field, where the overview groups give the themed grouping.

## VERIFY (planned, from 2026-10-27)

Joe, 2026-10-27: verification here can be informal. A proof would count, but so would a worked example. The proposal is a proof of concept on real tickets in an existing tracker's data model, not a made-up project: show that what Giraffe does fits an ordinary project-management routine and adds something to it. Prioritising a subset is enough.

**Scenario.** One Plane project holding a cluster of our real T- tickets (`futon3c/holes/tickets/`, 36 files; choose a connected cluster of 15–25 so that edges exist, e.g. the park/bell/kangaroo tickets). Plane is used for its data model, not necessarily as a running server: records are written in the shape of its models (`reference/plane/apps/api/plane/db/models/issue.py`), so a reader can check that nothing is invented. Loading them into a local Plane instance is optional and only for the screenshot.

**Mapping into Plane** (each a field Plane already has):
- ticket doc → `Issue` (`name`, `description`, `priority`, `parent`, `start_date`/`target_date`), with `external_source = "futon-holes"` and `external_id = <path>` (the tracker survey's "source system + immutable id");
- state → a `State` in one of Plane's groups (backlog, unstarted, started, completed, cancelled); `dropped` → cancelled, which loses the reason (a gap found by the survey, recorded rather than patched);
- a `needs` edge with an explicit path, key or item id → `IssueRelation` `blocked_by`; a `precedes` edge → `start_before` / `finish_before`. Prose-only dependencies are not converted (DERIVE trade-off: no false edges).
- what Plane has no field for (the want's check or locator, a typed reason for no check, the evidence on an edge) goes in the description under a fixed heading, and is listed as a gap.

**Baseline: what a Plane user sees.** The project list ordered the way Plane's default views order it (state group, then priority, then updated), with `blocked_by` shown but not enforced (survey: "typed relations that the core enforces nothing from").

**Giraffe pass on the same records.** At one recorded choice point: readiness for every item computed from the `blocked_by` edges and the blockers' state groups; for each ready item, what finishing it unblocks (the cone, via the existing gluing in `ot/glue`, no new evaluator); cycles flagged `:needs-breakdown`, with the dependent-piece split suggested if ARGUE settles that. The active-inference reading is unchanged: the attested wants are the preferred outcomes, and readiness plus unblocks says which of them can be pursued now. The output is the same list re-ordered, each item carrying its two facts and the reason for its place.

**Finding before the cluster was chosen (claude-11, 2026-10-27).** The T- tickets have almost no explicit need edges. Of the 36, the only ticket-to-ticket references are "same series" and "Related" lists (the forward-model four: `T-forward-model-vs-active-work`, `T-exogenous-evidence-update-rule`, `T-devmap-forward-model-calibration`, `T-watu-replay-layer`; and `T-car3-queue-routing-design-note` → `T-car3-queue-routing`). The one ticket with a `## Blocker` section (`T-exogenous-evidence-update-rule`) is blocked on a state of the world (the WM loop dead since 2026-07-14), not on another ticket. Mission headers are no better: 9 mission files have a `Blocked by`/`Depends on`/`Needs`/`Requires` line, and those lines name 1 item id between them. Under the no-prose-edges rule a T- cluster converts to Plane with nearly zero `blocked_by` rows, so the Giraffe pass would have nothing to say, and that would show a gap in our records, not a test of the method. This is H4 finding 1 again at ticket grain. The cluster choice waited on Joe.

**Decision (Joe, 2026-10-27): our own tickets, with edges read from the prose.** Joe declined the public Apache Jira alternative "until we can prove here on our own issues that we can actually do something meaningful with them and add something beyond just having a list of tickets". This amends the mapping rule above: a prose dependency *is* converted, provided it carries the verbatim sentence it was read from (file and line) and is marked as read, not recorded (`:edge/source :prose-reading`), so the Plane rows can be told apart from links a person entered. The check on the reading is computed: every quote must occur verbatim at its cited line. Edges whose target is outside the cluster (a mission, or a state of the world such as "the WM loop is running") are kept, typed as external, and count as unmet unless their state can be read from the record.

**Cluster (22 tickets, chosen 2026-10-27 by claude-11): agency, bells, park and pouches.** The ten tickets that name `M-agency-hardening` (`T-agency-desktop-save`, `T-car3-phase2-impl`, `T-car3-queue-routing`, `T-car3-queue-routing-design-note`, `T-codex-auto-bellback`, `T-durable-queue-rename-and-tidies`, `T-matrix-federation-proof`, `T-mesh-edge-coverage`, `T-mesh-qa-misrouting`, `T-mq7-unaddressable-caller`); the seven Agency failure-class tickets `T-fail-*`; and `T-typed-bell-arse-write-async`, `T-cx-new-blocks-emacs`, `T-kangaroo-v1-pouch`, `T-kangaroo-caching-and-compaction`, `T-zai-chat-transient-timeout`. States come from `holes/tickets/tickets-index.edn` (triaged 2026-09-01) unless the ticket says otherwise. Order of work: (1) edge reading, one job, with the verbatim-quote check; (2) claude-11 reviews the edges and writes the prediction below; (3) conversion to Plane's shape and the Giraffe pass, a second job. Data under `~/notes/experiments/2026-10-27-giraffe-verify/`.

**Edge reading (step 1, zai-2, 2026-10-27; reviewed by claude-11).** zai-2 returned 19 edges and 7 rejections; the verbatim-quote check passed. On review claude-11 moved 3 edges to rejected: `T-car3-queue-routing → T-car3-queue-routing-design-note` (the note is what Phase 1 of the routing ticket produces, so the edge would make the ticket wait on its own output); `T-car3-phase2-impl → design-note` from design-note L60 (it orders two steps inside Phase 2); `T-fail-activation-authority-unavailable → delivery-retry handling` (the ticket says close as non-reproducing, unconditionally). Two rejections cited a line other than the one their reason was about and were re-pointed. The quote check cannot catch that: it tests that the quote is at the line, not that it is the sentence the reason is about. Result: 22 tickets, 16 edges (7 ticket-to-ticket, 9 to an external state or decision), 10 rejected; zai-2's version kept as `edges.zai2-original.json`. Limits of the schema, for the write-up: a dependency inside one ticket (the H1→H5 ladder in `T-zai-chat-transient-timeout`; the exception-payload triage in `T-fail-invoke-error` is borderline the same) cannot be expressed, and preconditions that no sentence names (the fail-census `delivering`-state blind spot, claude-6's ownership of live reload) are not edges.

**Rules fixed before the pass (claude-11).** No ticket in the cluster states a priority (the "TOMORROW (2026-06-11)" header on `T-kangaroo-caching-and-compaction` is a date), so every issue converts with Plane's priority `none`, and Plane's order among open items reduces to state group, then updated. This is a gap in our records, listed as such; priorities are not invented to make the comparison work. A blocker in the cancelled group (`:superseded`, `:dropped`) does not count as met: it is flagged `:blocker-cancelled`, because work that moved to another ticket should have its edge re-pointed, and treating it as done would hide that. External edges are unmet unless the record shows their state.

**Prediction (claude-11, 2026-10-27, written before the pass runs, from the 16 edges and the index states only):**
- [ ] Open items: 15 (6 started, 7 unstarted, 2 backlog). Ready: 6 (`T-car3-queue-routing-design-note`, `T-fail-activation-authority-unavailable`, `T-fail-agent-not-found`, `T-fail-invoke-interrupted`, `T-fail-operator-cancelled`, `T-zai-chat-transient-timeout`); not ready: 9.
- [ ] Plane's top five (with no priorities, five of the six started items, by updated): at least 4 are not ready, and 5 unless the design note is among them. `T-car3-phase2-impl` and `T-mq7-unaddressable-caller` are blocked by the superseded `T-car3-queue-routing` (`:blocker-cancelled`); `T-fail-no-execution-evidence`, `T-fail-worker-lost-on-restart` and `T-kangaroo-caching-and-compaction` wait on external items.
- [ ] Which ready item unblocks the most: none unblocks anything in the cluster. Every ticket-to-ticket edge points at a done or superseded ticket, so every ready item's cone is empty. What unblocks most is outside the tickets: Joe's decisions gate 3 items (`T-cx-new-blocks-emacs`, `T-kangaroo-caching-and-compaction`, `T-matrix-federation-proof`), and re-pointing the two edges from the superseded routing ticket would decide 2 more.
- [ ] No cycle: the 7 ticket-to-ticket edges form a DAG with `T-durable-queue-rename-and-tidies` (done) at the top.

If this holds, the second pass-criterion is met by the `:blocker-cancelled` pair and the Joe-gated three, and not by the "low-priority item that unblocks several" kind, which this cluster cannot produce.

**Corrections found at the step 3a review (claude-11, 2026-10-27, after the prediction, before the pass).** Two of my counts above are wrong; the prediction is left as written so the pass is scored against it. (1) The edges are 8 ticket-to-ticket and 8 external, not 7 and 9: `T-kangaroo-caching-and-compaction → T-kangaroo-v1-pouch` is ticket-to-ticket (zai-2 found this in conversion). (2) "Every ticket-to-ticket edge points at a done or superseded ticket" is false: `T-durable-queue-rename-and-tidies` (done) needs `T-car3-phase2-impl` and `T-mq7-unaddressable-caller`, both `:partial` (started). The cone claim still holds, for a different reason: none of the six ready items is the target of any edge. A done item whose needs are still open is either a stale edge (written before triage) or a status the index got wrong, and the pass should report it.

**Passes if:**
- [ ] the round trip into Plane's shape loses nothing Giraffe uses except the listed gaps;
- [ ] the pass reports at least one fact the Plane view does not show (an urgent item that cannot start; a low-priority item that unblocks several), with the edges that support it;
- [ ] Joe, reading the two lists side by side, finds the Giraffe order defensible as "what to do next".

Not a controlled comparison, as in ARGUE: it shows the computation fits a tracker's records and says something a board does not, not that teams would ship faster.

