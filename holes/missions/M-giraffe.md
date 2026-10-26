# M-giraffe — start from the data structure: a work pool the War Machine can read

**Date:** 2026-10-25
**Status:** OPEN — DERIVE (from 2026-10-26; HEAD, IDENTIFY and MAP exits recorded in §MAP close-out)
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
- [ ] **Invariant: a cycle policy before anything reads the graph** — refuse, or report as a typed fact beside the choice (house convention, §Overlap guard vs record). Choose one.
- [ ] **Invariant: an item's own Missing column is never counted as its dependency.**
- [ ] **State: dated transitions, `dropped` with a reason** (obsolete, duplicate, declined, impossible, superseded), roles source / attester / doer / accountable, source system + external id, revision number (tracker survey).
- [ ] **Data flow:** holes/ adapter → item records (location open: substrate, files, or both) → read by a tool for Joe first (the overview groups, with what waits on what), and by the selector only after the defect list closes.
- [ ] **Open choice: a hard need as threshold 1 of a degree, or a separate edge kind** (HTM segments; H4 finding 1: a threshold term alone would rank M-self-improvement-loop first).
- [ ] **Open choice: the work-package ranking** — K2' prunes to the maximal antichain, fit to attested wants ranks within it, both scores recorded, packages marked *imagined* until a click scores them (§Lynn, proposed reading).
- [ ] **Wiring diagram:** triggers 2 and 3 of the lifecycle's list hold (adapter writes what the selector reads; it crosses futon3c/futon2), so one is drawn before VERIFY.
- [ ] **Fidelity contract (GF):** greenfield over the documents; declared commitments are the candidate-field list and the tracker survey's "worth taking" list.
