# M-象-cascade

Status: DERIVE draft (reopened 2026-11-02 by Joe; claude-16 working through the lifecycle. The lab ran past IDENTIFY on 2026-09-30 without this line changing.)

**Type**: Mission
**Lifecycle**: HEAD (2026-09-28, operator turns verbatim) → IDENTIFY (drafted 2026-09-28). Phases per `futon4/holes/mission-lifecycle.md`.
**Owner**: claude-17
**Builder**: codex-5, by bell and park, reviewed by claude-17

---

## HEAD

**Operator-voice anchor** (Joe, 2026-09-28, `*claude-repl:claude-17*`, verbatim, three turns in order):

> BTW only very loosely related to this, I'm a bit concerned that the current
> 象 annotator is possibly fairly frequently finding turns which it marks as
> only having holes, and it is able to come up with proposed fillers, but no
> one is moving those in to fill the slots.  I guess the connection is that we
> can't really UNDO something until we can DO something. […] What are the
> actual semantics?

> Well, I think this is why we need a coalescing phase that happens on an
> ongoing basis, not just a one-off.  The relationship to UNDO is interesting
> b/c perhaps two patterns would be merged that I or someone else later decide
> need to be teased back apart.  Basically I don't want to have to think about
> the mechanics of this aspect of things in usual everyday use where I just
> want to be able tod interact normally, but if we are going to actually get
> the system to do anything useful, we need to have a continually maintained
> cascade that does the coalescing and the hierarchical search.  Actually this
> reminds me of what I understand about Backus-Naur Form (BNF) lookup.  So,
> common patterns should be found quickly, more esoteric ones found via a
> direct path from those.

> OK, so regarding "there's no step that turns IF/THEN prose into these rules.
> The only text compiler, `interpret-pattern`, works on keywords, and nothing
> in selection uses it" ... that would be bad if we were not somehow building
> the missing part.  If 象 interpretations can be read as semantic parses, then
> we should be able to provide a background corpus of IF-THEN interpretations
> drawing on my turns, yes?

> Yes, let's open M-象-cascade following futon4/holes/mission-lifecycle.md, at
> IDENTIFY.  The gap is clear, and I quoted you above to identify it.

**Provenance:** operator turns in the claude-17 REPL on 2026-09-28, during
M-象-2000's INSTANTIATE (P10). The direction and the pattern-semantics survey
are also recorded in `M-象-2000.md` (390d4249, b21a6314).

**HEAD exit:** The anchor turns are recorded verbatim. Joe opened the mission
at IDENTIFY, which skips a separate HEAD review.

---

## IDENTIFY

### The gap (Joe quoted this to identify it)

> There's no step that turns IF/THEN prose into these rules. The only text
> compiler, `interpret-pattern`, works on keywords, and nothing in selection
> uses it.

The War Machine gives a pattern a meaning only through a rule an agent writes
by hand, `{:guard {:needs #{…} :forbids #{…}} :produces #{…}}`, over facts it
can observe. It picks among cascades using those rules, and afterwards checks
whether the produced facts appeared. 象 labels every operator turn with a move
and a pattern, but nothing turns those labels into such rules. So the labels
are only recorded, and the pattern library does not grow from them.

### Motivation: measured on 2026-09-28

- **Holes pile up and none close.** `xlate.py census` over 572 analysed turns:
  - 1,633 fragments have no pattern;
  - about 480 proposed patterns;
  - 13 of them RIPE (proposed from three or more turns, under
    `cascade-construction/lift-when-three-align`);
  - one of the 13 has a draft;
  - none has been added to the library.
- **Proposals multiply instead of merging.** `orchestration/consent-gate` is
  cited 34 times and has no children in the library. Near-duplicate go-ahead
  proposals sit under it separately:

  | Proposal | Times proposed |
  |---|---|
  | `editing/approve-as-gate` | 9 |
  | `operator/ratify-the-recommendation-inline` | 7 |
  | `orchestration/lightweight-ack-advance` | 4 |
  | `operator/curt-ok-before-the-real-report` | 4 |
  | `operator/approve-then-ask` | 3 |
  | `operator/in-that-case-proceed` | 2 |
  | `operator/yes-push-it` | 1 |

  A move proposed under five names reaches three under none of them.
- **Patterns carry no checkable meaning.** Of 1,227 flexiargs, 0 carry
  predictions (futon2 `holes/NOTE-pattern-as-production-rule-and-Q.md`). WM
  cascade rules are hand-written per target (futon2
  `resources/wm/cascade-sources/*.edn`).
- **Retrieval is flat.** `xlate.py find` is BM25 over about 1,400 patterns,
  with measured recall@5 of about 0.29.
- **Joe's example.** `turn-Y0IzMQ` ("Yes, please push it", claude-19,
  03:49:13Z) was labelled `approve`. It became a hole with the proposal
  `operator/yes-push-it` under parent `consent-gate`, and nothing followed.

### Theoretical anchoring

- **The WM cascade.** Its rule shape, the first enabled guard, and learning θ
  from observed produced facts (futon2 `wm/cascade_decision.clj`,
  `cascade_model_manifest.clj`, `accepted_increment.clj`; Lean
  `DarkTower/WarMachine/CascadeTransition.lean`) are the target this mission
  compiles into. The mission does not change them.
- **`cascade-construction/lift-when-three-align`** decides when a proposal
  should be added to the library.
- **Joe's BNF analogy.** Lookup descends from common parents to specific
  children, the way a grammar expands a nonterminal. A child's guard only
  narrows its parent's, so the path from the parent is also the search.
- **M-象-2000's patterns:**
  - 象/释义非授 (an interpretation is never authority): a rule induced from
    象's readings is a hypothesis until it predicts what follows on turns it
    was not drawn from.
  - 象/收回亦是行 (a withdrawal is itself an act, and nothing is deleted): a
    split withdraws a merge.
  - 象/名分有据 (every act records its authority): an addition, a merge or a
    split is a stamped act with a grant.

### Scope

In:
1. **The corpus.** For each analysed operator turn, a triple of pre-facts
   (the situation the turn answered), the move (象's intent and target), and
   post-facts (what happened afterwards). Facts come from records: the
   evidence store, commits, bells, parks and M-象-2000's act records. Any
   weaker proxy is named as a proxy.
2. **Rule induction.** For a pattern or proposal family with three or more
   aligned turns, derive a guard/produces rule from the triples. It is stored
   next to the pattern, and tested on held-out turns.
3. **Ongoing coalescing.** Proposals are compared as they arrive, with no
   operator involvement. The resulting additions, merges and splits are
   stamped acts, and they are reversible. Holes that an added pattern fills
   are linked back.
4. **Hierarchical lookup.** Lookup follows parent → child links, compared
   with flat BM25 on held-out turns.

Out (deferred):
- executing a pattern's THEN;
- changing the WM cascade decision;
- Lean statements of induced rules;
- rewriting flexiarg prose;
- reading agent turns (Decision P19: 象 reads operator turns only).

### Completion criteria (testable)

1. **Corpus.** Every analysed operator turn has a stored triple, or a typed
   reason why not. Proxies are counted separately.
2. **Induced rules.** At least one family (the go-ahead family under
   `consent-gate` first) has a guard/produces rule derived automatically,
   with its turns listed. On held-out turns it predicts the post-facts better
   than a baseline declared before the measurement.
3. **Coalescing runs by itself.**
   - A new proposal is compared with existing proposals and the library
     without anyone starting it.
   - Merges and splits are stamped acts, and a split restores the separate
     citations.
   - Joe does nothing in everyday use.
4. **Holes close.** A pattern added by this path links back to the turns
   whose holes it fills, and the census hole count falls by those turns.
5. **Lookup.** Parent → child lookup is measured against flat BM25's recall@5
   of about 0.29 on the same held-out turns, and the result is reported
   whichever way it comes out.
6. **The machine can load it.** One induced rule is admitted by the War
   Machine's loader and appears as a candidate in a plan-only run
   (`FUTON_WM_FLIGHT=plan`, futon2 `scripts/wm_scheduled_run.clj`). There is
   no flight and no change to the cascade decision. (Agreed by Joe
   2026-09-28 19:30Z, proposed by claude-8 in futon2
   `holes/labs/wm-contract/NOTE-xiang-cascade-seam.md`.)

#### What criterion 6 requires of this mission (from claude-8's note)

The loader is futon2 `aif/interpretation_evidence.clj` `interpretations!`,
schema `:wm/interpreted-pattern-set-v1`. It needs:
- `:clauses` citing IF, HOWEVER and THEN in the pattern's own library text,
  so a rule can only be loaded for a pattern that is in the library;
- a guard and an effect over declared facts, each fact with a locator the
  machine can evaluate;
- a stored `:sha256`, because the rule is loaded as stored and not re-derived.

Decisions taken here as owner:
- **The rule for criterion 6 need not come from the go-ahead family.** A
  go-ahead guard needs an operator turn, and an unattended flight has none.
  Criterion 2 still starts with the go-ahead family because it has the most
  turns. Criterion 6 uses a family whose operator turn corrects agent work,
  so the guard is over facts about the work.
- **A merged pattern id stays resolvable as an alias.** WM receipts cite
  pattern ids and pin the library by hash, so a merge must not make an old
  receipt unresolvable. This is part of criterion 3.
- **A rule induced from a proxy fact carries the proxy mark** into the
  record the machine loads.
- **Owed to claude-8 at DERIVE:** the fields of the induced-rule record (its
  turns, held-out hit and miss counts, the declared baseline), so that the
  machine side can add an authority value for induced rules in place of
  `:documented-interpretation`; and, for each fact the chosen family uses,
  where it is read from, so that claude-8 can decide whether an
  evidence-store locator class is needed.

### Relationship to other missions

- **Depends on M-象-2000:**
  - act stamps, grants and withdrawal (built);
  - P11 offer and agreement records (not built), which would make "an offer
    was pending" a fact rather than a proxy.
- **Feeds the War Machine** with rule readings that a cascade could use
  instead of hand-written ones. Criterion 6 tests this; claude-8 holds the
  receiving side.
- **Related:** `problems/pattern-genesis-from-evidence-bearing-holes`;
  futon2 `holes/NOTE-pattern-as-production-rule-and-Q.md`.

### Source material

- `~/.emacs-graph/session-turn-analysis/*.json`, `*.analysis.json` and
  `*.candidates.json` (572 analysed turns);
- `futon3c/scripts/xlate.py` (find, census);
- `futon3c/emacs/session-turn-analysis.el` (the 象 brief);
- the futon1b evidence store (:7073);
- futon2 `wm/cascade_*`, `accepted_increment.clj` and
  `resources/wm/cascade-sources/`;
- `futon3/library/` (about 1,400 patterns);
- `futon3c/holes/missions/M-象-2000.md` (DERIVE-2, P10 records).

### Owner and dependencies

- **Owner:** claude-17 until 2026-09-30; claude-16 from 2026-11-02 (Joe's direction to work it through).
- **Builder:** to be chosen at INSTANTIATE; the 2026-09-28 note named codex-5.
- **Repos:** futon3c (corpus and coalescing), futon3 (library additions),
  futon2 (read-only: the rule shape), futon1b (read-only: evidence).

**IDENTIFY exit:** awaiting Joe's reading of the gap and scope.

**Addendum, Joe 2026-11-02 (voice):** "I don't have ... a way to look or understand the
overall pattern cascade that's supposed to be mined and so we keep going around and around
looking at what happens in a given turn". Then: "we should work through M-象-cascade using
the mission-lifecycle phases". Proposed criterion 7, for his acceptance: **Inspectable.** Joe
can open a page showing the global cascade, what was created, and what was validated.

## MAP (claude-16, 2026-11-02)

Facts only, per the lifecycle. Probes are read-only: `~/notes/pattern-global/map_probe.py`,
`measure.py`, `build_page.py`; first view at
https://zone.hyperreal.enterprises/wip/pattern-global.html.

### Survey questions

| # | Question | Answer |
|---|---|---|
| Q1 | How far did the 2026-09-30 lab get? | D0 fact sources, C1 corpus of 48 turns, two rules induced and tested, C6 live-gap trace. All in `holes/labs/M-象-cascade/`. No later mission commit |
| Q2 | Did an induced rule pass? | No. Go-ahead: 20 of 22, p = 0.0205 against a threshold of 0.0167, `not-better`. Live-gap: 6 of 9 against a matched 177 of 253, `too-few-to-judge`. A commit was the wrong post fact for live-gap |
| Q3 | Are closures independent? | Of 12 live-gap reports: 0 independent records, 4 agent assertions, 2 operator confirmations (1 doubtful), 6 none found |
| Q4 | How many holes and proposals now? | 4,254 analysed turns; 12,189 fragments, 9,431 with no pattern (77%); 2,854 citations of 490 patterns; 3,828 distinct proposals over 4,207 occurrences; 20 ripe at three or more; 0 ripe proposals are in the library |
| Q5 | Does coalescing exist? | No code found. Name variants stand apart, such as `operator/port-via-breadcrumbs` and `operator/port-by-breadcrumbs` |
| Q6 | Does the library support descent? | Not in the library: 0 of 1,527 flexiargs declare a parent field. But 3,676 of 3,834 proposals name a library parent, across 637 patterns, so a tree exists on the proposal side. 646 have `@references`, untyped. 33 carry a guard, needs, produces or predicts field |
| Q7 | Does a global structure show in the labels? | No recurring pairs detected. The first test failed its positive control and is withdrawn. The second (`holes/labs/M-象-cascade/aggregate/DECLARATION.md`) detected planted same-turn pairs, and found 3 against 1.1 in names, p = 0.09. It has no planted control for sequences |
| Q8 | What can Joe see now? | Stepper and 象 feed, turn by turn. Cascade Live: mission lineage, clusters, 1,939 mission-pattern citations; not mined from turns. Pattern-global page: one snapshot, not maintained |
| Q9 | Is the machine side ready? | Loader exists (`interpretation_evidence.clj`). Rule records exist as `:xiang-cascade/induced-rule-v2`. No rule qualifies, and WM dispatches are on Joe's hold. Seam contact is now claude-1 |

### Ready and missing

| Ready | Missing |
|---|---|
| 象 readings with fragments, intents, pattern refs and rejections | A way to treat variant names as one move |
| Triple builder, induction and test (`xiang_cascade_corpus.py`, `xiang_cascade_induce.py`) with tests | Corpus beyond 48 turns |
| Stamped acts, grants, withdrawal (M-象-2000) | Stamped merge and split acts for proposals; aliases |
| `lift-when-three-align` and its census | An admission path into the library: 0 so far |
| BM25 lookup | Parent links in the library; hierarchical lookup |
| One global snapshot page | A maintained global view, Joe's criterion 7 |
| Post facts: commits, parks | Post fact recording activation or observed use |

### Surprises

1. This document stood at IDENTIFY while the lab tested six steps. The earlier statement in
   chat that nothing had happened on this mission was wrong; only the status line had not moved.
2. `xlate.py census` crashes on invalid candidates files: 4 today, so the tool named in this
   mission has no current output.
3. Scatter is real, but near-duplication explains little of it. 3,828 names for 4,207 occurrences,
   and merging at cosine 0.70 reduces 3,834 proposals only to 3,722.
4. The analysis corpus now has 4,254 readings, against 572 when this mission opened.
5. Labellers recorded 10,599 pattern rejections, which this mission does not use.

**MAP exit:** every question above has an answer, and the table is complete.

## DERIVE (draft, claude-16, 2026-11-02)

**Joe, voice, 2026-11-02:** "we absolutely need a type of aggregator or reduction so that when
things are added, they get assimilated into the map and this is what the minus 40 Memory system
and our discussion of three halves blends should be helping with". Two very similar proposals
should be noticed and amalgamated "into their blend", the others deleted or deprioritised.
Library patterns are "somewhat more likely to have been attested than brand new agent
creations", but may lack coverage, so "we're not trying to validate ... those patterns", and
rewriting or compressing them is also relevant.

Evidence so far (`holes/labs/M-象-cascade/aggregate/`, read-only):

- At cosine 0.70, 23 of 30 sampled merged pairs were the same move on one reader's judgement
  (claude-16): 77%, below the declared 80%. Those merges are not to be applied. Embedding alone
  is not enough.
- Known related go-ahead proposals score 0.41 to 0.57, inside the noise for random pairs
  (q99 0.47), so families cannot be found by embedding alone.
- Declared parents beat chance: median cosine 0.32 against 0.17, yet rank 63 among 1,207, with
  24% in the top ten. They give a first tree, not a verified one.
- Grouping did not produce recurrence, so merging does not by itself create a co-occurrence
  structure. The aggregator is justified by assimilation, not by that test.

### D0. Existing patterns first (Joe, voice, 2026-11-02)

"if we can't produce a reasonable-looking cascade out of the ones that are already sitting on the
file system, Mining new ones and assimilating them is going to be fairly challenging". So order
changes: one cascade of existing patterns comes first, tied to the operator turns they were
invoked against, then retrieval, then D1 to D3 on proposals. Findings
(`~/notes/pattern-global/`):

- MAP missed `~/notes/experiments/2026-10-24-session-cascade/graph.json`: 1,456 patterns, 6,316
  edges, largest component 774 without weak edges. `used-together` has 0 edges: use is unrecorded.
- 馬 read 454 turns: 2,213 picks, 1,815 void, 369 partial, 29 occurrence.
- 176 patterns attested at least partly; 93 of them connect through authored and citation edges.
  This is the first candidate cascade.
- Attested at least partly: embedding 10% of 1,025 picks, 驴 24% of 1,078.
- Shown picks were 89% void. The shown pick is usually embedding, from a query of 200 characters of
  the previous user message plus 200 of its reply (`dev.clj:1001-1004`), so it answers the
  previous exchange. Only ids reach the agent. 驴 reads the current turn but finishes after the
  render, in about 10 s.
- `m40` alternation shows 驴/m40 on only some turns.

**Joe, 2026-11-02:** the aim is partly to make the animals' work amenable to a fast classical path,
as 小象 is; 馬 can come last and be delayed, since it has the whole turn. What matters is that what
the mining loop learns can be taken on board in the prioritisation of work. The way of working
should itself be a pattern cascade, shown in Cascade Live; `agent/eat-your-own-tail` belongs early
in it. Draft: `~/notes/pattern-global/ways-cascade-draft.md`, over claude-2's
`holes/excursions/ways-of-working.json`.

**Joe, 2026-11-02:** feedback in the chaos-theoretic sense is what `agent/eat-your-own-tail` is
about, and "Possibly what's been going on is that I've been holding a lot of the learning loop."
Reading: cluster C looks thin because its loop is carried by Joe and is unrecorded as such. In the
analysed corpus, 2,401 of 13,234 fragments (18%) are corrections by intent (report-problem,
redirect, disagree, constrain, qualify). Test to run: how often does a correction repeat an
earlier one? Falling repeats would show the loop moving into machinery, step 4 of the pattern.

**Joe, 2026-11-02:** the synthetic view is "a bit like a skeletal core and the data-based view should
provide a more organism-level articulation". First articulation: `~/notes/pattern-global/articulate.py`,
reported in `design-cascade.md`; leave-one-out 23 of 52 against 25% majority.

### D1. Assimilate at intake

Each new proposal gets one of four stamped outcomes against the library and open proposals:
`restates` (cite the existing pattern, keep the turn link), `blends` (joins or forms a blend),
`child` (a variant under a parent), `new` (stays a candidate). Reuse minus40's route: near
candidates, then `judge.py`-style verdicts with verbatim quotes, so a merge rests on quoted text
and not on a score. Alternative rejected: score threshold alone, 77% precision.

### D2. A blend is a stamped act, not a deletion

Record the inputs, what they share (the generic space, usually the parent), the blend text, and
what each input loses. This is the 3/2-blend carrier's partial-map form. Inputs stay as aliases
and are lowered in priority, never erased (象/收回亦是行). A split restores the citations. The
carrier's quality order is unproved for this use: `NOTE-three-halves-pushout-discovery` records
that it accepts structure-dropping blends, so judged quality comes first.

### D3. The library is a prior, not a verdict

Library patterns start with more weight, and turn attestation can move it either way. A large
family whose children keep restating the same exception is a signal to revise or compress the
parent. Revision is proposed as a stamped act for Joe's page, not done automatically.

### D4. The global object is the tree first

What exists is a parent tree with counts; a recurrence network does not show. Criterion 7's page
starts from families: size, what was merged, what was admitted, what was revised, with use
evidence beside each.

### D5. Open before ARGUE

- Which model judges pairs: WM dispatch is on Joe's hold, and Zai is held too.
- Whether "created and validated" on the page should mean ripe by count, or tested on held-out turns.
- Fix `xlate.py census` for invalid candidates files.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-5, high confidence); not yet confirmed by the author._

- [ ] Corpus: every analysed turn has a triple or typed reason (proxies counted); one induced rule beats a pre-declared baseline on held-out turns
- [ ] Coalescing unattended: auto-compare proposals; stamped merges/splits; split restores citations; Joe does nothing daily
- [ ] Holes close: an added pattern links back to the turns it fills; census hole count falls accordingly
- [ ] Lookup: parent-child measured vs flat BM25 recall@5 (~0.29), reported either way
- [ ] Machine loads it: one induced rule admitted by the WM loader, appearing in a plan-only run (FUTON_WM_FLIGHT=plan)
