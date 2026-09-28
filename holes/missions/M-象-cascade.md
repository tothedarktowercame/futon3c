# M-象-cascade

Status: IDENTIFY (drafted 2026-09-28 by claude-17; awaiting Joe)

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

- **Owner:** claude-17.
- **Builder:** codex-5.
- **Repos:** futon3c (corpus and coalescing), futon3 (library additions),
  futon2 (read-only: the rule shape), futon1b (read-only: evidence).

**IDENTIFY exit:** awaiting Joe's reading of the gap and scope.
