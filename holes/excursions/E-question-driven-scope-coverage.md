# E-question-driven-scope-coverage — tagging driven by the questions, not the detector

**Excursion (method). Spun 2026-06-15, Joe↔claude-3, continuing
`E-queries-and-scopes.md`.** Turns the "remaining ct-anatomy work" from a
detector-accident coverage frontier into a **question-driven priority order**.

## HEAD (Joe, 2026-06-15, verbatim sense)

The query/scope lens is the right way to look at the remaining ct-anatomy work:
for content that *isn't* yet tagged, ask **what kinds of scopes are missing that
would let us answer reasonable questions about it.** MathOverflow gives the kinds
of questions mathematicians actually ask. And presumably most papers want to
**"preempt" those questions early in the exposition** — so the scopes that answer
the common questions should already be present, early, in a well-written paper.

## Why this is the fix for a known problem

The golden-walk-ledger's own verdict (futon6, Joe, 2026-06-12):

> "current coverage is ARBITRARY — the boundary between marked and unmarked
> tracks detector accidents (regex reach, span length, inline-vs-display, which
> lane saw the token first), not any property of the mathematics."

A question distribution supplies the **missing ordering principle**: tag next
what the question distribution most demands. Coverage stops being "whatever the
regex reached" and becomes "whatever answers the questions people ask."

## The method (a three-way join)

1. **Question phylogeny** — the distribution of question-*shapes* people ask.
   Source: MO `scopes.json` (`E-mo-query-phylogeny.md`), 555,509 scopes over
   95,321 Q&A, 22 `hx/type`s. A question's scopes ARE partial-hyperedge patterns
   (the **probe** side of `E-queries-and-scopes.md`).
2. **Answering-scope map** — for each question-shape, which paper-side scope
   answers it (the **store/environment** side). A `quant/universal` question is
   answered by a universally-quantified `states`/`quantifies` edge; a
   `constrain/such-that` by a `constrains` edge; a `bind/let` by a `bind`/`defines`.
3. **Current coverage** — what ct-anatomy actually emits (golden-graph SCHEMA's
   22 kinds). The **gap = (1)×(2) minus (3)**, weighted by question frequency, is
   the priority list.

## The join, on real vocabularies (2026-06-15)

MO question-shape (probe) → answering ct-anatomy scope (store) → emission today:

| MO `hx/type` | MO freq | answering ct kind | ct emission (golden) | verdict |
|---|---|---|---|---|
| `quant/universal` | **40.4%** | `quantifies` + `states` | 3× / 2× | **under-fed** |
| `constrain/such-that` | **19.7%** | `constrains` | 4× | **under-fed** |
| `bind/let` | 10.2% | `bind` / `defines` | 16× / 12× | covered |
| `bind/summation`+`integral`+`product` | ~14% | *(no analytic-binder kind)* | — | **absent (see caveat)** |
| `assume/explicit`+`assume/consider` | ~9% | `bind`(←assume) / `promises` | 5× / 1× | partial |
| `constrain/where` | 5.5% | `constrains` | 4× | under-fed |
| `quant/existential` | 0.4% | `quantifies` | 3× | under-fed |
| `env/{theorem,lemma,…}` | <1% | `states` / `goal` | 2× / 1× | partial |

**Headline:** ~60% of question structure is **quantify + constrain**, but
ct-anatomy's actual emission is dominated by the **definitional + proof-mechanics**
layer (`bind`/`defines`/`proof-step`). The kinds that answer the plurality of
questions (`quantifies`, `constrains`) *exist in the schema but are starved*. So
the highest-leverage remaining work is **reliably tagging universally-quantified
statements and their such-that constraints**, not more definition/proof markup.
Definitions (`bind/let`) are the one layer where coverage already matches demand —
"done enough."

This also explains the golden-walk frustration constructively: the under-tagged
content isn't random — it is disproportionately the **asserted, quantified facts**
of a paper (its theorems-as-universals and their side-conditions), which the
binder/proof-step detectors weren't built to catch (golden-walk W1's nine-word
sentence is exactly a "structures … are given by …" universal that "marks nil").

## Three sharpenings before this is a work plan

1. **Vocabulary crosswalk needed.** MO namespaces (`quant/…`, `bind/…`,
   `constrain/…`); ct-anatomy uses bare kinds (`quantifies`, `bind`,
   `constrains`). Same schema, different lexicon. The join table above is a
   hand-mapping; it should be made an explicit, reviewed crosswalk EDN so the
   coverage metric is reproducible, not eyeballed.
2. **Corpus-mismatch — filter MO to CT.** MO's phylogeny is over *all* math;
   ct-anatomy is *category theory*. The ~14% analytic-binder share (∑/∫/∏) is
   likely far smaller in CT specifically. The honest distribution filters MO to
   CT-relevant tags (`category-theory`, `homological-algebra`, `2-categories`,
   `higher-category-theory`, `ct.category-theory`…) and recomputes `hx/type`
   frequencies. `scopes.json` ⋈ `tags.json` (both on disk,
   `/home/joe/code/storage/mo-processed-gpu/`) is a clean stdlib computation.
3. **"Preempt" as a falsifiable metric.** A paper *preempts* a question-shape S
   if it carries, **early in the exposition**, a scope that answers S. Define
   **preemption coverage** = fraction of the (CT-filtered) question-shape mass
   answerable from a paper's first-N scopes. Measurable today on the 30 audited
   CT papers + the golden graphs. This operationalizes Joe's exposition intuition:
   good papers front-load the high-frequency answers; low preemption coverage
   flags either a hard paper or a tagging gap.

## Dogfood (2026-06-15): 24 answered questions on six tagged papers

Joe's "think about it on a few examples first" — four Codex agents (codex-1..4)
each asked **and** answered questions grounded directly in the six well-tagged
dp-demo CT papers (`futon6/data/showcases/ct-anatomy/dp-demo/`), posting (ask,
answer) pairs to ArSE (each agent answered only its own threads). 24 answered
questions, balanced across the six papers, each tagged `shape:<slug>` and marked
by source (TAGGED-MARKUP vs EXPOSITORY-PROSE).

**The finding refines the MO-derived prediction above.** The MO `hx/type` table
predicted *quantify/constrain* was the under-fed layer. On real CT papers the
deeper gap is different — and it shows up as **two orthogonal axes** the join
table conflated:

- **Scope-syntax axis** (MO `hx/type`: `quant/universal`, `bind/let`,
  `constrain/such-that`) — the binders/quantifiers *inside* a question's
  statement. This is what ct-anatomy's `quantifies`/`bind`/`constrains` capture.
- **Illocutionary axis** (the harvested `shape:` slugs) — *what kind of answer is
  sought*. NOT captured by any of the 22 scope kinds.

Shape × source over the 24:

| harvested shape | n | answerable from | current scope kind | verdict |
|---|---|---|---|---|
| **motivation** ("why X?") | **6** | prose, **6/6** | **none** | **★ biggest gap** |
| quantified-claim | 4 | tagged markup | `quantifies`+`states` | covered |
| construction | 4 | tagged markup | `built-from`+`defines` | covered |
| definition | 3 | tagged markup | `defines`/`bind` | covered |
| example | 3 | 2 tagged / 1 prose | `env/example` | mostly covered |
| universal-property | 2 | 1 tagged / 1 prose | partial | half-missing |
| expository-connection | 1 | prose | none | gap |
| transfer-property | 1 | prose | none | gap |

Clean break: **all 14 TAGGED-MARKUP questions are definitional/structural**
(definition, construction, quantified-claim, example) — the existing vocabulary
answers them. **All 6 `motivation` questions came from prose with no markup
support** and are the single most common shape (25%). This *confirms and sharpens*
the "preempt" hypothesis: the papers DO preempt the "why" questions in their prose
(every one was answerable), but our tagging captures none of that layer.

**Missing scope kinds, ranked by harvested demand:**
1. **`motivation`/`rationale`/`telos`** (6) — links a construction/definition to
   the problem it solves or the property it is designed to have; lives entirely in
   expository prose. The single highest-value missing scope.
2. **`universal-property`/`characterizes`** (2) — the CT-native "why this
   definition"; half-captured today (1 from a tagged coalgebra statement, 1 from
   prose).
3. **cross-structure `connection`/`transfer`** (2) — "this object relates to that
   outside structure"; prose-only, no scope kind.

ArSE harvest: 24 threads tagged `ct-anatomy-coverage`,
`/home/joe/code/storage/arse/entities.json`, all answered.

## Dense dogfood (2026-06-15): close reading of six paper openings

Scaling the sparse seed: codex-1..4 close-read the first ~250 body lines (after
the abstract) of all six dp-demo papers, turning each sentence into one or more
**(scope, query)** records carrying BOTH axes — `hx/type` (scope-syntax) and
`shape` (illocutionary) — holes marked `?`, inventing `NEW:<kind>` wherever the
22-vocabulary failed. **427 records over ~1,266 body lines** (~1 per 3 lines).
Files: `close-reading/<id>.close-reading.md`.

**Per-paper fingerprint — papers split into two genres:**

| paper | rec | dens/100ln | genre + top hx / top shape | NEW |
|---|---|---|---|---|
| 0807.1872 | 111 | 43.7 | formal: states/defines/logical · definition/assertion/implication | 2 |
| 0905.0595 | 88 | 48.1 | formal: states/defines/proof-step · proof/assertion | 2 |
| 1005.2653 | 26 | 50.0 | formal/terse: states/constrains · assertion | 3 |
| 1012.1220 | 74 | 29.7 | expository: defines · construction/relation | 3 |
| 0801.2567 | 70 | 25.5 | expository: construction/defines · construction/connection | 6 |
| 0711.1761 | 60 | 23.7 | expository: states/defines/NEW:rationale · definition/motivation | 3 |

**The inversion (the "different shapes" result, quantified):** formal-genre
openings are dense in formal scopes and the existing vocabulary fits (few NEW);
expository-genre openings are *less* dense per line yet generate nearly all the
NEW kinds. **The vocabulary gap is genre-specific — it lives in the why/program/
connection prose, not in the definition-theorem-proof body.**

**Pooled distributions.** Scope-syntax axis dominated by `states`(103) +
`defines`(100) [≈47%], then `cites`(30), `constrains`(21), `logical`(21),
`proof-step`(20), `construction`(16), `quantifies`(12). Illocutionary axis:
`definition`(79), `construction`(62), `assertion`(61), `relation`(35),
`proof`(27), `motivation`(24), `quantified-claim`(21), `implication`(20),
`connection`(17), `example`(15), `roadmap`(11).

**The gap, quantified: 44/427 = 10%** of records needed a `NEW:` hx-kind, and
independent readings of different papers reinvented the SAME families:

| proposed scope kind | evidence (hx + shape) | recurs in |
|---|---|---|
| **`rationale`/`telos`** — links a construct to the problem/goal it serves | 17 hx + 24 `motivation` shapes | 5 of 6 |
| **`connection`/`transfer`** — relates/transfers to an outside structure | 9 hx + 23 shapes | all expository |
| **`computes-invariant`/`calculation`** — computational step yielding a value | 5 hx + 7 shapes | 2 |
| **`open-problem`/`status`** — declares an unresolved question / literature gap | 7 across axes (+`literature-gap`) | 3 |
| **`universal-property`/`characterizes`** (+ `obstructs`, `interprets-diagrammatically`) | low count, CT-central | 2–3 |

This supersedes the all-math MO prediction with measured CT evidence: the
existing vocabulary already covers ~90% of the scope-syntax axis; the durable gap
is the **illocutionary "why/connect" layer** (`rationale`/`telos` first), and it
is concentrated in expository prose — exactly the content current detectors skip.

## Minting run (2026-06-15): 196 papers, 4 agents, a saturated vocabulary

Built the expository-scope-detector and ran it over 196 of the gh200 top-cited
math.CT papers. Components (all reusable): `futon6/scripts/expository_region_extract.py`
(leaf-section + inflight-exposition regions), the seed `expository-scope-hierarchy.edn`,
and `futon6/scripts/consolidate_scope_votes.py` (vote tally + mint-pressure
+ discovery curve). 4 agents proposed scopes binned into the hierarchy; the
consolidation applied the ≥5-papers/≥2-agents mint rule.

**5 expository sub-kinds minted** (each 39–83 papers, ≥2 agents):
`connection/application-domain` (68p), `connection/example-source` (77p),
`connection/literature-gap/terminology-origin` (83p),
`obstruction/counterexample-status` (39p),
`rationale/telos/organization-roadmap` (70p).

**Saturation: decisive.** Discovery curve — all 5 minted within the first **24
papers**; papers 25–196 minted **zero** new kinds. So ~24 papers sufficed; 196
confirmed sufficiency with margin. (Answers the open "how many papers" question.)

**The mint-pressure mechanism proved itself under stress.** One agent (codex-3)
over-generated badly (28k proposals; an idiosyncratic `context-setting` drew
**14,324 votes**). Because only one agent used it, the ≥2-agent rule **quarantined
it to its parent (`rationale/telos`) and refused to mint it** — single-agent
runaway could not pollute the vocabulary. This is the "doesn't accept every vote"
design working as intended.

**Caveats:** (1) the coverage % (34.72% overall) is contaminated by codex-3's
over-generation (~40 papers >100%, more proposals than sentences) — an honest
figure needs a dedup/cap pass; (2) naming divergence across agents split some
concept support (codex-4's `connection/application` resolved separately from the
minted `connection/application-domain`) — concept-normalization would consolidate,
not change, the five. **Methodological finding:** LLM scope-annotation is
high-variance in *volume and naming*; the cross-agent **mint signal is robust**
where raw vote counts and coverage are not. Report:
`close-reading/consolidation-report.{json,md}`.

## First buildable rung (data-backed, bounded)

**Compute the CT-filtered MO question phylogeny** (sharpening #2): join
`scopes.json` × `tags.json`, restrict to CT tags, emit the `hx/type` frequency
table. Deterministic, stdlib, no mining. Output is the *real* demand vector that
weights the gap table — replacing the all-math frequencies above with CT ones.
Then re-rank the join table by CT-weighted demand: that ranked gap **is** the
prioritized remaining-work list for ct-anatomy. (Companion: write the crosswalk
EDN from #1 so the re-rank is reproducible.)

Bigger arc (gated on the rung): measure preemption coverage on the 30 audited CT
papers; the papers/sections with low coverage are where the missing scopes live.

## Relations

- **`E-queries-and-scopes.md`** — the probe/store duality this method rides on
  (question = probe-scope; paper = environment-scope; answering = filling).
- **`E-mo-query-phylogeny.md`** — the question-distribution source; this excursion
  is its inward turn (use MO to *prioritize our own tagging*, not just to
  benchmark QA).
- **`M-typed-holes.md`** — projection #5; an untagged-but-needed scope is a
  `:hungry-for` typed hole the question distribution says to feed first.
- **futon6 `golden-walk-ledger.md`** (R1–R15, the "coverage is arbitrary"
  verdict) and **`golden-graphs/SCHEMA.md`** (the 22 emitted kinds) — the
  coverage side of the join.
- **futon6 `anatomy-v0-loss-backlog.md`** — the detector-side gap backlog
  (C1–C5); this excursion says which of those to fix *first* (the ones feeding
  `quantifies`/`constrains`).
- **MO corpus** — `/home/joe/code/storage/mo-processed-gpu/{scopes,tags}.json`.
