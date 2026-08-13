# Pattern / memory retrieval — architecture as-is and as-it-should-be

**claude-2, 2026-08-13, at Joe's instruction ("what we need now is not a
fresh reviewer but an architecture diagram"). Every count below was
measured today, not recalled; the commands are in §6 so anyone can
re-run them.**

---

## 1. The as-is, honestly

```
                     ┌───────────────────────────────────────────┐
                     │  MINING LANE (slices 1-3, case-1 pilot)   │
                     │  marks → clusters → flexiarg candidates   │
                     └──────────────────┬────────────────────────┘
                                        │ authors
                                        ▼
   ┌────────────────────────────────────────────────────────────────┐
   │  WORLD A — FILES (futon3, a git repo)                          │
   │                                                                │
   │   library/<family>/<name>.flexiarg      ~1078 files, 40+ fams  │
   │        ├── math-informal/        45 ──┐  curated math boundary │
   │        ├── math-informal-CT/          │                        │
   │        ├── math-formalization/   ←────┼── the mined patterns   │
   │        ├── math-strategy/        ←────┤   landed HERE          │
   │        ├── process-coherence/    ←────┘                        │
   │        ├── agency/ aif/ baldwin/ war-room/ iiching/ …          │
   │        └── (essays, roadmaps, exotype boilerplate)             │
   │                                                                │
   │   resources/sigils/patterns-index.tsv   1358 rows              │
   │        ├── was a SYMLINK → storage/futon3/… (now orphaned      │
   │        │   regular file, 1355 rows, stale since 2026-08-10)    │
   │        └── data/notions/… is a compat symlink to the live one  │
   └───────────────────────┬────────────────────────────────────────┘
                           │ read by
                           ▼
   ┌────────────────────────────────────────────────────────────────┐
   │  RETRIEVER 1 — Tier-0 hotword  (futon6/scripts/cas_select.py)  │
   │    score = |hits| + |hits|/|hotwords|                          │
   │    NO stopword list.  NO IDF.  NO specificity weighting.       │
   │    default pool: math-informal* only .......... 45 patterns    │
   │    whole-index pool (added today, ad8ec02) ... 1334 patterns   │
   └────────────────────────────────────────────────────────────────┘

   ┌────────────────────────────────────────────────────────────────┐
   │  WORLD B — STORE (futon1b, XTDB2, :7073, migration-store-21)   │
   │                                                                │
   │   140,333 evidence rows                                        │
   │   :pattern/library entities   ── agency/loud-failure           │
   │                                  math-informal/transport-…     │
   │   coarse pattern buckets      ── math/holomorphic-disk-api     │
   │                                  math/missing-dependency-proto │
   │   memories (e-*) ──hyperedge──► pattern + problem + receipts   │
   └───────────────────────┬────────────────────────────────────────┘
                           │ read by
                           ▼
   ┌────────────────────────────────────────────────────────────────┐
   │  RETRIEVER 2 — store recall ladder                             │
   │    futon3c/src/futon3c/dispatch_with_recall.clj                │
   │    stopword family (:112), normalized + stopword-filtered +    │
   │    IDF-ranked vocabulary (:339), problem-idf anchors           │
   │    → this is the one that LEARNED the lexical lessons          │
   └────────────────────────────────────────────────────────────────┘
```

**The two worlds do not talk.** `math-formalization/transport-across-an-instance-diamond`
returns **404** from `/api/alpha/entity/`. Every pattern the mining
campaign has produced lives only in World A. The retriever with stopwords
and IDF can only see World B. The retriever that can see the mined
patterns is the one with no lexical hygiene at all.

## 2. Where today's five defects sit on that picture

| # | Defect | Where |
|---|---|---|
| 1 | Six mined patterns unreachable by Tier-0 | filed outside `math-informal*`, the only indexed family |
| 2 | Ranking dominated by function words (`the` in 218 hot sets; 257 `iiching/exotype-NNN` rows share one 7-token boilerplate) | Retriever 1 has no stopword/IDF stage |
| 3 | 7 of 12 provenance queries share no specific token with their target | patterns indexed in Lean identifiers, queried in runner English |
| 4 | 24 index rows silently dropped; 16 distinct patterns unreachable (`f3/p0` = both "Portal Query Layer" and "MUSN Coordination Substrate") | TSV has duplicate qualified names; loader is last-write-wins |
| 5 | Three-way index drift, symlink replaced by a regular file | a TSV is not a database |

Defects 4 and 5 are **artifacts of the substrate choice**, not bugs to
fix one at a time. A TSV keyed by a string that isn't unique, edited by
append, reachable by three paths, is going to keep producing this class.

## 3. The degeneration, named

Two mistakes compound, and the second is mine.

**Mistake 1 — mined math patterns were filed outside the math boundary.**
`cas_select`'s scoping to `math-informal*` is a *curation boundary*, and
its own source comment says so. The mining lane authored math patterns
into `math-formalization/` and `math-strategy/`, which the boundary does
not include. Nobody noticed because the mandatory per-slice retrievability
check was never run.

**Mistake 2 — I read the boundary as the bug and deleted it.** Widening
the pool to 1334 documents dragged essays, roadmaps, `throat-clearing-close`,
and 257 rows of exotype boilerplate into a *mathematical* retrieval
problem. That is why a query about trivial kernels and preimages returns
"MUSN Coordination Substrate". **Joe is right that we should not be
hitting that string at this point, and the reason we are is that I widened
the pool instead of moving six files.**

The corrected reading: **fork (a) was the load-bearing half all along.**
The boundary should stay; the mined patterns should sit inside it.

## 4. The as-should-be

```
   ┌────────────────────────────────────────────────────────────────┐
   │  ONE SUBSTRATE — XTDB (futon1b :7073)                          │
   │                                                                │
   │   :pattern/library  {:id        stable uuid  ← NOT the name    │
   │                      :qualified "math-formalization/…"         │
   │                      :family    "math-formalization"           │
   │                      :domain    :mathematics | :coordination…  │
   │                      :grade     :principle|:technique|:snippet │
   │                      :title :hotwords :conclusion :however     │
   │                      :provenance {:slice :marks :transcripts}  │
   │                      :status    :staged|:reviewed|:assayed}    │
   │                                                                │
   │   bitemporal → no drift, no second copy, no symlink            │
   │   uuid-keyed  → f3/p0 collisions become impossible             │
   │   memories ──hyperedge──► pattern (already works this way)     │
   └───────────────┬────────────────────────────────────────────────┘
                   │
                   ▼   ONE retrieval stack, two legs, shared lexis
   ┌────────────────────────────────────────────────────────────────┐
   │  normalize → stopword-filter → IDF/specificity rank            │
   │  (the dispatch_with_recall apparatus, applied to BOTH legs)    │
   │                                                                │
   │  leg 1  lexical reachability  ── scoped by :domain/:family     │
   │         "is this pattern findable by its own vocabulary?"      │
   │  leg 2  paraphrase reachability ── store recall ladder         │
   │         "is it findable by what a blocked runner would type?"  │
   └────────────────────────────────────────────────────────────────┘

   flexiarg files stay as the AUTHORING + REVIEW format (git-diffable,
   human-editable, the thing claude-4 reviews). The store is the
   RETRIEVAL substrate. One direction: files → store, on assay pass.
   The TSV becomes a build artifact, or disappears.
```

**The curation boundary survives as a query predicate, not a directory
glob.** `:domain :mathematics` scopes a math mining run; it does not
require the pattern to sit in one blessed directory, and it cannot be
silently bypassed by filing a pattern one level over.

## 5. Migration, in dependency order

1. **Schema + ingest** — define `:pattern/library` fully, ingest all 1358
   TSV rows *plus* the flexiarg bodies, uuid-keyed, `:qualified` as an
   attribute. The 26 duplicate qualified names surface as a data-quality
   report at ingest instead of vanishing.
2. **Retrieval over the store**, reusing `dispatch_with_recall`'s
   normalize/stopword/IDF stage rather than a second implementation.
   Scope by `:domain`.
3. **Re-point Tier-0** at the store, or retire it. `cas_select`'s
   CAS-SEL-3 fixture path keeps working off its own snapshot.
4. **Then** the tide test is measurable — both legs exist, both are
   lexically hygienic, and the mined patterns are in the pool by
   `:domain`, not by which directory someone chose.
5. TSV → build artifact or deleted; storage twin archived.

Sequenced this way, defects 1, 2, 4 and 5 are *dissolved* rather than
patched, and defect 3 (formal-identifier vocabulary vs runner English)
becomes the one real question left — which is exactly the question the
tide test was built to ask.

## 6. Provenance of every number here

```bash
# 140,333 evidence rows
curl -sS http://127.0.0.1:7073/api/alpha/evidence/count
# mined pattern absent from the store (404)
curl -sS http://127.0.0.1:7073/api/alpha/entity/math-formalization%2Ftransport-across-an-instance-diamond
# 1358 index rows → 1334 keys, 24 dropped; 26 dup qualified, 16 disagreeing
cd /home/joe/code/futon6/scripts && python3 -c "
import sys,collections; sys.path.insert(0,'.')
import cas_select as cs
rows=cs.read_index_rows(cs.DEFAULT_INDEX); P=cs.load_all_patterns()
print(len(rows), len(P), len(rows)-len(P))"
# 45 vs 1334 pools; the six mined patterns present/absent
# stopword dominance: 'the' in 218 hot sets  (claude-4, review of ad8ec02)
# 7/12 queries with no specific shared token (claude-4, same review)
```

Counts 2 and 3 in §2 are claude-4's measurements from its review of
`ad8ec02`; I reproduced the 24-row drop and the 16 disagreeing collisions
independently. `pytest` is installed nowhere on this box, so any "tests
pass" claim about `cas_select` needs an independent harness — claude-4
used one and got 7/7.

## 7. What this does not decide

- Whether `probe-the-claimed-property-not-the-acceptance-proxy` is
  `:domain :mathematics` at all. It is filed `process-coherence/` and it
  is genuinely not a math pattern; under `:domain` scoping it simply
  would not appear in a math run, which is the correct outcome and needs
  no directory move.
- Whether the two pattern namespaces reconcile: the memory-attachment
  lane uses coarse buckets (`math/holomorphic-disk-api`) that already
  exist in the store, while the library uses `math-formalization/…`.
  These are different objects today. Ingest must decide whether the
  coarse buckets are patterns, tags, or a third thing.
- Sequencing against the mining slices. Slice 4 can run on the current
  files; only the *tide test* is blocked on this migration.

---

## 8. Where this fits: it is a Phase 0 instrument

Read against `E-memory-whitepaper-v3-programme.md`, this is not a detour
from the programme. It is a **Phase 0 unrecoverable that the bank never
enumerated**, because the bank's instrument list covers the *memory*
channel and this is the *pattern* channel.

Phase 0's rule is the sharpest in the programme:

> **"Every dispatch that runs without these loses data forever."**

Three of the programme's own predictions are unmeasurable until this
migration lands:

| Prediction | What it needs | Status today |
|---|---|---|
| **P3** recall-empty falls as scribe coverage accumulates; *falsifier:* empty-rate insensitive to coverage ⇒ lexical/scribe work is misdirected | coverage must be able to *reach* the retriever | mined patterns are 404 in the store; coverage can rise with no possible effect on empty-rate, which would **fire P3's falsifier for the wrong reason** |
| **P4** LIB-citation rises after *"index automation (the cheapest lever, still unautomated)"* | the automated index | §5 of this document IS that lever |
| **P2** fingerprinted use shifts substitutive-ward; *falsifier:* regulative stays ≥80% | pools that don't mix the two kinds | see §9 — currently mixed |

**P3's falsifier is the live danger.** If we run waves while mined
patterns are unreachable, a flat empty-rate reads as "the attachment-layer
conjecture dominates, lexical/scribe work is misdirected" — a substantive
finding about the *design* — when the true cause is that the supply never
reached the retriever. That is the silence result recurring inside the
instrument built to measure it: well-formed receipts, honest-looking null,
wrong conclusion. §2's five defects are exactly the mechanism.

So the sequencing claim is stronger than "the tide test is blocked":
**this migration is a precondition for the second-half programme's
headline measurements**, and it belongs in Phase 0 beside 0a/0b/0c rather
than after them.

## 9. Process patterns vs content patterns — a measurement problem

The mining produced a **mix**, and the mix is not merely untidy.

- **Content/substitutive** — `transport-across-an-instance-diamond`,
  `lift-prove-upstairs-reflect-by-injectivity`,
  `close-bijectivity-by-counting-not-inverting`,
  `construct-through-a-finite-correspondence`,
  `replace-enumeration-with-structural-counting`. Mathematics. These
  belong in a math runner's pool.
- **Process/regulative** — `probe-the-claimed-property-not-the-acceptance-proxy`,
  `separate-evidence-history-from-verdict-state` (rejected, held).
  These are about *the kind of work we are doing here*: gates, verdicts,
  evidence, acceptance. They do **not** belong in a math runner's pool.

The programme's own taxonomy already names this split —
**substitutive-content** vs **regulative/practice** — and **P2's
falsifier turns on the ratio between them** ("regulative stays ≥80% of
fingerprinted uses ⇒ §4's surprise is the finding and the promotion
investment thesis is wrong").

Which makes the consequence precise: **if process patterns sit in the
math pool, accurate recall of them corrupts P2.** A math runner surfacing
`probe-the-claimed-property` is a regulative hit that occurred because of
pool composition, not because the memory was load-bearing — and P2 would
count it. Joe's "accurate recall of them would be bad news" is exactly
right, and the reason is measurement, not distraction alone.

**Proposal (Joe's): a `library/pattern-mining/` family.** It is the right
home, and the reasons are stronger than tidiness:

1. Process patterns are genuinely useful — to *this* work and to the
   helpers doing it. Deleting them would lose real findings.
2. Under `:domain` scoping (§4) they simply do not appear in a math run.
   No directory games, no filter to bypass.
3. It gives the mining lane its own dogfood corpus: the campaign's method
   patterns, retrievable by the agents running the campaign.
4. It makes the mix *visible* at authoring time — a candidate that wants
   to be in `pattern-mining/` is announcing that it is regulative, which
   is metadata P2 needs anyway.

Recommended: `:domain :process` (or `:pattern-mining`) as a first-class
domain beside `:mathematics`, so the split is a store predicate rather
than a directory convention, per §4.

## 10. Candidate process patterns from this week — staged, NOT authored

The architecture work itself produced findings that look like
`pattern-mining/` material. They are recorded here as **candidates with
provenance**, deliberately not authored as library patterns: the mining
discipline requires ≥2 independent instances, an honest HOWEVER, and
review by someone who is not the author, and the author here is me.
Authoring them by fiat because they are mine would be the exact
self-corroboration failure that got `search-the-namespace` rejected.

| Candidate | Instances so far | Note |
|---|---|---|
| **presence-is-not-configuration** | claude-4 on the roster but auto-registered with no model/role (today); catalogue instance 5 — seat re-registration returning `ok` while serving stale config; `patterns-index.tsv` present at the expected path with detached content | **at threshold (3, independent subsystems)** — strongest candidate |
| **verify-the-premise-before-acting-on-a-finding** | amendment 6 (packet text never checked); amendment 9 (infrastructure never checked); cleanup items 5–6 struck today (state moved after the review that generated them) | **at threshold (3)**; the catalogue already names two |
| **do-not-plant-the-diagnosis-in-the-packet** | my dispatch pre-labelled "a hotword defect, repairable" and received it back as a finding; catalogue instance 11 (phantom `native_decide` ban — a supervisor attribution repeated until it read as fact) | 2 instances; HOWEVER available (a planted label can still be *true* — mine was nearer right than my correction) |
| **check-the-object-is-misfiled-before-deleting-the-filter** | the `math-informal*` boundary read as a bug and widened, when six files were misfiled | n=1 (today) |
| **the-instrument-that-scores-your-lane-is-not-yours-to-build** | retriever dispatched away from me; tide test held | n=1, but it is the programme's own separation rule |
| **measure-the-quantity-the-claim-is-about** | my basename-vs-qualified-name miscount that appeared to contradict claude-4; claude-4's first stoplist, which would have manufactured its own zero-overlap result | 2 instances, both self-caught |
| **a-machine-move-does-not-carry-the-agent's-memories** | Joe, today: the campaign moved Zone←Dionysus and Claude's own memories did not follow, which is *why basics are being rediscovered* | n=1, high explanatory value |

That last one deserves emphasis because it reframes several of the
others. The Zone/Dionysus move carried the corpus, the toolchain, the
store and the lanes — but **not the operator's memory**. Several of
today's rediscoveries (the futon3 version-control premise, the index
symlink, the family boundary's purpose) are things a Dionysus-resident
operator would likely have known. The working patterns did not carry
over because the *substrate for working patterns* did not carry over —
which is, precisely, the thesis this whole programme is testing, observed
on ourselves.

**Next step for §10, if wanted:** these go to a fresh seat to be mined
*independently* from the week's transcripts under the stage-1 rubric,
blind to this table. If the same candidates surface from blind reads,
they clear the bar honestly. If they do not, this table is what it looks
like when an author grades their own week.
