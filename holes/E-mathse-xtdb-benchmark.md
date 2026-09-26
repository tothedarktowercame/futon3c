# E-mathse-xtdb-benchmark — temporal text-index benchmark from Math.StackExchange

**Status:** PROPOSED (2026-08-23).  
**Owner:** Joe.  
**Upstream design target:** [XTDB #5637 — Text indexing](https://github.com/xtdb/xtdb/issues/5637).  
**Origin:** Follow-up to the 2026-08-05 discussion with James Henderson, where
Stack Exchange mining assets were proposed as a benchmark corpus for #5637.  
**Lineage:** this is the follow-on that `futon2/holes/M-text-sidecar.md`
explicitly promised ("prefix search, phrase queries, relevance ranking …
deferred to a follow-on mission if JUXT engages. The follow-on gets authored
when the boundary is hit"). JUXT engaged 2026-08-05. It inherits D1 (the FTS5
sidecar) and owes D2 (the #5637 evidence packet). Operator anchor:
`M-text-sidecar` §HEAD and `futon0/holes/missions/M-futon-problems.md` §D14.  
**Classification (review, claude 2026-08-23):** promote to
`M-mathse-xtdb-benchmark` under `futon4/holes/mission-lifecycle.md`, with
phases trimmed — see §Lifecycle plan at the end. Until then this file stands
as the IDENTIFY + DERIVE draft.

## Purpose

Build a reproducible benchmark that turns the capability and design choices in
#5637 into executable tests. The benchmark uses the Math.StackExchange data
dump because it contains substantial natural-language and mathematical text,
field structure, revisions, and human-created duplicate links.

The central relevance question is:

> Given the original text of a question at the moment it was posted, would a
> text search over the corpus visible at that moment have found the question
> later identified as its duplicate?

The benchmark is not a redistributed corpus. The repository contains scripts,
schemas, derivation rules, runners, and a synthetic test fixture. A user obtains
the upstream dump and processes it locally.

## Satisfaction requirements

### S1. Reproducible, provenance-preserving derivation

- **S1.1** The benchmark MUST accept an explicitly identified upstream
  Math.StackExchange dump and MUST record its source, release/version, input
  filenames, sizes, and checksums.
- **S1.2** One command MUST deterministically transform the same input dump and
  configuration into the same logical XTDB transactions and benchmark cases.
- **S1.3** Generated records MUST retain the upstream post, revision, link, and
  user identifiers needed for attribution and audit. Content-licence metadata
  MUST be retained where the dump supplies it.
- **S1.4** No Stack Exchange content or generated XTDB database MUST be required
  in the repository. Automated tests MUST use a small synthetic fixture.
- **S1.5** The processor MUST stream or batch the XML input within a documented
  memory bound; it MUST NOT require loading the whole dump into JVM heap.
- **S1.6** A derivation manifest MUST report input and output counts, rejected or
  incomplete records, and enough checksums/statistics to detect a partial or
  non-repeatable import.

### S2. Faithful temporal model

- **S2.1** Question creation MUST create the initial valid-time version of a
  question.
- **S2.2** Title, body, and tag revisions MUST create later valid-time versions,
  preserving the values visible during each interval.
- **S2.3** Import transaction/system time MUST remain distinct from the
  historical valid time represented by the source event.
- **S2.4** Duplicate links MUST be represented as separately timestamped facts,
  not folded into the question text or made visible before their creation.
- **S2.5** The importer MUST state which deletions, merges, removed links, or
  missing histories cannot be reconstructed from the dump. It MUST NOT silently
  treat absence from the dump as a known historical deletion.
- **S2.6** Snapshot checks MUST demonstrate that at least one edited question
  returns different, correct text before and after its revision time.

### S3. Leakage-free duplicate benchmark

- **S3.1** A benchmark case `Q -> D` is eligible only if duplicate target `D`
  existed when source question `Q` was posted.
- **S3.2** Candidate search MUST run against the corpus visible at `Q`'s
  creation time. Text added in later revisions MUST NOT affect that result.
- **S3.3** The later duplicate link is an evaluation label only. It MUST NOT be
  supplied to candidate generation, ranking, token selection, or analysis.
- **S3.4** Search input MUST follow named deterministic policies rather than an
  invented human query. The minimum policies are original title, original
  title plus tags, and original title plus original body.
- **S3.5** The benchmark MUST distinguish the observed claim "a moderator later
  linked Q to D" from the stronger and unobserved claim "Q was already a
  semantic duplicate at creation time."
- **S3.6** Cases excluded because of missing history, missing targets, target
  creation time, or malformed content MUST be counted by reason.

### S4. #5637 retrieval-level coverage

The derivation MUST produce independent suites for the feature levels in
#5637. An implementation MAY support only a declared subset, but the runner
MUST report unsupported suites rather than count them as passes.

- **S4.1 Exact token:** derived positive and negative cases for analysed-token
  equality.
- **S4.2 Prefix:** cases using deterministic prefixes of eligible terms, with
  explicit minimum prefix length and negative controls.
- **S4.3 Boolean:** AND, OR, and NOT cases composed from title/body terms and
  exact tags.
- **S4.4 Phrase:** positive and negative multi-token phrase cases whose token
  order is known at the queried valid time.
- **S4.5 Fuzzy:** edit-distance cases, including a suite derived from spelling
  changes where revision history supplies an unambiguous correction.
- **S4.6 Relevance ranking:** duplicate retrieval scored with at least MRR and
  Recall@1, @5, @10, and @100.
- **S4.7 Linguistic analysis:** at least one declared English stemming and
  stop-word profile, compared with an unstemmed baseline.

Every suite MUST record its derivation rule, case count, corpus snapshot, query
policy, analysis profile, and expected judgement.

### S5. Per-field and mathematical analysis

- **S5.1** Title, body, and tags MUST remain distinct indexable fields.
- **S5.2** Tags MUST support exact, non-stemmed matching independently of prose.
- **S5.3** The benchmark MUST compare title-only, title-plus-tags, and
  title-plus-body retrieval rather than assuming one field mixture.
- **S5.4** HTML removal MUST be an explicit analysis step with a testable output.
- **S5.5** TeX, Unicode mathematical symbols, and code/preformatted blocks MUST
  not be handled accidentally by a generic prose analyser. The benchmark MUST
  define at least two contrasting profiles, initially:
  1. prose analysis with TeX preserved as opaque source text;
  2. prose analysis with TeX commands and mathematical symbols tokenised by a
     documented deterministic rule.
- **S5.6** Each profile MUST report token and posting counts per field so the
  storage and selectivity effects of analysis choices are visible.

### S6. Current, historical, and ever-held semantics

The benchmark MUST compare three declared index semantics:

- **CURRENT:** only the latest visible version of each question;
- **AS-OF:** versions visible at the benchmark case's historical valid time;
- **EVER-HELD:** terms from all indexed historical versions.

It MUST answer:

- **S6.1** whether current-state search leaks text added after `Q` was posted;
- **S6.2** whether and when an edit makes a known duplicate retrievable;
- **S6.3** how much EVER-HELD increases terms and postings relative to CURRENT;
- **S6.4** whether candidates returned by an index remain valid after XTDB
  re-checks them at the same valid-time snapshot;
- **S6.5** whether migrations/rebuilds reproduce the same results and reset any
  implementation-specific ever-held accumulation in a documented way.

### S7. Candidate-and-re-check contract

- **S7.1** The text index MUST return candidate identifiers with scores; XTDB
  remains authoritative for document state and temporal visibility.
- **S7.2** Every measured result set MUST be re-checked against XTDB at the same
  valid time as candidate generation.
- **S7.3** The index MUST expose or accompany results with an indexed-through
  watermark sufficient to determine whether the requested snapshot is covered.
- **S7.4** The benchmark MUST measure candidate false positives removed by
  re-check, hydration latency, and the effect of candidate-set size.
- **S7.5** A stale or insufficient watermark MUST be reported explicitly. The
  benchmark MUST NOT hide it by silently querying a different snapshot.

### S8. Lifecycle, scale, and performance evidence

- **S8.1** The runner MUST separately measure initial build, chronological
  revision ingestion, query, re-check, and deterministic rebuild.
- **S8.2** Reports MUST include corpus/document/version counts; indexed fields;
  term/posting counts; on-disk index size; ingest throughput; update-to-search
  latency; query latency; candidate count; and hydration/re-check latency.
- **S8.3** Latency reports MUST state sample size and distinguish cold and warm
  runs. A few observations MUST NOT be presented as p95/p99 distributions.
- **S8.4** Correctness results MUST be reported separately from performance.
  Timeouts, unsupported features, stale-watermark cases, and incorrect results
  MUST remain distinguishable.
- **S8.5** A rebuild from the same manifest MUST reproduce case eligibility and
  relevance judgements exactly; ranking changes require an explicitly changed
  implementation or configuration identity.

## Minimum acceptance slice

The first useful version is intentionally narrower than the full requirements
space. It is accepted when it:

1. Processes question creation, title/body/tag revisions, and duplicate links
   from a locally supplied Math.StackExchange dump.
2. Demonstrates correct XTDB valid-time reconstruction with synthetic tests and
   sampled real-data audits.
3. Generates leakage-free duplicate cases using the three fixed query policies.
4. Runs title-only BM25-style retrieval over both CURRENT and AS-OF corpora.
5. Reports MRR, Recall@1/5/10/100, build and query timings, index size, and all
   eligibility/exclusion counts.
6. Re-checks candidates through XTDB at the case's valid time and records an
   indexed-through watermark.
7. Rebuilds deterministically from the same dump manifest.

Phrase, fuzzy, Boolean, alternate linguistic analysis, and specialised TeX
profiles are subsequent independent satisfaction increments. They MUST not be
simulated or marked complete by weakening the minimum slice.

## Expected generated artefacts

Names and formats may change, but the derivation must expose equivalents of:

```text
manifest.edn
transactions.arrow
duplicate-cases.arrow
exact-token-cases.arrow
prefix-cases.arrow
boolean-cases.arrow
phrase-cases.arrow
fuzzy-cases.arrow
analysis-profiles.edn
expected-counts.edn
```

These are local build products, not repository inputs.

## Non-goals

- Republishing the Math.StackExchange dump or a populated XTDB database.
- Claiming duplicate links are complete semantic relevance judgements.
- Selecting a Lucene dependency boundary before measurements exist.
- Treating latest-state search as an adequate proxy for historical search.
- Optimising benchmark scores by using duplicate links or future revisions as
  search features.
- Routing around missing XTDB temporal or indexing semantics in the benchmark.
  A blocked requirement remains visible as a failed or unsupported requirement.

## Decision evidence for #5637

The benchmark is successful only if its results help decide the scope described
in #5637. At minimum, its report must make evidence available for:

1. which retrieval levels materially improve duplicate discovery;
2. whether ranking and phrase support justify position/frequency-bearing index
   data rather than exact posting lists alone;
3. which fields require distinct analysis;
4. whether generic Lucene analysers handle mathematical text adequately;
5. the operational cost of CURRENT, AS-OF, and EVER-HELD semantics;
6. the required watermark and candidate-re-check surfaces; and
7. whether XTDB should adopt Lucene analysis, storage, query execution, or some
   smaller combination of those subsystems.

---

## Review notes (claude, 2026-08-23)

The sections above were written before surveying what exists. Everything
below is what a MAP pass turns up, and how it changes the spec's emphasis.
The requirements text above is left intact; these notes say which parts are
load-bearing, which are deferred, and what the first dispatch should be.

### MAP — what already exists

| asset | where | bearing on this spec |
|---|---|---|
| Math.SE dump, snapshot 2024-04-07 | `storage/futon6/se-data/math.stackexchange.com/` — Posts.xml 5.8 GB, PostHistory.xml 9.5 GB, PostLinks.xml 48 MB, Comments/Votes/Users/Badges/Tags | Answers S1.1. Record this snapshot date and the CC BY-SA 4.0 licence in the manifest. The `.7z` is alongside (and a second copy under `_linode_reclaimed/`). |
| Tag-slice samples | `futon5/data/stackexchange-samples/math.stackexchange.com__{category-theory,mathematical-physics}.jsonl` (50 rows each) | Seed material for the synthetic fixture (S1.4) and the natural first slice. |
| **Reference implementation of S7** | `futon1b/futon1b_text.clj` (743 lines): FTS5 candidate pre-filter + per-candidate XTDB 2 re-check, BM25 `:score`, deterministic rebuild, keyset checkpoint; oracle `futon1b/fts_oracle.clj` (10/10 agreement, 2026-07-11) | The spec never names *which index is under test* — #5637 is unimplemented, so without this the runner has nothing to run. **The sidecar is implementation #1.** |
| Prior S6 measurement | `futon1b/textprobe_divergence.clj`, `textprobe_census.clj`, `textprobe_updates.clj`, `futon1a/scripts/textprobe-history-*.clj` | Ever-held posting inflation **1.028** over 131,807 histories vs JUXT's guessed 1.5–2×; plus the finding that migration *flattens* ever-held accumulation. S6.3/S6.5 have been measured once on a flattened corpus; Math.SE with full PostHistory is the *un*-flattened one. |
| XTDB version | `futon1b/deps.edn`: xtdb-core/xtdb-api **2.1.0**; separate JVM on :7073 (I-0 override); 4 GB heap, OOMed on `/health?deep=true` 2026-08-23 | Valid-time backdating is native (`VALID_FROM` on insert). Heap is the scale constraint — see slice parameter below. |
| Outreach state | `futon7/data/outbox/receipts/2026-07-28-jhenderson-xtdb5637.edn`; call 2026-08-05; JH happy to collaborate on #3663 first; Joe replied on #3663 ~2026-08-21 | Not a contract route (M-futon-problems D14 discount). The deliverable is findings, Joe-gated before posting, same as D2. |

### What the MAP changes

1. **The irreplaceable asset is S6, not S4.** S4.1–S4.7 is a Lucene feature
   matrix JUXT can build for themselves. What nobody else holds is *bitemporal
   edit history + human duplicate labels* — exactly the chalk notes' named
   stress case ("rewrite-heavy documents"), for which they have no data.
   Re-centre: CURRENT vs AS-OF vs EVER-HELD is the headline; duplicate
   retrieval is the vehicle that supplies a ground-truth relevance label.
   S6.1 (does latest-state search leak post-creation text?) is the number to
   lead with, because it is what every non-temporal search engine gets wrong.
2. **Two independently timed duplicate-label sources.** `PostLinks`
   LinkTypeId=3 (the spec's source) has no deletion history (S2.5).
   `PostHistory` PostHistoryTypeId=10 (close) with the duplicate close-reason
   names the original question ids *with the closure timestamp*. Use both:
   it gives S3.5's "moderator linked later" claim a date, and partially
   recovers removed links. MAP question for the census below.
3. **Slice parameter in the manifest** (tag set + date cutoff). 9.5 GB of
   PostHistory into a 4 GB-heap XTDB 2.1.0 node is not a first iteration.
   First slice: `category-theory` (samples exist). Full dump is an S8
   increment, not part of the minimum slice.
4. **Original title is load-bearing (S3.4).** Moderators routinely retitle
   questions *after* closing them as duplicates, often borrowing the target's
   wording. A "current title" query policy would leak the label through the
   title. Keep the three policies as written; add the census count of
   post-closure title edits so the leak's magnitude is reported.
5. **The CURRENT arm is the non-temporal baseline.** Plain FTS5 over latest
   text is what any ordinary engine does; say so explicitly so S6.1 reads as
   a comparison against the status quo, not as an XTDB-internal curiosity.
6. **Cite the 1.028 result in S6.3/S6.5.** The question this corpus can
   answer that the futon1b corpus could not: was 1.028 a migration artefact?

### Priority within the requirements

- **Load-bearing for the minimum slice:** S1, S2, S3, S6 (CURRENT + AS-OF;
  EVER-HELD as soon as the sidecar exposes it), S7, S8.4.
- **Deferred increments, each its own handoff:** S4.2–S4.5 (prefix, boolean,
  phrase, fuzzy — S4.5's revision-derived spelling corrections is clever but
  expensive), S4.7 and S5.5 profile 2 (stemming / TeX tokenisation), S8.1–S8.3
  (p95/p99, cold/warm, sample sizes — correct as written, premature on
  iteration one), full-dump scale.
- **Keep from day one:** S8.4 — correctness reported separately from
  performance; timeouts / unsupported / stale-watermark / wrong remain
  distinguishable.

### Handoff plan (one file / one behaviour / one acceptance test each)

Per `CLAUDE.md` "keep handoffs small": the minimum acceptance slice as
written is seven things in one packet. Split, with a review gate between
each:

1. **Discovery census (MAP, no code that ships).** Over the `category-theory`
   slice of the on-disk dump, count: (a) duplicate links (PostLinks type 3);
   (b) those where the target predates the source — the eligible set;
   (c) sources with edits *before* the link; (d) sources whose title was
   edited *after* closure; (e) closure events (PostHistory type 10, duplicate
   reason) and their agreement with PostLinks. **If (b) is small, the
   benchmark has no power in this slice and the mission reshapes for £0.**
2. Streaming XML → normalised event log (creation; title/body/tag edits;
   links; closures) with the derivation manifest (S1.1, S1.5, S1.6).
   Synthetic fixture + tests.
3. XTDB loader with valid-time backdating (S2.1–S2.4); S2.6 snapshot test on
   the fixture plus a sampled real-data audit.
4. Case generation with exclusion counts by reason (S3.1–S3.6).
5. Runner: FTS5 sidecar as implementation #1; CURRENT vs AS-OF; MRR and
   Recall@{1,5,10,100}; re-check and indexed-through watermark (S6, S7).

### Discovery census result (2026-08-23)

Handoff 1 is complete. A streaming census in
`/home/joe/code/mathse-xtdb-benchmark` scanned the 2024-04-07 dump and tied
its result to SHA-256 checksums for `Posts.xml`, `PostLinks.xml`, and
`PostHistory.xml`. The `category-theory` slice has enough power for the first
benchmark iteration:

| measure | count |
|---|---:|
| questions in dump | 1,641,406 |
| `category-theory` questions | 14,706 |
| duplicate links | 234 |
| eligible links whose target predates the source | **228** |
| excluded: target missing / target not older | 1 / 5 |
| sources edited before first duplicate link | 90 |
| sources retitled after first duplicate closure | 7 |
| duplicate pairs in both PostLinks and PostHistory | 224 |
| pairs only in PostLinks / only in PostHistory | 10 / 9 |

The result clears the handoff's reshape gate. The marginal disagreement also
confirms that both duplicate-label sources must be retained with provenance;
neither is a complete substitute for the other. The generated report is
`out/category-theory-census.json` and is deliberately ignored by git.

### Lifecycle plan (why a Mission, and which phases)

The spec hits all four of `mission-lifecycle.md`'s "wiring diagram required"
triggers: it is a machine (import → load → derive → run → re-check); more
than one party writes fields another reads (importer / manifest / runner /
XTDB); records cross a process boundary (sidecar ↔ XTDB JVM); and its claims
rest on evidence it produces. That is the shape the lifecycle formality pays
for. Trimmed:

| phase | plan |
|---|---|
| HEAD | **skip** — operator anchor exists in `M-text-sidecar` §HEAD and `M-futon-problems` §D14; quote it. |
| IDENTIFY | the central question + minimum slice above; add the lineage header (done). |
| MAP | the table above + handoff 1's census. |
| DERIVE | S1–S3, S6–S7 as written, with the re-centring in §What the MAP changes. Wiring diagram: importer / manifest / XTDB / sidecar / runner / report, ports = manifest fields and the watermark. |
| ARGUE | five sentences, plain language: *we hold the one corpus that can answer JUXT's open question with data, and we already built the machine that answers it.* |
| VERIFY | Specification BOM. The *claim* row is the one to get right: denominator = S3.6's exclusion counts; confounder = post-closure edits (item 4 above). |
| INSTANTIATE | handoffs 2–5. |
| DOCUMENT | the #5637 comment (Joe-gated, per D2), not a docbook entry. |
