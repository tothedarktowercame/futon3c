# Excursion: Pattern Mining

**Type:** Excursion (E-prefix; bounded scope-out from a mission; owned end-to-end by a single hand-off agent; see [[project_e_prefix_excursions]] for the convention).
**Status:** ACCEPTED / MAP-SEEDED (claude-1 authored 2026-05-25; accepted by Codex 2026-05-25; bounded embedded MAP pass required before v0 implementation claims).
**Date:** 2026-05-25
**Author:** claude-1 (inhabiting `:war-machine-pilot` peripheral; emacs-repl surface paired with Joe).
**End-to-end owner:** Codex.
**Sibling excursions (the operational REPL family under M-war-machine-pilot v1):**
- `E-street-sweeper.md` (claude-2-owned; PRINT — working-tree commits to git)
- `E-pilot-vsatarcs-feed.md` (codex-owned, EXECUTED 2026-05-25; PRINT — evidence sync to VSATARCS)
- `E-night-shift.md` (codex-owned, PARTIALLY EXECUTED 2026-05-25; EVAL — code modification via branch+PR)
- **`E-pattern-mining`** (this excursion; LOOP — substrate update; closes the cycle)

**Parent mission:** `futon3c/holes/missions/M-war-machine-pilot.md` (v1 cycle).  Cross-refs to `futon3/holes/missions/M-pattern-mining.md` and `M-pattern-application-diagnostic.md` (existing IDENTIFY-phase missions whose operational completion this excursion enables).  Also `M-pilot-appearance.md` §8.x for the canonical REPL framing.
**Surface that authored this:** emacs-repl 2026-05-25 between claude-1 and Joe.

## Why this exists (Joe, emacs-repl 2026-05-25)

The blue-yellow-green hue arrangement on the WM patterns view (landed earlier this session) made the loop's gap visually undeniable.  Joe's framing:

> *"Just like E-street-sweeper 'prints' material to Git, and E-night-shift 'evals' AIF signals plus current code state to get new code state, we need an E-pattern-mining phase to get a real read-eval-print-loop going (AIF as we have set it up covers 'read' pretty well)."*

The four-step REPL is half-built:

| REPL step | Capability | Status | Owner |
|---|---|---|---|
| **READ**  | AIF/WM signal intake | ✓ shipped: scheduler cache, observation channels, retrieval-tagged evidence | apparatus core |
| **EVAL**  | code state transformation | ✓ landed: `E-night-shift` (branch-isolated PR-as-deliverable) | codex |
| **PRINT** | typed artefact emission | ✓ landed: `E-street-sweeper` (git commits) + `E-pilot-vsatarcs-feed` (substrate sync) | claude-2, codex |
| **LOOP**  | substrate update from cycle observations | **gap — this excursion** | proposed claude-4 |

What "by all rights should show up a lot but doesn't" is the diagnostic.  From the live patterns view (2026-05-25 14d window):

- **iiching / iiching**: 257+ patterns, 0 activations.  Joe's read: machine-generated at scale, large, Chinese / LISP / both — embedding-retrieval-hostile by construction.  *Understandable*.
- **A bunch of other 0-activation collections** that "by all rights should show up a lot."  *Not understandable* — these are real gaps the library has where work is happening but patterns aren't surfacing.  Pattern-mining is the closure mechanism.

The patterns view + activation data are now the **diagnostic instrument** for E-pattern-mining: pattern-mining's job is to make those 0-activation zones either activate (because the patterns get retrieved) or honestly retire (because they don't match anything anyone needs).

## What "Pattern Mining" is

A new peripheral or substantive process (envelope TBD by owner — peripheral if it benefits from inhabited cycles, batch-job if not), provisionally named **`:pattern-miner`** (peripheral form) OR **`pattern-mining-job`** (batch form).

**Input:** the live WM patterns block (with per-collection activation counts), the evidence store (PSRs, context-retrieval entries, raw turn evidence), recent code/commit activity, recent mission and excursion authoring.

**Process:** identify (a) **implicit patterns** present in the actual work but not in the library, (b) **silent library patterns** that should be activating but aren't (mismatched encoding, wrong embedding, stale rationale), (c) **dead library patterns** that haven't activated in any reasonable window and probably shouldn't exist anymore.

**Output:** **candidate** new/revised/retired patterns, **not** automatic library mutations.  Mined candidates flow through operator review (analog to PR review in E-night-shift) before merging into the canonical pattern library at `futon3a/...`.

**LOOP closure:** approved candidates land in the library; the next AIF READ cycle retrieves a richer pattern set; the next EVAL cycle's hop-into-night-shift has better recommendations; the next PRINT cycle ships better-grounded artefacts.  The patterns view's activation gradient sharpens or shifts as the library improves.

## Why this isn't just "M-pattern-mining"

The existing `M-pattern-mining.md` (futon3, IDENTIFY 2026-04-27) and `M-pattern-application-diagnostic.md` (futon3, IDENTIFY 2026-04-27, IFR-articulated) are the *substantive* missions for the broader pattern-application program.  This excursion is the **operational handle** that makes them tractable now:

- **Mission scope is bigger** — multi-phase IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE.  Years of work.
- **Excursion scope is smaller** — *one cycle of mining → review → merge*, demonstrated end-to-end with the current substrate.  Days of work.

Once this excursion ships one real round-trip, M-pattern-mining gains a working v0 it can iterate on; M-pattern-application-diagnostic gains its first instrumented closure.  Both move forward without the excursion needing to be everything.

## Embedded MAP pass (required before v0 implementation)

This excursion should include a **bounded MAP phase** before claiming a v0
diagnostic/round-trip design. Not a full mission lifecycle, but enough mapping
to avoid rediscovering earlier work or importing the wrong assumptions.

### MAP objective

Recover the strongest already-written constraints on:

- what counts as a legitimate pattern-mining signal
- what shape a mined candidate should have
- how recurrence should be computed without glib overcounting
- what kind of review / feedback loop makes the result reusable

### MAP donor shortlist

1. **`futon3/holes/missions/M-pattern-mining.md`**
   Use for the *retire-the-old* verdicts:
   - current futon3a MiniLM-cosine retrieval is "artifact, not signal"
   - coverage, silent-fail, duplicate-id, and stale-corpus problems are
     already named
   - the auto-ingest invariant survives even though the old pipeline does not

   Load-bearing consequence for this excursion:
   the v0 diagnostic pass must not treat current activations as a clean sensor.
   Every candidate emitted from live activations must preserve the possibility
   that the underlying retrieval substrate is itself biased or incomplete.

2. **`futon3/holes/missions/M-pattern-application-diagnostic.md`**
   Use for the *build-the-new* shape:
   - pattern applications want typed slots such as
     `{context, tension, move, witness-shape}`
   - demonstrable value requires a witness / round-trip check
   - a candidate pattern is stronger when it points at a constructive witness,
     not just a plausible title or embedding neighbour

   Load-bearing consequence for this excursion:
   staging entries should prefer typed mining rationales and candidate witness
   pointers over loose prose summaries.

3. **`futon3/holes/missions/M-pattern-retrieval-calibration.md`**
   Use for the retrieval / HIT / feedback contract:
   - hybrid top-k retrieval packet
   - structured candidate rendering
   - cheap rationale synthesis
   - operator adjudication as durable evidence

   Load-bearing consequence for this excursion:
   operator review should be treated as a structured feedback loop, not just an
   informal "Joe reads the file". The staging format should be easy to review
   and cheap to accept / reject / mark ambiguous.

4. **`futon3/holes/missions/M-mission-coherence-patterns.md`**
   plus
   **`futon3/library/mission-coherence/PILOT-WORKSHEET-2026-05-11.md`**
   and
   **`.../PILOT-SUMMARY-2026-05-11.md`**

   Use for a practical pattern-mining method:
   - anti-glibness discipline: real mining from historical corpus
   - normalize issues into worksheet rows
   - compute recurrence by mission-level occurrence, not paragraph frequency
   - the worksheet/summary pair is already a usable pilot method

   Load-bearing consequence for this excursion:
   when mining implicit patterns from recent pilot / mission / excursion work,
   recurrence should be computed at the level of turns / missions / excursions,
   not by counting repeated phrases in one long note.

5. **`futon3c/holes/missions/M-pilot-appearance.md` §8.7**
   Use for the local LOOP framing:
   - `Read` = WM/AIF signal intake
   - `Evaluate` = capability classification / hop choice
   - `Print` = typed artefact emission
   - `Loop` = reflection, substrate update, and cross-inhabitant continuity

   Load-bearing consequence for this excursion:
   pattern mining is not only library maintenance; it is the recap / reflection
   turn that metabolises what the prior turns learned.

### MAP questions to discharge in v0

Before implementation is called complete, this excursion should answer:

1. Which live WM collections are best read as:
   - hostile-by-construction
   - silent-because-broken
   - silent-because-dead

2. What is the minimal staging schema that preserves:
   - collection under review
   - source evidence / activation signal
   - candidate action (`new`, `revise`, `retire`, `document-as-intentionally-inactive`)
   - typed mining rationale
   - operator decision field

3. What is the right recurrence unit for implicit-pattern mining in this
   excursion:
   - per turn
   - per session
   - per mission / excursion

4. What counts as a round-trip witness strong enough for success criterion 5:
   - activation count changes only
   - improved retrieval packet quality
   - one accepted candidate landing via `pattern_author`
   - some combination of the above

### MAP output expected from v0

The embedded MAP pass should leave behind at least:

- a donor shortlist with notes against each donor
- a chosen staging schema
- one declared recurrence unit
- one declared witness criterion for the first round-trip

This is enough mapping to keep the excursion honest without inflating it into a
full mission.

### MAP pass results (2026-05-25)

The first embedded MAP pass is now complete enough to constrain v0.

#### Chosen staging schema

Use `futon3a/data/pattern-mining-candidates.edn` as the v0 staging substrate.
The batch shape should be:

- top-level batch metadata (`:schema/version`, `:artifact/type`, window,
  created-at, source artifacts)
- `:map-output` for the bounded MAP conclusions that govern the batch
- `:collection-reviews` for per-collection diagnosis rows
- `:candidates` for actual new/revise/retire proposals

Each candidate row should preserve at minimum:

- `:collection-id`
- `:candidate/action`
- `:diagnosis`
- `:signal`
- `:mining-rationale`
- `:operator-decision`
- optional `:pattern-author/proposal` when the candidate is intended to land
  through `submit-draft!`

This keeps review cheap while still preserving the path into the canonical
`pattern_author` boundary.

#### Declared recurrence unit

For **implicit-pattern mining**, use **mission/excursion occurrence** as the
primary recurrence unit: count at most one hit per authored mission /
excursion / technote / handoff artefact in the mining window.

Reason:

- `M-mission-coherence-patterns` already showed this is the right anti-glib
  move for corpus mining
- paragraph-level counting would overfit long notes
- turn-level counting is still appropriate for raw retrieval diagnostics, but
  not for deciding that an implicit pattern genuinely recurs across the work

So v0 uses a deliberate asymmetry:

- retrieval-surface diagnosis -> per-turn evidence counts
- implicit-pattern recurrence -> per mission/excursion occurrence

#### Declared round-trip witness criterion

The first round-trip witness is **composite**, not activation-only.

It requires:

1. one operator-approved `new` or `revise` candidate lands via
   `futon3a/src/futon/peripheral/pattern_author.clj`
2. the next WM patterns block shows a **structural shift**

For v0, a structural shift may be any of:

- collection count changed
- a new collection became visible
- total pattern count changed
- embedding-coverage changed

Immediate activation-count increase is **welcome but not required** for the
same tick, because the accepted candidate may change library structure before
it has had time to accumulate retrieval evidence.

#### Live substrate finding that changes v0 priority

The current WM patterns block adds an important concrete result:

- every current collection with `count > 10` and `activations-14d == 0`
  also has `has-embedding? == false`
- the strongest examples are:
  - `iiching` (`count 257`, `0 activations`, `has-embedding? false`)
  - `iching` (`64`, `0`, `false`)
  - `math-informal` (`36`, `0`, `false`)
  - `futon-theory` (`33`, `0`, `false`)
  - `writing-coherence` (`23`, `0`, `false`)

This means v0 should prioritise **silent-because-unindexed / broken-coverage**
diagnosis ahead of library retirement for most large silent collections.

The working exception is the `iiching` / `iching` family, which remains the
best candidate for **hostile-by-construction** treatment because the operator
already named it as machine-generated and embedding-retrieval-hostile.

#### Traced examples: `futon-theory` and `writing-coherence`

Two concrete traces sharpen the diagnosis:

- `futon3/resources/sigils/patterns-index.tsv` already contains
  `futon-theory/*` and `writing-coherence/*`, so the break is **not** at
  pattern authoring or canonical index generation
- current counts from the canonical TSV are:
  - `futon-theory`: `33`
  - `writing-coherence`: `23`
- `futon3a/resources/notions/minilm_pattern_embeddings.json` currently
  contains only `407` embedded patterns total against `1125` canonical TSV
  rows, and contains **0** entries from either `futon-theory` or
  `writing-coherence`
- the notions loader in `futon3a/src/futon/notions.clj` simply loads the
  frozen embeddings JSON from disk; it does not derive missing embeddings or
  refresh stale coverage on demand
- `futon3a/scripts/index_patterns.sh` only rebuilds MiniLM embeddings when
  explicitly invoked with `--minilm`; there is no standing automatic refresh
  path evidenced in this trace

So for these two collections the failure mode is currently best described as:

- canonical patterns exist
- retrieval hotword TSV exists
- MiniLM embedding coverage is missing
- WM activation silence is therefore downstream of a **stale / partial
  embedding build**, not evidence that the collections are dead

This moves `futon-theory` and `writing-coherence` out of the "candidate for
retirement" bucket and into the "repair the embedding/indexing pipeline, then
re-observe" bucket.

#### Refresh-path trace: why the WM shows stale embedding coverage

The traced refresh path is currently asymmetric:

- `futon0/data/cron-jobs.edn` has an installed `:pattern-index-rebuild` cron
  job (`30 4 * * *`) that runs only
  `clojure -M -m scripts.build-pattern-index` in `futon3`
- that cron job refreshes
  `futon3/resources/sigils/patterns-index.tsv`, and this matches the current
  file timestamp (`2026-05-25`)
- there is **no sibling installed cron job** in the registry that runs
  `futon3a/scripts/index_patterns.sh --minilm` or otherwise refreshes
  `futon3a/resources/notions/minilm_pattern_embeddings.json`
- the MiniLM embeddings file used by the retrieval surface and the WM remains
  timestamped `2026-01-16`

The propagation chain into the live WM is then straightforward:

- `futon2/scripts/futon2/report/war_machine.clj` `scan-patterns` reads the
  fresh TSV for collection membership and counts
- the same function reads
  `futon3a/resources/notions/minilm_pattern_embeddings.json` to compute
  collection centroids / `:has-embedding?`
- so a newly-added or newly-indexed collection can appear immediately in the
  WM `:count` field while still appearing embedding-dead in the same row if
  the MiniLM artefact was never rebuilt
- `futon3a/src/futon/notions.clj` and `futon3a/scripts/notions_search.py`
  likewise read the same frozen embeddings JSON directly, so the retrieval
  surface shares the same stale-coverage bias

So the current system behavior is:

- **pattern-count coverage** is on a scheduled refresh path
- **embedding coverage** is on a manual / ad hoc refresh path
- the WM truthfully reports that mismatch as `count > 0` with
  `has-embedding? false`

This is exactly the kind of split Joe's earlier desired invariant family
(`pattern-ingested`, `pattern-embedded`, `pattern-newly-added-must-flow`)
was meant to prevent, but that family has not yet been turned into an
operational check or scheduled repair path in the traced substrate.

#### Opportunity: repair the shared pattern+mission embedding corpus together

This same trace shows the repair target should be broader than patterns alone:

- `futon3a/scripts/index_patterns.sh` already has the extension point for
  mission indexing via `--missions-roots`
- when that path is used, it writes:
  - `resources/notions/mission_records.json`
  - `resources/notions/minilm_mission_embeddings.json`
  - `resources/notions/minilm_corpus_embeddings.json`
- the current on-disk mission artefacts are present and much fresher than the
  stale pattern MiniLM file:
  - `mission_records.json` timestamp `2026-05-21`
  - `minilm_mission_embeddings.json` timestamp `2026-05-21`
  - `minilm_corpus_embeddings.json` timestamp `2026-05-21`
- current counts are:
  - `mission_records.json`: `157`
  - `minilm_mission_embeddings.json`: `157`
  - `minilm_corpus_embeddings.json`: `564` (= `407` pattern entries + `157`
    mission entries)

So there is already a **shared embedding-space** path for patterns and
missions; it is just not on the same operational footing as the newly-added
TSV cron rebuild.

That suggests the clean repair is not:

- "add one more pattern-only rebuild"

but rather:

- "promote the existing shared corpus builder to an installed refresh path"

with the output contract:

- fresh `patterns-index.tsv`
- fresh `minilm_pattern_embeddings.json`
- fresh `mission_records.json`
- fresh `minilm_mission_embeddings.json`
- fresh `minilm_corpus_embeddings.json`

This keeps the pattern-mining repair aligned with the nearby stack-geometry /
mission-locating work, rather than fixing only one half of the intended
embedding space.

#### Dependency trace: which missions this repair actually unlocks

The dependency picture is now clearer than "many things vaguely touch this."

##### Direct unlock: `M-stack-geometry`

This appears to be the strongest remembered "M-...something-or-other" match.

- `futon5a/holes/missions/M-stack-geometry.md` explicitly scopes:
  - mission embeddings in the **same space** as pattern embeddings
  - persistence rather than per-render recomputation
  - missions locatable relative to pattern/claim landmarks
- it also explicitly says that if the existing infrastructure only embeds
  pre-registered library patterns and cannot embed arbitrary mission-doc text,
  extending that tool is in scope there
- its later checkpoint table records "Mission embeddings persisted" as a done
  condition, so the shared corpus is not incidental there; it is one of the
  mission's shipped core deliverables

So the shared refresh path is a **maintenance / operationalisation follow-on**
to M-stack-geometry's embedding-space work.

##### Direct operational owner: `M-patterns-done-right` + `M-the-futon-stack` / `M-recommendation-bindings`

The best owner for turning this from an ad hoc build into a durable invariant
does **not** look like M-stack-geometry itself.

- `futon0/holes/missions/M-patterns-done-right.md` is the cross-stack pattern
  discipline mission; it explicitly spans:
  - library in `futon3`
  - parser / notions in `futon3a`
  - peripheral surface in `futon3c`
  - entity graph / categorical layers elsewhere
- `futon5a/holes/missions/M-recommendation-bindings.md` already names Joe's
  desired invariant family almost verbatim:
  - `pattern-ingested`
  - `pattern-embedded`
  - `pattern-newly-added-must-flow`

So the operational repair belongs most naturally to the
**pattern-library-ingestion discipline** line, with E-pattern-mining acting as
the diagnostic witness that the invariant is presently violated.

##### Direct consumer on the pattern side: `M-a-sorry-enterprise`

Joe's note about `M-a-sorry-enterprise` is right but directional:

- it is about A→B use of retrieved patterns as sorry-proximity signals, not
  B→A mining
- but it directly depends on
  `futon3a/resources/notions/minilm_pattern_embeddings.json` as source
  material

So stale pattern embeddings degrade sorry-proximity scoring immediately, even
though the mission's logic runs in the other direction from E-pattern-mining.

##### Direct consumer on the mining side: `M-learning-loop`

`M-learning-loop` is the nearest sibling in conceptual shape:

- it owns **B→A pattern mining**
- it explicitly treats A→B tagging and B→A mining as a closed loop over the
  same library
- it also names `M-stack-geometry` as sharing the embedding space and as one
  projection used by later pattern-mining work

So the shared corpus refresh path is not M-learning-loop's owner-task, but it
is one of the substrate conditions that makes its mining loop trustworthy.

##### Mission-corpus methodological consumer: `M-mission-coherence-patterns`

`M-mission-coherence-patterns` matters for the mission side of the corpus:

- it explicitly says the mission corpus should be accessed first through
  existing aggregation surfaces rather than raw filesystem scans
- its target is mining recurring structure from the mission corpus

So fresh mission records / mission embeddings are not only useful for spatial
rendering; they also support later corpus-mining work where mission material
must already be surfaced coherently.

##### Practical conclusion from the dependency trace

The repair should therefore be framed as:

- **not** a local patch for WM pattern counts
- **not** a pattern-only rebuild
- **not** a geometry-only refinement

but as a **shared corpus freshness invariant** with at least three immediate
beneficiary families:

- geometry / mission-locating (`M-stack-geometry`)
- pattern application and sorry-proximity (`M-a-sorry-enterprise`)
- pattern mining / mission mining (`M-learning-loop`, `M-mission-coherence-patterns`)

and one likely long-term owner family:

- `pattern-library-ingestion-discipline`
  (`M-patterns-done-right` + `M-the-futon-stack` / `M-recommendation-bindings`)

#### Repair design landed + live witness

The repair is now no longer hypothetical. The correct fix turned out to be:

- **not** "run the old MiniLM script more often"
- **not** "cron the TSV rebuild plus a second ad hoc embed step"

but:

- make the **shared corpus refresh path** the installed contract
- ensure that path uses the **canonical flexiarg projection** rather than the
  old narrow parser
- refresh mission records / mission embeddings in the same run
- give cron an explicit `CLJ_CMD` + `PYTHON_BIN` contract so the job does not
  depend on ambient `PATH` luck

The root cause was sharper than "scheduler missing":

- `futon3/scripts/embed_patterns_minilm.py` still only walks
  `library/` + `holes/LDTS`
- the canonical projection in
  `futon3a/src/futon/flexiarg/projection.clj` sees the real current corpus
- therefore even a fresh run of the old script would have plateaued far below
  canonical coverage

The landed repair path is:

- `futon3a/scripts/index_patterns.sh --minilm --include-missions`

with:

- pattern embedding inputs emitted by the canonical projector
- mission records emitted by `futon.missions index`
- `minilm_pattern_embeddings.json`
- `minilm_mission_embeddings.json`
- merged `minilm_corpus_embeddings.json`

The live witness from the repair run on `2026-05-25` is:

- canonical pattern embedding inputs written: `1036`
- canonical TSV rows after rebuild: `1125`
- mission records / mission embeddings written: `189`
- merged corpus size: `1225`
- live WM collection rows with embeddings: `39 -> 70`

The previously silent example collections now show embedding coverage in the
live WM:

- `futon-theory`
- `writing-coherence`
- `math-informal`
- `storage`
- `realtime`

So the defect is no longer merely diagnosed. The shared refresh path has been
designed, implemented, and observed live. The remaining work is to use that
fresh substrate for actual mining / retirement / authoring rounds.

#### Live A→B retag probe

To test the repaired substrate in a way that is closer to real LOOP use than
"count the embeddings file", I re-ran retrieval against a handful of real
user-turn texts from the live evidence store and recorded the batch in:

- `futon3c/data/pattern-mining-retag-probe-2026-05-25.edn`

The result is materially better than the pre-repair state.

Positive witness:

- the shared-corpus / substrate-repair turn now surfaces
  `library-coherence/library-embedding-refresh` **and**
  `storage/open-world-continuity`
- the "embed missions into the same space" turn now surfaces
  `futon-theory/mission-dependency`
- the "pilot not yet inhabitable" turn now surfaces
  `war-machine/inhabitation-threshold`, `futon-theory/mission-lifecycle`,
  and `realtime/listener-leases`
- the "real night shift" turn now surfaces
  `realtime/transport-pivot` and `realtime/liveness-heartbeats`

So the repaired corpus is now demonstrably reaching collections that were
previously silent in the live WM / retrieval surface:

- `storage`
- `realtime`
- `futon-theory`

This is enough to support the narrower claim we actually need at this stage:

- the repaired embedding system can now retag live A→B turns into collections
  that were previously absent from the reachable retrieval surface

Negative witness:

- the explicit "retag a handful of A→B turns" probe still pulls `iching`
  strongly alongside the more relevant `pattern-discipline` /
  `library-coherence` hits

That is **not** evidence the corpus refresh failed. It is evidence that the
next pattern-mining round should keep `iching` / `iiching` in the seeded v0
review set as likely **high-gravity / absorptive** collections whose salience
may crowd out more helpful retrievals.

The live WM rows sharpen that distinction:

- `iiching` is now embedded but still `257 / 0 activations-14d`
- `iching` is now embedded and `64 / 1 activations-14d`

So the right current question is not simply "are these collections hostile?"
It is:

- are they structurally hostile-by-construction,
- or are they merely highly absorptive collections that need routing,
  weighting, or scope discipline so they do not over-take operational turns?

#### Shared-corpus 2D projection

To get a more constructive read on that question than activation counts alone,
I also rendered the full repaired shared corpus (`1036` patterns + `189`
missions) into a deterministic 2D PCA scatter:

- script: `futon3a/scripts/project_corpus_2d.py`
- points: `futon3a/resources/notions/corpus_projection_2d.json`
- image: `futon3a/resources/notions/corpus_projection_2d.png`

Pattern dots are colored by family; missions are rendered as dark crosses.

The first visual result is already useful:

- `iching` / `iiching` do **not** look like a diffuse haze spread through the
  whole corpus
- they appear as a visibly detached left-hand island, separate from the denser
  mission/pattern field

That matters because it suggests the current question is not just "why are
they retrieved?" but also:

- are they acting as a coherent semantic basin with its own attractor pull,
- and if so, should that basin be treated as a special-purpose library,
  a gated donor, or a down-weighted exotic region for ordinary operational
  turns?

So the 2D view strengthens the "high-gravity" reading. It does not yet decide
whether the right response is retirement, quarantine, or explicit routing, but
it gives us a better geometric basis for that judgement than counts alone.

#### Seeded v0 review set

The first staging batch should therefore start with:

- `iiching` -> review for `:high-gravity / possibly-hostile`
- `iching` -> review for `:high-gravity / possibly-hostile`
- `futon-theory` -> review for `:silent-because-unindexed`
- `writing-coherence` -> review for `:silent-because-unindexed`
- `pacspine` -> review for `:high-on-few-patterns` rather than silence

This is enough to begin a real v0 round without pretending the whole pattern
library has already been understood.

## Scope (v0)

1. **Diagnostic pass** against the live WM patterns block + activations data:
   - List collections with `count > 10` and `activations-14d == 0` ("silent libraries")
   - Sample a handful of recent context-retrieval turns; identify any retrieved candidates that have low relevance to actual content (retrieval is hitting wrong patterns)
   - Optionally sample a handful of recent evidence/turn entries; identify recurring shapes not currently represented in any library (implicit patterns)

2. **Candidate emission**:
   - For silent libraries: propose either (a) a per-pattern reframe / rewrite to improve embedding match, (b) retirement of patterns that genuinely don't apply, (c) splitting the collection if it conflates concerns.
   - For implicit patterns: propose one or more new patterns (with `:rationale`, `:sigil`, `:pattern-id`) that name the recurring shape; suggest which collection they'd live in or whether a new collection is warranted.
   - All candidates emit as structured `:bilateral-evidence`-style entries (or a sibling shape) into a staging substrate the operator reviews — never directly into the canonical library.

3. **Review pathway**: operator reviews the staging file, accepts/rejects/edits, and the accepted candidates land via `:pattern-author` peripheral (`futon3a/src/futon/peripheral/pattern_author.clj` already exists) or equivalent.

4. **Round-trip witness**: at least one mined candidate makes it through review and lands in the library; one new pattern OR one revised pattern is sufficient as v0 proof.

## Hard structural invariants

Mirror the discipline of sibling excursions:

1. **No direct mutation of the canonical pattern library.**  All mining writes to a staging substrate; only operator-approved entries cross into the canonical library via the existing `pattern_author` peripheral.
2. **Provenance trail.**  Every candidate carries `:auto-source` and `:mining-rationale` fields naming what signal drove the proposal (which session / evidence cluster / activation gap).
3. **Per-collection scope.**  One mining pass addresses one collection at a time (or one cross-cutting theme); never bulk-rewrites the library in one operation.
4. **Embedding-aware diagnostics.**  When proposing a rewrite of a silent library, the mining process must demonstrate (via a probe of futon3a retrieval against representative queries) that the rewrite would actually improve activation; otherwise the rewrite is speculative and gets `:speculative true`.
5. **Retirement requires evidence-of-absence.**  Proposing retirement of a pattern collection requires showing it has 0 activations across a configurable window (default 30 days) AND no recent evidence body text mentions any of its pattern-names.
6. **Operator-can-always-discard.**  Mining staging entries older than N days (configurable; default 14) without operator action get marked for reaping (envelope emits a bell with the discard list; never auto-deletes).
7. **No cross-repo bulk operations.**  Library writes stay scoped to `futon3a` (or wherever the pattern-author canonical store lives); mining doesn't touch unrelated codebases.

## Trigger conditions

`:pattern-miner` (or `pattern-mining-job`) is invoked when an agent (today: `:war-machine-pilot`) reaches a LOOP-class observation:

1. **High-activation collection drift**: when a collection's activation count crosses a threshold or its retrieval rank distribution shifts notably (signal: existing patterns may need refinement).
2. **0-activation collection over a horizon**: when a collection has been at 0 activations for ≥ N days despite its `count` being substantial (signal: silent library — diagnose / retire).
3. **Retrieval-quality bell**: when an inhabiting agent observes low retrieval relevance to a turn's actual content (signal: library has the wrong shape for this turn class).
4. **Operator-directed mining**: explicit bell to the miner with a hypothesis.

## Concrete starting points (v0 candidates)

Today's live patterns block gives these immediate workable inputs:

- **`iiching` (0 / 257)** — machine-generated, embedding-hostile.  v0 task: confirm this is a *category-2 hostile-by-construction* collection; document it as "intentionally inactive" or propose a separate retrieval path (e.g. SIGIL match rather than embedding).
- **`f1`, `f5`, `f8` and other f-prefixed collections at low or 0 activations** — these are paper / leaf-based collections; if some have 0 activations they may have indexing issues.  v0 task: identify which f-prefixed collections fall in the "by all rights should show up" zone and propose remedies.
- **`pacspine` (200 / 12)** — high activations on few patterns.  v0 task: examine which pacspine pattern is over-retrieved (likely `pacspine/frictionless-next` at 34 retrievals) and whether the other 11 patterns are under-retrieved because the collection lacks variety the retrieval looks for, OR because their content is buried under the heavy hitter.

## Success criteria (v0)

1. **Staging file exists** at `futon3a/data/pattern-mining-candidates.edn` (or analogous), with v0 schema-version + provenance metadata.
2. **At least one diagnostic pass landed** — output captures the per-collection signals named in §Scope step 1.
3. **At least three candidate proposals** — covering at least one silent-library remedy, one implicit-pattern surface, and one retirement (or three of the same kind if that's where the evidence points).
4. **One round-trip demonstrated** — at least one operator-approved candidate lands in the canonical library via `pattern_author`.
5. **Patterns view shifts** — the next WM scheduler tick after the merge shows a measurable change in the patterns block (new collection present, or a previously-0 collection now shows activations, or a previously-active one re-balances).  This is the **LOOP closure witness** — direct empirical evidence that mining → library → READ chain is wired end-to-end.
6. **Bell-back to claude-1** when the round-trip lands.

## What the owning agent owns end-to-end

- Pick form (peripheral vs batch job) and scope.
- Author the staging substrate schema.
- Implement the diagnostic pass against live WM data + evidence store.
- Implement the candidate-emission logic.
- Demonstrate the round-trip on at least one real candidate.
- Bell claude-1 with results so the M-war-machine-pilot v1 substrate-thread `:st/pattern-mining-loop-closure` (to be added) can be marked discharged.
- If the work surfaces a follow-on (e.g., embedding-method-as-artefact, separate retrieval path for hostile-by-construction collections), draft a stub `.md` and bell back — do NOT build the follow-on.

## What the owning agent does NOT own

- **Canonical pattern library writes** outside the operator-approved channel.  All canonical library writes route through `pattern_author` per operator approval.
- **Retrieval-engine modifications** in futon3a beyond what mining needs to query — mining READS retrieval; modifying retrieval is a separate excursion if needed.
- **Activation log structural changes** — those live in `derive-pattern-activations` (futon3c) and are claude-1 / codex territory.
- **The bigger M-pattern-mining and M-pattern-application-diagnostic missions** — this excursion is a single operational round-trip; the broader missions remain in IDENTIFY for their full multi-phase work.

## Cross-references

- `M-war-machine-pilot.md` — parent mission; this excursion closes the LOOP step of its operational REPL apparatus.
- `M-pilot-appearance.md` §8.x — canonical REPL alignment (Read / Eval / Print / Loop ↔ AIF / capability-class / typed-artefact / substrate-update).
- `M-pattern-mining.md` (futon3) — substantive sibling mission; this excursion is its first operational v0.
- `M-pattern-application-diagnostic.md` (futon3) — substantive sibling mission; this excursion's success criteria #5 satisfies one of its IFR observable indicators.
- `futon3a/src/futon/peripheral/pattern_author.clj` — existing peripheral the merge step routes through.
- `futon3c/src/futon3c/transport/http.clj` `derive-pattern-activations` — live data source for diagnostic pass.
- `E-street-sweeper.md`, `E-night-shift.md`, `E-pilot-vsatarcs-feed.md` — sibling excursions completing the operational REPL family.

## Pattern emerging — the operational REPL is now nameable

With this excursion landing, the M-war-machine-pilot v1 substrate carries a **named four-step operational loop** that mirrors the REPL discipline articulated in M-pilot-appearance:

| REPL step | Excursion | Capability class | Core safety invariant |
|---|---|---|---|
| READ  | (apparatus core) | AIF/WM signal intake | structural — operator-observable, never bypasses inhabitation |
| EVAL  | E-night-shift | code modification | branch isolation + PR-as-deliverable |
| PRINT | E-street-sweeper + E-pilot-vsatarcs-feed | typed-artefact emission | secret-pattern blacklist / idempotency / no-overwrite |
| LOOP  | E-pattern-mining (this excursion) | substrate update | no-direct-library-mutation + operator-approved-merge |

That's the operational REPL Joe named.  Worth canonicalising as a memory after this excursion ships.

## Provenance

- Operator framing: Joe via emacs-repl, 2026-05-25, while watching the live blue-yellow-green patterns view's clear evidence of 0-activation library zones.
- Excursion authored: claude-1 via emacs-repl, 2026-05-25, while inhabiting `:war-machine-pilot`.
- M-pilot-appearance §8.x REPL alignment cited as the canonical framing this excursion operationalises.
- E-prefix convention: see `[[project_e_prefix_excursions]]` and sibling excursions.
