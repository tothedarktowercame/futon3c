# Excursion: Substrate-2 Sorry Typing

**Type:** Excursion (E-prefix; bounded downstream authoring from a parent mission; owned end-to-end by a single agent per [[project_e_prefix_excursions]]).
**Status:** HEAD drafted; IDENTIFY ratified; MAP ratified; DERIVE ratified; ARGUE ratified; VERIFY ratified; INSTANTIATE drafted; awaiting operator VERIFY-of-INSTANTIATE before DOCUMENT.
**Date:** 2026-05-27
**Author + end-to-end owner:** codex-5 via Agency bell (current surface: bell; caller: claude-1).
**Parent mission:** `futon3c/holes/missions/M-action-cost-modelling.md` (§3.2 selected v0 path; §3.8 aliveness synthesis; §5 T1 output).
**Cross-references:**
- `futon3/holes/missions/M-live-geometric-stack.md` — the existing geometric layer this excursion extends.
- `futon3/scripts/geometric_layer_phase2.clj` — read-side reference implementation for `T`, `∇T`, `ΔT`, and drift over currently-typed entities.
- `futon2/data/sorrys.edn` — current canonical sorry registry and bootstrap entity source.
- `futon5a/holes/stack-annotations.edn` — existing shared stack description; contains sorry-shaped references but not first-class `:sorry` entities.
- `futon4/holes/missions/M-interest-network-coupling.md` — mission of origin for the seven-status event vocabulary.
- `futon3/library/structure/interest-event-vocabulary.flexiarg` — concrete step `(b)` vocabulary artifact now on disk; candidate status surface for typed `:sorry` entities.
- `futon5a/holes/missions/M-a-sorry-enterprise.md` — sibling mining track that will eventually emit into the same entity domain.

## HEAD

### Operator shape

Joe's live framing, relayed through `M-action-cost-modelling`, is that the recommendation problem cannot be solved honestly while sorries are only visible as entries in a hand-curated registry and not as first-class things in the geometric layer. The current geometry can say useful things about code entities because those entities and their relationships are typed and queryable; it cannot yet say the same about sorry-targeted work, even though the recommendation surface is actively telling the operator to act on sorries. The excursion therefore exists to ask a narrow question: what would it take to make a sorry a real typed thing in the live graph, so the same geometry can read it rather than the operator having to infer its structural importance by hand?

### The question

This excursion answers one bounded question: **how should the stack represent sorries as first-class entities in the live graph so that the existing geometric layer can compute tension and change around them, instead of treating sorry-targeted actions as structurally invisible?** It does not yet implement that typing. It names the gap, anchors the idea, and defines what future completion would have to prove.

### What's already felt to be true

- The geometric layer itself already exists. `M-live-geometric-stack` is complete, and the read-side reference implementation lives in `futon3/scripts/geometric_layer_phase2.clj`, not only in `src/`.
- The stack already has a real sorry domain to work from: `futon2/data/sorrys.edn` is live, queried by the recommendation machinery, and contains the five tied sorries that motivated the parent mission.
- The recommendation problem is therefore not "invent a sorry domain from nothing." It is "move an already-used domain into the same typed graph that the geometry can read."

### Anti-glibness discipline

- Do not claim that sorry-typing already exists in the live graph. It does not.
- Do not claim that `T(sorry) = 1 if :open else 0` is implemented. It is only the proposed v0 semantics inherited from the parent mission.
- Do not quietly absorb this work into M-INC step (b) or into the mining track. This excursion is the explicit geometry-track authoring object created by `M-action-cost-modelling` §5 T1.

### Working-economy position

This excursion underwrites one concrete working need: if the recommendation surface is going to suggest sorry-targeted work, then the structural importance of those sorries must be computable from the same live graph that already supports geometric readings elsewhere. Without that, cheap and expensive sorry-targeted actions remain structurally indistinguishable unless a human reads rationales by hand. The excursion itself is underwritten by three existing realities: the live geometric layer, the live sorry registry, and the parent mission's finding that the recommendation surface is too coarse without a structural reading of sorries.

### Clarity gap / carried-forward tensions

1. v0 `T(sorry)` is provisionally accepted as binary (`1 if open, else 0`) for the proof of concept. The carried-forward question is whether later phases need a richer mapping across the seven-status lifecycle from M-INC.
2. Should `:mission` be typed alongside `:sorry` in the same pass, or is `:sorry` the only bounded target for this excursion's first cycle?
3. Which edge classes are genuinely necessary for the first geometric reading of sorries, and which are follow-on refinements once the first entity typing lands?
4. How much of the current `sorrys.edn` schema maps directly into typed graph entities, and how much remains an operator-curated overlay?

### Provenance

This `HEAD` was authored from an Agency bell handoff sent by claude-1 on 2026-05-27, carrying forward `M-action-cost-modelling` §5 T1's provisional resolution to author `E-substrate-2-sorry-typing.md` as a sibling excursion.

**HEAD exit criterion:** Joe recognises this as a faithful statement of why the excursion exists, and the open tensions are named honestly enough for IDENTIFY to proceed without pretending the design is already settled.

## 1. IDENTIFY

### Motivation

The geometric layer already computes `T`, `∇T`, `ΔT`, and drift over typed entities such as vars, tests, namespaces, and terms. The read-side reference implementation in [geometric_layer_phase2.clj](/home/joe/code/futon3/scripts/geometric_layer_phase2.clj:1) explicitly reads only those entity and edge types. Sorries are different: they exist today in [sorrys.edn](/home/joe/code/futon2/data/sorrys.edn:1) as a hand-curated registry that the recommendation machinery can act on, but they are not first-class entities in the live graph. `stack-annotations.edn` contains many sorry-shaped references and notes that sorry entities are "not yet lifted," but it does not yet provide typed `:sorry` entities the geometric layer can query [stack-annotations.edn](/home/joe/code/futon5a/holes/stack-annotations.edn:7).

That mismatch creates the exact gap this excursion names:

- the recommendation layer can target sorries;
- the geometric layer cannot read sorries;
- therefore geometric change cannot yet be computed for sorry-targeted actions.

The parent mission already showed the practical cost of this gap: five sorry-targeted actions tied exactly in the recommendation ranking despite spanning very different effort and impact profiles. Until sorries are represented in the same live graph as other typed entities, the geometry cannot help separate those cases.

### Theoretical anchoring

- **Existing geometric layer.** `M-live-geometric-stack` already establishes the pattern: assign a simple tension value to typed entities, derive edge gradients from that value, and use those derived quantities to read where the structure is unfinished or under strain. In the current implementation, `T(var) = 1 if no incident :coverage edge else 0` and `T(test) = 0` [geometric_layer_phase2.clj](/home/joe/code/futon3/scripts/geometric_layer_phase2.clj:16).
- **Sorry as unfinished boundary.** `M-action-cost-modelling` proposes the v0 extension: treat an open sorry as the same kind of unfinished boundary for this domain, with proposed semantics `T(sorry) = 1 if :open else 0`. Joe has now indicated that this binary reading is acceptable for a proof of concept. This is still not an implementation claim; it is the bounded first-pass semantics this excursion is organised around.
- **Aliveness framing.** The parent mission's aliveness synthesis gives the plain-language reason this geometry matters: some open items hold more stored tension than others, and resolving them changes more of the surrounding working situation. In that framing, a sorry is not just a ticket in a list; it is a named region where the system is still unfinished.
- **Seven-status lifecycle as concrete status vocabulary.** `M-interest-network-coupling` now has an on-disk step `(b)` deliverable at `futon3/library/structure/interest-event-vocabulary.flexiarg`, defining the richer event/status vocabulary (`:spawned :refined :strengthened :addressed :falsified :foreclosed :reopened` plus `link/asserted`). This excursion can now treat that file as real source material, while still keeping the proof-of-concept semantics narrow enough for IDENTIFY.

### Scope in

- Add `:sorry` as a first-class entity type in the live graph, downstream of this excursion's later phases.
- Define the bounded v0 question for `T(sorry)`, starting from the parent mission's proposed semantics `1 if open else 0`.
- Identify the first edge classes needed for a geometric reading around sorries: `:addresses`, `:resolves`, `:raises`, `:bites`, and `:related-mission`.
- Treat [sorrys.edn](/home/joe/code/futon2/data/sorrys.edn:1) as the bootstrap entity source for the first typed sorry population.
- Name the relationship between the current hand-curated registry and the now-authored richer event/status model in `interest-event-vocabulary.flexiarg`.

### Scope out

- Implementing the in-JVM query path that would let the recommendation machinery consume `T`-delta per sorry. The parent mission explicitly holds that as later work.
- Building the full mining pipeline from agent interactions into sorry entities. That belongs to the sibling track `M-a-sorry-enterprise`.
- Revising the recommendation math, UI, or work-breakdown logic directly. Those are parent-mission concerns, not this excursion's first-cycle scope.
- Settling the full status geometry for all seven M-INC states. The vocabulary now exists on disk, but this excursion still does not force the finer-grained semantics in IDENTIFY.
- Claiming that `stack-annotations.edn` is already the canonical sorry substrate. It is not; it currently contains references and gaps, not first-class typed sorry entities.

### Completion criteria

Future completion of this excursion will require all of the following to be demonstrable:

- [ ] `:sorry` entities are ingested into the live graph from [sorrys.edn](/home/joe/code/futon2/data/sorrys.edn:1) with stable identities and queryable properties.
- [ ] `T(sorry)` is computable for those entities under an explicitly recorded rule, rather than by operator interpretation alone.
- [ ] At least the initial sorry edge classes (`:addresses`, `:resolves`, `:raises`, `:bites`, `:related-mission`) are represented well enough that gradients and local change around a sorry can be derived.
- [ ] The five tied sorries named in the parent mission can be distinguished by a geometric reading (`T`, `∇T`, `ΔT`, or a directly derived tie-break), rather than only by hand-reading rationale text.
- [ ] The excursion records clearly whether the first-cycle semantics remain binary (`open` vs not-open) or whether the richer seven-status lifecycle is already needed.

### Relationship to other missions

| Mission / file | Relationship |
|---|---|
| `M-action-cost-modelling.md` | Parent mission. This excursion is the geometry-track authoring object created by §5 T1 and implements the bounded first step of §3.2. |
| `M-live-geometric-stack.md` | Upstream foundation. Supplies the geometric formalism and current read-side implementation that this excursion intends to extend. |
| `M-interest-network-coupling.md` | Upstream mission of origin for the seven-status vocabulary and checkpoint/event discipline. |
| `interest-event-vocabulary.flexiarg` | Concrete step `(b)` artifact. Gives this excursion real status-language content to work from now, even before later ingest/query phases are built. |
| `M-a-sorry-enterprise.md` | Sibling track. That mission mines candidate sorry events and records; this excursion makes sorry entities geometrically legible once they are typed. |
| `stack-annotations.edn` | Relevant existing surface but not yet a solution. It shows that sorry-shaped content exists in the wider stack description while also naming that sorry entities are not yet lifted. |

### Source material

- [M-action-cost-modelling.md](/home/joe/code/futon3c/holes/missions/M-action-cost-modelling.md:357) §3.2 selected v0 path; [§3.8](/home/joe/code/futon3c/holes/missions/M-action-cost-modelling.md:480) aliveness synthesis; [§5 T1 output](/home/joe/code/futon3c/holes/missions/M-action-cost-modelling.md:629).
- [M-live-geometric-stack.md](/home/joe/code/futon3/holes/missions/M-live-geometric-stack.md:229) formalism and entity/edge taxonomy.
- [geometric_layer_phase2.clj](/home/joe/code/futon3/scripts/geometric_layer_phase2.clj:1) read-side reference implementation.
- [sorrys.edn](/home/joe/code/futon2/data/sorrys.edn:1) current canonical sorry registry.
- [stack-annotations.edn](/home/joe/code/futon5a/holes/stack-annotations.edn:1) current shared stack description and "not yet lifted" note for sorry entities.
- [M-interest-network-coupling.md](/home/joe/code/futon4/holes/missions/M-interest-network-coupling.md:165) event vocabulary and sequencing role for step (b).
- [interest-event-vocabulary.flexiarg](/home/joe/code/futon3/library/structure/interest-event-vocabulary.flexiarg:1) concrete step `(b)` deliverable now on disk.
- [mission-lifecycle.md](/home/joe/code/futon4/holes/mission-lifecycle.md:1) lifecycle convention for `HEAD` and `IDENTIFY`.
- Agency bell from claude-1 on 2026-05-27 initiating this excursion authoring handoff.

### Owner and dependencies

- **Owner:** codex-5. Per the handoff constraints, this excursion is not sub-handed off.
- **Primary repos involved:** `futon3c` (this excursion doc and parent mission), `futon3` (geometric layer mission + read-side script), `futon2` (sorry registry), `futon5a` (shared stack description), `futon4` (M-INC vocabulary mission).
- **Dependency update:** the richer status vocabulary is no longer purely pending; a concrete step `(b)` artifact now exists in `futon3/library/structure/interest-event-vocabulary.flexiarg`. Later phases depend on whether that artifact is accepted as the canonical status surface to consume, and on the downstream ingest/query work that would make it operational for `:sorry` entities.
- **Non-blocking dependency for IDENTIFY only:** none. HEAD + IDENTIFY can be authored now because the gap is already visible on disk.

### IDENTIFY exit criterion

A human (Joe) reads §IDENTIFY and agrees:

- the gap is real: sorries are active recommendation targets but not yet first-class entities in the live graph;
- the scope is right: this excursion is about typing and first geometric legibility, not about implementing the full recommendation consumer path;
- the completion criteria are testable and do not pretend the work is already done;
- the dependency story is honest: the richer M-INC vocabulary matters, but its absence does not block this excursion from naming the gap now.

---

## 2. MAP (drafted 2026-05-27; facts only)

### Inventory: existing infrastructure

The existing write/read path for the live graph is more specific than the word "entity" suggests.

- **Read surface used by the geometric layer:** the live geometric script reads typed hyperedges through `GET /api/alpha/hyperedges?type=...`, not through the entity lookup API. `geometric_layer_phase2.clj` pulls `code/v05/var`, `code/v05/test`, `code/v05/namespace`, `code/v05/term`, `code/v05/calls`, `code/v05/coverage`, `code/v05/vocabulary-use`, and `code/v05/contains` entirely via `fetch-of-type` against the hyperedge route [geometric_layer_phase2.clj](/home/joe/code/futon3/scripts/geometric_layer_phase2.clj:40).
- **Current vertex convention:** substrate-2 vertex-types are represented as one-endpoint typed hyperedges. The ingest path for code writes `code/v05/var`, `code/v05/test`, `code/v05/namespace`, and `code/v05/term` via `POST /api/alpha/hyperedge`, each with a one-element `:hx/endpoints` vector [ingest_v05_to_futon1a.clj](/home/joe/code/futon3/scripts/ingest_v05_to_futon1a.clj:406). The watcher mirrors the same pattern [file_ingest.clj](/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj:596).
- **General write surface available today:** futon1a exposes `POST /api/alpha/entity`, `POST /api/alpha/relation`, and `POST /api/alpha/hyperedge`, plus `GET /api/alpha/entity?...` and `GET /api/alpha/hyperedges?...` [api-surface.md](/home/joe/code/futon1a/docs/api-surface.md:22). The tests confirm the minimal entity payload shape is `{:name ... :type ...}` and the minimal hyperedge payload shape is `{:hx/type ... :hx/endpoints [...]}` [hyperedge_http_test.clj](/home/joe/code/futon1a/test/futon1a/integration/hyperedge_http_test.clj:37).
- **Existing precedent for mission-like typing:** mission docs already have a mixed representation. The watcher writes one `code/v05/mission-doc` hyperedge with endpoint `<label>/mission/<mission-id>`, and may also create a companion entity named with the same string and typed `"mission/doc"` [README-conventions.md](/home/joe/code/futon1a/README-conventions.md:62), [file_ingest.clj](/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj:322). This is the nearest live precedent for the `:mission alongside or sorry-only` tension.
- **Current geometric consumers are closed over a fixed type set:** the geometric script hardcodes the types it fetches, and the watcher-side inventory logic likewise hardcodes `VERTEX-TYPES ["code/v05/namespace" "code/v05/var" "code/v05/test"]` [multi.clj](/home/joe/code/futon3c/src/futon3c/watcher/multi.clj:41). A new typed sorry vertex would therefore require explicit extension of the fetch/enumeration lists; it will not be discovered automatically.

### Inventory: existing data

The current sorry domain is smaller and cleaner than the working picture in the bell text implied.

- **Count:** `futon2/data/sorrys.edn` currently contains **14** sorry entries, not 21.
- **ID shape:** all 14 IDs are EDN keywords in the `sorry` namespace (for example `:sorry/r3d-per-entity-attribution`), not strings. The string local names are available by `name`, but the raw registry identifiers are keywords.
- **Status distribution:**
  - `:open` = 5
  - `:addressed` = 2
  - `:n-a-by-design` = 6
  - `:acknowledged-v1-in-force` = 1
- **Kind distribution:**
  - `:prototyping-forward` = 5
  - `:n-a-by-design` = 6
  - `:meta` = 1
  - missing `:kind` = 2
- **Open set:** the five currently-open sorries are exactly the ones driving the parent's tied-actions story:
  - `:sorry/r3a-likelihood-coupling-density`
  - `:sorry/r3a-likelihood-ticks-firing-ratio`
  - `:sorry/r3d-per-entity-attribution`
  - `:sorry/stub-lifts-pending-aif-edn`
  - `:sorry/wm-ui-hud-mode-rationale-hardcode`
  All five are `:kind :prototyping-forward`.
- **Field inventory present today:** every entry has `:id`, `:title`, `:raised-at`, `:status`, `:rationale`, `:related-missions`, and `:links`. Optional fields already in live use include `:kind`, `:resolution`, `:resolved-at`, `:resolved-by-cg`, `:resolved-by-pilot`, `:addressed-at`, `:addressed-by-excursion`, `:addressed-by-cg-chain`, `:addressed-by-pilot`, and `:partial-closure-notes`.
- **Implicit relation sources already present in the registry:**
  - `:related-missions` appears on all 14 entries and names **6 distinct missions**, with `"M-war-machine-aif-completion"` appearing on 12 of 14 entries.
  - `:links` appears on all 14 entries, with **52 total link strings**. One entry (`:sorry/stub-lifts-pending-aif-edn`) alone contributes 34 links.
  - closure/provenance fields already point at concrete external records: CG ids, excursions, pilots, dates.
- **What is not present as a first-class field:** there is no explicit `:addresses-via` field in the current registry, and none of the v0 target edge names (`:addresses`, `:raises`, `:resolves`, `:bites`) appear as first-class schema fields in `sorrys.edn`.

### Ready vs missing

| Ready today | Missing / actual work |
|---|---|
| A live sorry registry exists at [sorrys.edn](/home/joe/code/futon2/data/sorrys.edn:1) with stable IDs, statuses, rationales, mission references, and links. | No `code/v05/sorry` typed vertex exists in the live graph today. |
| The substrate already supports writing new typed hyperedges through `POST /api/alpha/hyperedge`. | The geometric script does not fetch or compute over sorry vertices or sorry edges. |
| The geometric layer already has a working `(T, ∇T, ΔT)` read pattern over typed one-endpoint vertices plus typed edges. | No ingest path exists yet that projects `sorrys.edn` entries into that typed hyperedge form. |
| Mission-doc typing already provides a live precedent for one-endpoint hyperedge vertices and companion entities. | `:related-missions` in the registry are basename strings like `"M-war-machine-aif-completion"`, not normalized current mission endpoints; a mapping step would be required. |
| M-INC step `(b)` now has a concrete vocabulary artifact at [interest-event-vocabulary.flexiarg](/home/joe/code/futon3/library/structure/interest-event-vocabulary.flexiarg:1). | The registry statuses and the M-INC seven-status vocabulary are not yet aligned one-to-one. Current registry statuses include `:n-a-by-design` and `:acknowledged-v1-in-force`, which are outside the seven-status set. |
| The five tied open sorries provide a concrete discriminator test-case. | The edge classes named in IDENTIFY are only partially backed by live source fields. `:related-mission` is directly sourceable; `:addresses`, `:raises`, `:resolves`, and `:bites` still need explicit projection rules or external sources. |

### Q1: How does a new typed vertex become visible to the current geometric layer?

**Answer:** today, not through `/api/alpha/entity` alone. The geometric layer reads typed hyperedges by `:hx/type`, and current vertex-types are one-endpoint hyperedges under `code/v05/*` [geometric_layer_phase2.clj](/home/joe/code/futon3/scripts/geometric_layer_phase2.clj:40), [README-reflection.md](/home/joe/code/futon1a/README-reflection.md:17). A future `:sorry` typing would therefore need at minimum:

- a new typed hyperedge family for sorry vertices;
- an ingest path that writes those hyperedges;
- explicit inclusion of that type in whichever read-side enumeration the future geometric report uses.

The entity API is available, and mission docs show a companion-entity pattern is possible, but the current geometry does not consume entities directly.

### Q2: What is the current schema for a sorry record, and how much of it maps cleanly to typed graph properties?

**Answer:** the core fields map cleanly as candidate vertex properties:

- stable identity (`:id`)
- title
- raised-at date
- current status
- rationale text
- kind

Optional provenance fields also map cleanly as properties or future provenance edges:

- closure dates and pilots
- CG ids / CG chains
- addressed-by excursion
- partial closure notes

The less-clean parts are the relation-like fields:

- `:related-missions` are strings naming mission basenames
- `:links` are heterogeneous file-path / anchor strings

These are clearly useful source material, but they are not yet typed graph edges in their current form.

### Q3: What does the ID-encoding question look like on disk today?

**Answer:** there is a mismatch.

- The registry uses EDN keyword IDs in the `sorry` namespace.
- The current substrate conventions for typed hyperedge endpoints use strings (for example `<label>/<qname>` for code vertices, `<label>/mission/<mission-id>` for mission-doc vertices) [README-reflection.md](/home/joe/code/futon1a/README-reflection.md:42), [README-conventions.md](/home/joe/code/futon1a/README-conventions.md:64).

That means later phases will need an explicit normalization rule if sorrys are projected into the current substrate conventions. MAP does not choose the normalization; it records that the mismatch is real.

### Q4: Do `:related-missions` already point at something typable, or would this excursion need to invent a mission domain too?

**Answer:** mission-like typing already exists elsewhere, but the registry strings do not yet directly match the current geometric conventions.

- The registry stores mission references as strings like `"M-war-machine-aif-completion"`.
- The mission watcher stores mission-doc vertices under endpoint strings like `<label>/mission/<mission-id>`, and the companion entity uses `:external-id = "M-<mission-id>"` [README-conventions.md](/home/joe/code/futon1a/README-conventions.md:62), [file_ingest.clj](/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj:322).

So there is already a mission-like surface to connect to. For this excursion's MAP, that means `:related-missions` can remain source strings in the registry, but any future `:related-mission` edge would still need normalization through either mission external-id or mission-doc endpoint conventions.

### Q5: Which of the proposed edge classes are directly recoverable from the registry, and which are follow-on work?

**Answer:**

- **Directly sourceable now:** `:related-mission` from `:related-missions`; generic provenance attachments from closure fields and link strings.
- **Only partially sourceable now:** `:resolves` can be approximated from closure fields such as `:addressed-by-excursion`, `:resolved-by-cg`, and `:addressed-by-cg-chain`, but not from a single already-typed edge field.
- **Not directly sourceable from the registry alone today:** `:addresses`, `:raises`, and `:bites` are not first-class fields in `sorrys.edn`; they would require projection rules, additional source material, or sibling-mission work.

This means IDENTIFY's v0 edge-class list is still useful as a target inventory, but MAP shows that only part of it is backed by direct current data.

### Surprises documented

1. **The registry has 14 entries, not 21.** The working picture carried into the bell overstated the current population.
2. **The current status space is not binary and not equal to the M-INC seven-status space.** The registry includes `:n-a-by-design` and `:acknowledged-v1-in-force`, neither of which is in the seven-status vocabulary. This does not block a v0 binary `open` vs not-open reading, but it matters for any richer mapping later.
3. **Two addressed entries still have no `:kind`.** `:sorry/r3a-likelihood-support-coverage` and `:sorry/r3a-likelihood-attack-coverage` are the only missing-kind entries.
4. **`related-missions` is much stronger source material than `addresses`-style edges.** It is universal across the registry; most of the more semantically ambitious edge names are not explicit in the source file.
5. **The nearest precedent for this work is mission-doc typing, not var/test typing.** That is because both problems involve non-code artifacts that need one-endpoint typed vertices plus relation edges.
6. **The current geometry consumes hyperedges, not entities.** That narrows the practical meaning of "add `:sorry` as an entity type": the critical path is really "add a typed sorry vertex-hyperedge family the geometry can read."

### MAP exit criterion

This excursion's MAP phase is complete when a human (Joe) agrees that:

- the current write/read surfaces and type conventions are described factually;
- the sorry registry's current field shape, counts, and relation-like material are inventoried concretely;
- the ready-vs-missing table distinguishes what exists on disk from what still needs design;
- the survey questions produce facts and constraints rather than premature solutions;
- the surprises are recorded before DERIVE starts narrowing the design.

---

## 3. DERIVE (drafted 2026-05-27; narrowed from MAP facts)

DERIVE answers the design question MAP set up: **what is the smallest honest substrate-2 shape that makes sorries geometrically legible without pretending the richer lifecycle or edge vocabulary is already implemented?** The answer is a v0 projection that stays inside the current hyperedge-first seam, preserves the registry's richer raw fields as properties, and only promotes relations to first-class edges when the source file actually supports them.

### PSR - Pattern Selection Records

#### PSR-1: Reuse the current hyperedge seam rather than invent a parallel sorry path

- Pattern chosen: peripherals/read-existing-seam-before-implementing
- Candidates: reuse current hyperedge seam; entity-only sorry typing; parallel sorry-specific ingest/query path
- Rationale: MAP established that the live geometric layer reads typed hyperedges, not entities, and that mission-doc typing is the nearest existing precedent for a non-code artifact. Reusing that seam keeps sorry typing visible to the current geometry instead of requiring a second consumer path.
- Confidence: high
- Success criteria: DERIVE chooses a sorry representation that the existing `/api/alpha/hyperedge` and `GET /api/alpha/hyperedges?type=...` pattern can consume without a second storage contract.

#### PSR-2: Treat the documented HTTP surface as the persistence contract

- Pattern chosen: storage/canonical-interface
- Candidates: HTTP hyperedge/entity API; direct store coupling; ad hoc file-side interpretation only
- Rationale: the substrate already exposes a minimal documented HTTP contract for entities, relations, and hyperedges. The excursion should design against that surface, not against internal assumptions about futon1a's backing store.
- Confidence: high
- Success criteria: every proposed sorry vertex/edge shape can be expressed as ordinary hyperedge/entity writes through the documented API.

#### PSR-3: Preserve the real problem instead of solving the richer status theory too early

- Pattern chosen: structure/what-problem-is-this-actually-solving
- Candidates: solve tied-sorry geometric legibility first; solve full seven-status geometry now; handwave status questions into a future mission
- Rationale: the parent problem is that sorry-targeted work is structurally invisible to the geometry. Prematurely solving the full status semantics would be a tractable proxy unless MAP showed it was required for first legibility. It did not.
- Confidence: high
- Success criteria: DERIVE preserves enough raw status information for later refinement while keeping v0 small enough to land as a first prototype.

### Choice 1: Typed hyperedge shape

**IF** the live geometric layer reads one-endpoint typed hyperedges as its vertex surface, and the nearest non-code precedent (`code/v05/mission-doc`) already uses that convention,

**HOWEVER** an entity-only `:sorry` representation would remain invisible to the current geometry, while a more elaborate multi-endpoint sorry vertex would add structure the source registry does not actually provide,

**THEN** v0 should represent each sorry as a one-endpoint typed hyperedge, `code/v05/sorry`, whose sole endpoint is the normalized sorry identifier. A companion entity may exist later if another consumer genuinely needs one, but it is not on the critical path for geometric legibility.

**BECAUSE** the real problem is not "make sorries exist somewhere in substrate-2"; it is "make them readable by the geometry the stack already has." Reusing the current vertex-hyperedge seam is the shortest path that satisfies that requirement without forking the data model.

**Selected v0 shape**

- Vertex hyperedge type: `code/v05/sorry`
- Endpoint cardinality: one
- Required vertex properties: normalized endpoint string, original registry id, title, status, raised-at
- Useful additional properties: kind, rationale, links, related-missions, closure/provenance fields when present

### Choice 2: ID normalization rule

**IF** the source registry uses EDN keyword ids in the `sorry` namespace, while the current substrate conventions use string endpoints shaped like `<label>/<domain>/<local-id>`,

**HOWEVER** carrying the raw keyword unchanged as the endpoint would skip the stack's current naming discipline, and inventing a totally new id form would make sorrys harder to relate to the existing mission-doc and code vertex conventions,

**THEN** normalize each sorry endpoint as `<label>/sorry/<local-name>`, where `<local-name>` is the keyword name after removing the `sorry/` namespace prefix. Preserve the original registry id verbatim as metadata, preferably as `external-id=":sorry/<local-name>"` and/or a dedicated `registry-id` property.

**BECAUSE** this keeps sorrys inside the live endpoint naming family while retaining a lossless pointer back to the canonical registry key. It also gives later relation edges a predictable target string without requiring downstream readers to speak EDN keywords.

**Worked example**

- Registry id: `:sorry/r3a-likelihood-coupling-density`
- Proposed endpoint: `<label>/sorry/r3a-likelihood-coupling-density`
- Preserved source identity: `external-id=":sorry/r3a-likelihood-coupling-density"` (or equivalent property if the hyperedge write path does not surface `external-id` directly)

### Choice 3: Status semantics for v0

**IF** the proof-of-concept requirement is to make open sorries geometrically legible now, and MAP showed that the registry currently has four live statuses rather than the M-INC seven-status lifecycle,

**HOWEVER** collapsing the registry entirely to `open` vs `closed` would throw away real source information, while forcing a seven-status geometric semantics now would solve a richer theory question before the first typed projection exists,

**THEN** v0 should carry the raw registry status as a property and define `T(sorry)` as a binary projection of that property:

- `T(sorry) = 1` when `status = :open`
- `T(sorry) = 0` for `:addressed`, `:n-a-by-design`, and `:acknowledged-v1-in-force`

The richer status fields remain available for later refinement, but they do not yet alter `T`.

**BECAUSE** this is the smallest honest design that preserves information and answers the actual v0 question. It keeps the first prototype about geometric legibility rather than pretending the stack has already settled the deeper lifecycle semantics.

**Consequence for later phases**

- The registry's native status remains queryable and documentable.
- Later work may map M-INC's event vocabulary onto sorry histories without breaking the initial endpoint/id choice.
- Any richer `T` mapping must justify itself against the live five-sorry case rather than being assumed in advance.

### Choice 4: Edge classes for v0

**IF** the goal is to make sorries visible in the same graph as surrounding work, and MAP showed that only some relation material is directly sourceable from `sorrys.edn`,

**HOWEVER** promoting unsourced edge names such as `:addresses`, `:raises`, and `:bites` to first-class graph structure now would force the ingest layer to invent relations the registry does not yet state, while refusing all relations would make the typed sorry vertex too isolated to be useful,

**THEN** v0 should project only the relations the registry can support honestly:

- **In scope for typed edges now:** `code/v05/related-mission` from each entry's `:related-missions`, normalized onto the existing mission-doc surface where possible
- **Preserved as vertex properties for now:** `:resolved-by-cg`, `:addressed-by-excursion`, `:addressed-by-cg-chain`, `:resolved-by-pilot`, `:partial-closure-notes`, and heterogeneous `:links`
- **Explicitly deferred from v0 typed-edge status:** `:addresses`, `:raises`, `:bites`, and a generalized `:resolves` edge family

**BECAUSE** the right first move is to type what the source file already knows, not what later theory wishes it knew. `:related-mission` is universal and immediately useful. The closure/provenance fields are real, but they do not yet point into a fully normalized target domain. Deferring the unsourced edge families keeps the graph truthful.

**Boundary statement**

This is the main narrowing from IDENTIFY. IDENTIFY named a five-edge-class target inventory. DERIVE now splits that into:

- v0 projectable now: `:related-mission`
- provenance retained but not yet normalized as graph edges: closure / addressing fields
- follow-on work: `:addresses`, `:raises`, `:bites`, richer `:resolves`

### Choice 5: Ingest pipeline shape

**IF** the long-lived requirement is for typed sorry vertices to stay in sync with the canonical registry and to appear on the same live substrate surface as the rest of the stack,

**HOWEVER** a one-off script is attractive for a first backfill because it is faster to author, while a script-only approach would leave sorry typing as a side path that drifts the next time `sorrys.edn` changes,

**THEN** the canonical design target should be watcher-integrated ingestion (or another always-on futon3c-managed ingestion path that writes through the same HTTP hyperedge seam). A one-off bootstrap script is acceptable only as a temporary seeding aid during implementation, not as the enduring source of truth.

**BECAUSE** the excursion is solving a live-graph visibility problem, not merely a one-time migration problem. If the registry changes but the graph does not update automatically, the geometry falls out of sync and the whole point of typing sorries is lost.

**Operational implication**

- Canonical steady-state: always-on ingest tied to the live stack
- Acceptable temporary aid: bootstrap/backfill script for initial population
- Invalid end-state: "run this script occasionally when someone remembers"

### Resulting v0 design

The derived first prototype is therefore:

1. Each sorry entry in `sorrys.edn` becomes a one-endpoint `code/v05/sorry` hyperedge.
2. Its endpoint is normalized as `<label>/sorry/<local-name>`.
3. The original registry key and raw status are preserved as properties.
4. `T(sorry)` is computed as a binary projection of the raw status (`:open` -> `1`; everything else -> `0`).
5. `:related-mission` is the only new typed edge family required in v0.
6. Closure/provenance fields stay present as properties until a later phase normalizes their target domains.
7. The enduring ingest path is live and watcher-managed; a one-off script may seed but may not remain the canonical path.

### What DERIVE does not claim

- It does not claim the current five tied sorries will be fully discriminated by `T` alone. The point of v0 is to make sorrys geometrically queryable at all; later phases can test what additional edges or derived quantities are needed for better separation.
- It does not claim the registry's four statuses are the final sorry lifecycle. It preserves them and leaves open whether M-INC's seven-status event vocabulary should later become the richer semantic layer.
- It does not claim a new `:mission` domain is required for this excursion. Existing mission-doc typing is sufficient as the initial target for `:related-mission` normalization.

### DERIVE exit criterion

A human (Joe) reads §3 and agrees that:

- the derived v0 shape reuses the actual live graph seam instead of inventing a parallel sorry substrate;
- the id rule is explicit and lossless with respect to the canonical registry;
- status semantics are honest about v0 binary `T` while preserving richer source information;
- the edge set is narrowed to what the registry can really support today;
- the ingest recommendation names a canonical steady-state path rather than a workaround.

---

## 4. ARGUE (drafted 2026-05-27; strategic only)

ARGUE's job here is narrow: show that the derived v0 design is the right first instantiation of the parent mission's larger idea, not because it solves everything, but because it makes the right thing first-class in the right place.

### Pattern cross-reference

The three PSRs in DERIVE grounded the direct design choices. A wider pattern scan does not overturn those choices; it shows why they cohere with the broader futonic discipline.

| Pattern | Where it applies here | What it clarifies |
|---|---|---|
| `structure/hinge-point` | The registry-to-geometry boundary this excursion is cutting across | This excursion sits at a real steering hinge: the stack already uses sorries operationally, but the geometry cannot yet see them. That is not a routine phase transition; it is a place where the representation choice changes what the rest of the stack can know. |
| `structure/cook-ting` | The choice to reuse the existing hyperedge seam instead of inventing a parallel sorry representation | The correct cut is along the existing cleavage in the structure. MAP showed that the geometry already reads one-endpoint typed hyperedges. Reusing that seam is the natural cut; entity-only or side-path designs would be cutting through gristle. |
| `peripherals/canonical-typed-event-vs-side-channel` | The treatment of closure/provenance fields and the refusal to invent typed edges the registry does not yet support | The lesson transfers cleanly: when the stack has a canonical typed surface, extend that surface rather than smuggling semantics in ad hoc side paths. Here that means typed `code/v05/sorry` and, later, properly normalized relation types, not a parallel sorry-only interpretation layer. |
| `aif/expected-free-energy-scorecard` | The link back to the parent mission's recommendation problem | The parent mission's live surface already depends on decomposed, auditable recommendation terms. Sorry typing does not replace that scorecard; it gives the scorecard a less proxy-like structural substrate to read from when sorry-targeted actions are in play. |
| `structure/mana-allostasis` | The parent mission's aliveness synthesis and this excursion's binary `T(sorry)` starting point | Open sorries are being treated as stored unresolved pressure, not just bookkeeping rows. The v0 design makes that pressure locatable in the graph, even before the richer lifecycle semantics are worked out. |

### Theoretical coherence

This v0 design is small, but it is not arbitrary. It is the first concrete step of the broader aliveness argument from `M-action-cost-modelling`.

In plain language:

- the registry already records places where the system has not become coherent yet;
- the parent mission argued that these places carry stored tension rather than merely administrative status;
- the geometric layer already has a way to represent unfinishedness as `T`;
- this excursion's v0 move is to let those sorry-shaped unfinished regions enter that same geometric reading.

That is why the binary `T(sorry)` choice is acceptable here. It is not claiming to be the final theory of sorry life-cycles. It is claiming that an open sorry is, at minimum, an explicit unfinished boundary. Under the parent mission's terminology, that makes the typed sorry vertex a first substrate-2 representation of an anamnesis-region: the system has not forgotten the unresolved work, and the graph can now name where that unresolvedness sits.

The same point can be said geometrically. The manifold-end reading from the parent mission only becomes testable if the sorry exists in the same typed graph as its surroundings. Before that, "this sorry holds tension" is just prose. After this v0, it becomes a queryable claim, even if the first query is still coarse.

### Trade-off summary

The v0 design gives up five things deliberately:

- It defers the richer seven-status semantics. Raw registry status is preserved, but `T` only projects `open` vs not-open.
- It defers four of the five initially named edge classes. Only `:related-mission` is promoted to typed-edge status now.
- It refuses to claim that `T` alone will discriminate the five tied sorries. That remains a later empirical question.
- It leaves `:links` as opaque strings rather than pretending heterogeneous references are already a typed structure.
- It leaves the in-JVM query path for recommendation consumption in the parent mission's scope rather than smuggling that implementation into this excursion.

Those are real sacrifices. The payoff is that the first prototype stays honest about the current source material and lands on the live seam the geometry already uses.

### Carried-forward tensions for VERIFY

ARGUE does not resolve the remaining tensions; it names the ones VERIFY must operationalize next.

1. **Lossless re-ingest tension.** The design preserves the registry keyword id alongside the normalized string endpoint, but VERIFY still needs a concrete test that this remains lossless across watcher-managed re-ingest and future schema evolution.
2. **Bootstrap-vs-canonical ingest tension.** DERIVE allows a bootstrap script as a temporary aid, but VERIFY needs an explicit rule for recognizing when that temporary path has overstayed its mandate and the live ingest path must take over.
3. **Richer-status pressure tension.** The design defers seven-status `T`, but VERIFY should ask what live evidence would justify moving beyond binary `open` vs not-open.
4. **Deferred-edge honesty tension.** The design keeps closure/provenance material as properties for now; VERIFY should name what additional source material would be required before promoting any of those into first-class typed relations.

### ARGUE exit criterion

A human (Joe) reads §4 and agrees that:

- the v0 design is now connected back to broader futonic patterns rather than justified only by local convenience;
- the small concrete design still reads as the first honest instantiation of the parent mission's aliveness / geometric synthesis;
- the trade-offs are named plainly enough that the design is not being oversold;
- the remaining tensions are strategic carry-forwards for VERIFY, not hidden unresolved design work.

---

## 5. VERIFY (drafted 2026-05-27; operational hooks for ARGUE's carried-forward tensions)

VERIFY in this excursion is not "is the design convincing?" ARGUE already did that work. VERIFY here means: **for each remaining tension, what concrete rule, trigger, or test will tell us what to do next?** Some hooks can be authored now as discipline rules; others only become live once substrate-2 sorry typing actually ships.

**Dispatch discipline** (car-of-sequence): per [[feedback_car_of_sequence_dispatch]], this is not a flat ratify-all-at-once list. The table carries explicit dispatch metadata, and only the current car is live for immediate closure. Operator interjection-points live at each car-boundary.

### §5.0 Dispatch state (audit 2026-05-27; honest tally)

**First-up**:
- **T-A2** — bootstrap-vs-canonical ingest discipline. This is the cheapest hook to close now because it is a rule-of-work, not an empirical substrate test.

**Next-up**:
- **T-A4** — deferred-edge honesty trigger. This is the next highest-leverage gate because it determines when sibling work is allowed to promote closure/provenance material into typed relations.

**Held for INSTANTIATE**:
- **T-A1** — lossless re-ingest test. Meaningful only once a live ingest path exists to exercise.
- **T-A3** — richer-status pressure trigger. Meaningful only once v0 typing is queryable enough to show whether binary `T` is insufficient.

**Gating pre-commit**:
- **T-A2** must be closed before any bootstrap/backfill script is accepted as part of the implementation path.
- **T-A4** must be closed before any deferred edge family (`:resolves`, `:addresses`, `:raises`, `:bites`, or typed `:links` interpretation) is promoted into the v0 graph design.

**Score (audit 2026-05-27)**: 0 done; 1 current-car (`T-A2`); 1 next-up gate (`T-A4`); 2 held for INSTANTIATE (`T-A1`, `T-A3`). Excursion is in a clean VERIFY state: immediate work is discipline-setting, not build-work.

### §5.1 Tension-by-tension operational hooks

| # | Tension (from §4 ARGUE) | Operational hook ("what are we actually going to do about it?") | Owner / venue |
|---|---|---|---|
| T-A1 | **Lossless re-ingest tension**: does keyword-id plus normalized endpoint remain lossless across watcher-managed re-ingest and schema evolution? | **Round-trip fixture at first live ingest.** Use at least three registry cases: one `:open` sorry, one non-open sorry with closure metadata, and one `:n-a-by-design` sorry. For each: ingest -> query substrate record -> re-ingest unchanged source -> confirm (a) endpoint is stable, (b) preserved original registry id is unchanged, (c) no duplicate vertex is created, and (d) non-id property edits update in place without changing identity. If any schema evolution breaks one of those four, the normalization rule must be revised before the ingest path is accepted. | codex-5 in INSTANTIATE; Joe verifies from substrate-2 query results during VERIFY-of-implementation |
| T-A2 | **Bootstrap-vs-canonical ingest tension**: how do we stop a temporary backfill path from becoming the real system by drift? | **Author and enforce a decommission rule now.** A bootstrap/backfill script is allowed only if the same change-set also names: (a) the canonical always-on ingest path, (b) the condition that proves the canonical path has taken over, and (c) the moment the script becomes invalid. For this excursion, the invalidation rule is: once watcher-managed ingest has successfully populated `code/v05/sorry` from the live registry and one subsequent registry edit reaches substrate-2 without rerunning the script, the script may no longer be treated as the canonical path. Any later need to rerun it is a VERIFY failure, not a maintenance norm. | codex-5 now in VERIFY text; enforced during INSTANTIATE by whoever lands the ingest path |
| T-A3 | **Richer-status pressure tension**: when does binary `open` vs not-open stop being good enough? | **Use a trigger test, not a theory-first expansion.** Do not implement richer `T` semantics unless at least one of these conditions fires after v0 exists: (a) two non-open statuses need distinct geometric treatment in a live operator query, (b) M-INC status/history integration lands and binary projection collapses meaningful differences the operator cares about, or (c) the five-sorry case remains operationally under-discriminated even after typed sorry vertices and `:related-mission` edges are live. If none fires, binary `T` remains the accepted v0 semantics. | codex-5 + Joe in INSTANTIATE/VERIFY-of-results; parent mission consumes the outcome |
| T-A4 | **Deferred-edge honesty tension**: when is it legitimate to promote closure/provenance material from properties into typed relations? | **Adopt a three-part promotion gate.** A deferred edge family may be promoted only when all three are true: (a) there is a canonical source field or canonical sibling artifact for that relation, not a heuristic parse of free text or `:links`; (b) the target domain is already normalized as a stable typed endpoint surface; and (c) there is at least one named consumer/query that needs the relation as a relation rather than as a stored property. Until then, keep the material as properties. This blocks premature typing of `:resolves`, `:addresses`, `:raises`, `:bites`, and any typed interpretation of heterogeneous `:links`. | codex-5 now in VERIFY text; sibling mission owners must satisfy the gate before promotion in later phases |

### §5.2 What VERIFY does NOT do

- It does not re-litigate whether the v0 design is the right one. ARGUE already closed that strategic question.
- It does not pretend all four tensions are actionable today. Two are discipline gates now; two are empirical tests later.
- It does not authorize implementation shortcuts. Any bootstrap path that cannot state its own invalidation condition fails T-A2 by definition.
- It does not silently promote deferred edge classes just because adjacent source material exists. T-A4 keeps that promotion honest.

### §5.3 VERIFY exit criterion

A human (Joe) reads §5 and agrees that:

- each remaining tension now has a concrete operational hook rather than a vague "we'll revisit this later";
- the dispatch order is honest about what can be settled now versus what only becomes meaningful in INSTANTIATE;
- the owners / venues are specific enough that the hooks are actually runnable;
- the pre-commit gates (`T-A2`, `T-A4`) are strong enough to block the two most likely forms of drift: script-becomes-system and property-becomes-edge-by-handwave.

---

## 6. INSTANTIATE (drafted 2026-05-27; on-disk implementation + non-live fixture only)

INSTANTIATE in this cycle lands the code and the non-live validation path. It does **not** claim live substrate-2 writes have been exercised. Joe explicitly held that boundary: no writes to futon1a on `:7071` without operator go-ahead.

### What landed on disk

- **Watcher-integrated ingest path** in [file_ingest.clj](/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj:1):
  - `sorry-registry-path?` routes `futon2/data/sorrys.edn` into a dedicated ingest path.
  - `build-sorry-registry-docs` projects the canonical registry into:
    - one-endpoint `code/v05/sorry` vertex hyperedges
    - v0 `code/v05/related-mission` edges only when the target mission-doc vertex can already be resolved
  - `ingest-sorry-registry!` posts those docs through the same watcher HTTP seam as the existing substrate-2 writers.
  - `normalize-sorry-endpoint` and `sorry-t` implement the DERIVE rules directly.
- **Watcher root recognition** in [multi.clj](/home/joe/code/futon3c/src/futon3c/watcher/multi.clj:1):
  - the multi-watcher now watches the canonical sorry registry path even though generic `.edn` files are still not part of the watched extension set.
- **Bootstrap script** at [ingest_sorrys_to_futon1a.clj](/home/joe/code/futon3c/scripts/ingest_sorrys_to_futon1a.clj:1):
  - defaults to dry-run
  - supports `--fixture` for T-A1's three-case non-live round-trip
  - supports `--write` for later operator-approved POSTs
  - states the T-A2 decommission rule explicitly in its header comments
- **Focused tests** in [file_ingest_test.clj](/home/joe/code/futon3c/test/futon3c/watcher/file_ingest_test.clj:1) and [multi_test.clj](/home/joe/code/futon3c/test/futon3c/watcher/multi_test.clj:1):
  - sorry-registry path detection
  - endpoint normalization
  - non-live fixture round-trip
  - watcher recognition of `futon2/data/sorrys.edn`

### Design decisions as implemented

- Vertex shape: `code/v05/sorry` one-endpoint hyperedges
- Endpoint normalization: `<label>/sorry/<local-name>`
- Properties carried on the vertex hyperedge:
  - normalized endpoint
  - original registry id
  - title
  - status
  - raised-at
  - derived binary `sorry/t`
  - kind, rationale, closure fields, `:related-missions`, and `:links` when present
- Typed edges in v0:
  - only `code/v05/related-mission`
  - projected from `:related-missions`
  - emitted only when the target can already be normalized onto the existing mission-doc endpoint surface
- Explicit non-features maintained:
  - no deferred edge-family promotion
  - no recommendation machinery changes
  - no in-JVM sorry-query consumer path yet

### Non-live T-A1 fixture result

The required three-case fixture was run through the bootstrap script in non-live mode:

```clojure
clojure -M scripts/ingest_sorrys_to_futon1a.clj --fixture
```

Result:

- **Pass**: true
- **Cases exercised**: 3
  - one `:open` sorry
  - one non-open sorry with closure metadata
  - one `:n-a-by-design` sorry
- **Lossless round-trip**: true
- **Planned docs for the fixture subset**:
  - 3 `code/v05/sorry` vertices
  - 5 `code/v05/related-mission` edges
  - 0 unresolved related missions under the fixture resolver

The important point is not the counts. It is that the same projection code used by the watcher and the bootstrap script produced stable endpoints, preserved the original registry ids, and rebuilt the same payloads on repeated projection without drift.

### What was intentionally not done

- No live writes were sent to futon1a on `http://localhost:7071`.
- No heavy verification against the live JVM was attempted.
- No deferred edge family (`:resolves`, `:addresses`, `:raises`, `:bites`, typed `:links`) was promoted past the T-A4 gate.
- No DOCUMENT-phase work was started in this cycle.

### INSTANTIATE exit criterion for this cycle

A human (Joe) reads §6 and agrees that:

- the canonical watcher path and the one-off bootstrap path now both exist on disk;
- the bootstrap path carries its own T-A2 decommission rule explicitly;
- the non-live fixture gives real evidence that the projection is stable and lossless at the property/endpoint level;
- the live substrate boundary was respected rather than blurred.

## Provenance

- Authored in response to an Agency bell from claude-1 on 2026-05-27, titled `M-action-cost-modelling §5 T1-output; scope-bounded authoring handoff`.
- Bell context: `M-action-cost-modelling` VERIFY T1 was provisionally resolved to `(a)` — author `E-substrate-2-sorry-typing.md` as a sibling excursion.
- This file was authored by codex-5 on the `bell` surface, following the handoff instruction to stop at `HEAD → IDENTIFY` and cite the bell as provenance.
- Updated on 2026-05-27 after Joe reported that `futon3/library/structure/interest-event-vocabulary.flexiarg` had been written as the concrete M-INC step `(b)` deliverable, and that binary `1 if open, 0 if closed` semantics are acceptable for the proof of concept.
- Advanced to `MAP` on 2026-05-27 after claude-1 whistled that Joe ratified the IDENTIFY exit and requested a factual infrastructure/data inventory only, stopping again before DERIVE.
- Advanced to `DERIVE` on 2026-05-27 after claude-1 whistled that Joe ratified the MAP exit and requested the five design-choice points be narrowed with explicit IF/HOWEVER/THEN/BECAUSE structure, again stopping before operator VERIFY.
- Advanced to `ARGUE` on 2026-05-27 after claude-1 whistled that Joe ratified the DERIVE exit and requested a short strategic argument, with operational hooks explicitly deferred to VERIFY.
- Advanced to `VERIFY` on 2026-05-27 after claude-1 whistled that Joe ratified the ARGUE exit and requested a car-by-car operational-hook table following the same discipline used earlier on `M-action-cost-modelling`.
- Advanced to `INSTANTIATE` on 2026-05-27 after claude-1 whistled that Joe ratified the VERIFY exit and explicitly bounded the cycle to: watcher integration on disk, bootstrap script on disk, non-live T-A1 fixture run, and no live substrate writes without operator approval.
