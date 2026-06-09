# Mission: M-mission-scopes-into-substrate-2

**Date:** 2026-06-08
**Status:** HEAD authored / IDENTIFY (charter). Greenlit by Joe 2026-06-08.
**Owner:** claude-3 (mission-scope hypergraph + Salingaros field) · claude-1 (WM EFE — Deliverable 2 is
co-owned with the pudding-engine) · codex (the ingest).
**Predecessor:** M-web-arxana-missions (futon4) Layer-3 — the session of *play* (the mission carpet,
2026-06-08) that surfaced this. The name is deliberate: missions are **already** in substrate-2 (as nodes);
it is their **scopes** (the scope-tree-of-centres) that are not.
**Co-owns / touches:** C-pudding-prover, E-warranted-play, the WM gap-reader / pre-witness loop (futon3c/futon7).

## 1. IDENTIFY — the gap

Substrate-2 (the Arxana hypergraph, port 7071) already reflects code **contents** (substrate-1). What it lacks
is the mission **scopes** — the eightfold phases, sub-scopes and bound concepts that M-web-arxana-missions
Layer-3 computes. Today those scopes are produced **ad-hoc and position-indexed**
(`futon6/scripts/mission_scope_detect.py` emits `hx/content {position,end}` char-offsets) — ephemeral,
recomputed each run, living *outside* substrate-2. Two consequences:

- The Salingaros aliveness/entropy field (`futon6/scripts/mission_wholeness.py`, `L = T·H`, `C = T·(10−H)`,
  where `(10−H)` is architectural entropy) is a **document-derived** read, **blind to substrate-1**: a fat code
  file is a real refactor-sorry (high entropy) that no scope-tree sees, because the scopes aren't in the same
  hypergraph as the code.
- The field can't be composed with the **War Machine's EFE**, which is the *same notional quantity*
  (free energy ≈ entropy ≈ `C`) computed a **different way** — live code/agenda state, not mission documents.

**Two computations of one notional entropy, never unified.** This mission unifies them.

## 2. Deliverables

### D1 — Mission scopes as real, verbatim-anchored hyperedges in substrate-2  *(the knowable half)*
Import the 194 mission-scope-trees into substrate-2 as durable hyperedges, anchored so they survive edits:
- **scope-id** = `<mission-stem>/<heading-slug>` — stable identity across reordering / small edits;
- **anchor** = the heading **verbatim passage** + a short content **fingerprint**, re-resolved by *search*,
  **never** by buffer/char position (the README-essays.md convention; positions rot on the first edit);
- **maintenance:** fuzzy re-resolve when a heading is reworded; an editorial-log **`:detached`** state when an
  anchor can't resolve (review, never silent loss); a **watcher-driven re-detect** on mission save → diff
  stored-vs-new scopes (added / removed / moved) → patch the hypergraph.
- **Builds on:** codex-1's `mission_scope_ingest.clj` (futon3c), which already ingested the 6 ensemble
  scope-trees into futon1a/substrate-2 for the L1 `/ego` render — extend to 194 + swap positional anchors for
  verbatim+scope-id+fingerprint.

### D2 — Compose the Salingaros C-field with the WM EFE  *(the unknown — why this is a mission)*
With missions and code in one substrate-2, run the Salingaros entropy field over the **unified** hypergraph
(a fat file and an empty phase are both high-C now) and compose it with the WM EFE as a **layered drive**:
- **macro** — the C-field = a *where-to-attend* prior over the whole substrate-2 (cheap, document+code derived);
- **micro** — substrate-1 code grain (fat files), within-scope holes, near-miss genes = the *actual buildable
  action* (not "finish the mission", which is what you get when you stop at the macro);
- **live-filter** — footfall / WM live-state, to drop stale high-C (e.g. `M-reachable-from-boot`, a forgotten
  mission the static field over-surfaces).

**The open question this mission exists to answer:** *how do the two entropy computations compose into one
drive without one merely overriding the other?* (additive term? a prior the WM samples? macro scopes / micro
acts / live filters?). This is the unknown specification — the reason it's a mission, not a task.

**Salvo resolution (claude-3 ⟷ claude-1, short-whistle salvo 2026-06-08) — answered:**
- **MACRO where-prior = full-C** (`T·(10−H)`): it **subsumes and retires the additive gap-term**, lifting it
  OUT of `G-total`. Rationale (today's F1 lesson): the additive-C **is** the gap-reader we de-saturated this
  morning; an additive C-term re-runs that saturation/mis-steer bug. The gap was "a *where* miscast as a
  *what*"; full-C puts it at the right altitude — a scoping prior, not an EFE term. One non-fragile mechanism.
- **MICRO action = clean `G-total`/EFE** over substrate-1 code + scope-holes — C never names the move.
- **LIVE-FILTER = attested-footfall + WM live-agenda** → drop stale high-C (e.g. `M-reachable-from-boot`).
  Footfall is *liveness, not progress* — and that's **correct** for drop-stale: the filter's job is to
  not-attend **dead** regions, and a thrashed mission is at least alive (don't drop it). Keep it **attested**
  (real logged turns) — same anti-laundering discipline as the fruit-witness, or "footfall" becomes a
  launderable proxy.
- **Transition guard (subsumption-discipline):** retiring the live F1 gap-reader is the **end-state**, not an
  immediate rip-out. It is **both-live-gated** — F1 stays live until full-C demonstrably subsumes its function
  on the field (the domain-gate AND the de-saturation) — and the swap is **Joe's consent locus**. Nothing
  leaves the live EFE before its replacement is proven; nothing touches the live EFE without Joe.
- Symmetry worth noting: the F1 gap-reader built + de-bugged this session is the **stepping-stone** to the
  cleaner architecture, not the destination.

## 3. Scope

**Scope-in:** `mission_scope_detect` re-anchoring (verbatim, not position); the substrate-2 mission-scope
ingest; the Arxana annotation-maintenance (fuzzy + editorial-log + watcher-re-detect); the unified-substrate
Salingaros C-field; the WM-EFE composition design + a worked example.

**Scope-out:** the full diachronic **footfall** layer (separate concern — turns→missions association is spotty);
per-mission **action-generation** (a follow-on once the field exists); the descriptive carpet/mandala renders
(done this session — `mission_carpet.py`, `mission_phylo_mandala.py` etc.); other uses of the position-indexed
detector.

## 4. MAP — build on what exists
- scope-detector + Salingaros + carpet: `futon6/scripts/mission_{scope_detect,fold,wholeness,carpet,phylogeny}.py`.
- proven ingest path: codex-1's `mission_scope_ingest.clj` (6 missions → futon1a/substrate-2).
- anchor convention: futon4 `README-essays.md` (verbatim `:passage`, `.edn` authoritative, `audit-passages`).
- substrate-2: futon1a hyperedge store / 7071 (already reflects code contents).
- the WM EFE: gap-reader / pre-witness / E-warranted-play / C-pudding-prover (futon3c/futon7, claude-1).

## 5. Exit conditions
- 194 mission-scope-trees live in substrate-2 as verbatim-anchored hyperedges; a mission edit re-resolves (or
  `:detached`-logs) without manual repair.
- the Salingaros C-field runs over the **unified** substrate-2 and a fat code file registers as high-C.
- a **ratified composition** of C-field + WM EFE (the layered macro/micro/live drive), demonstrated by a worked
  example: the WM, asked what to build, returns a *specific buildable unit* (a fat file / a scope-hole / a
  gene-graft), **not** "finish the mission".

## 6. Disciplines (hard lines)
- **Never** re-introduce position anchors as the source of truth (verbatim + scope-id only).
- **Never** restart the 7071 / futon3c JVM — Drawbridge reload only (substrate-2 is live).
- A scope that can't re-anchor is **`:detached` and logged**, never silently dropped.

## 7. D1 progress — incremental per-scope-type handoffs (Joe 2026-06-08: not bulk; log each)

Scopes are the key information source here — **no EFE yet** (D2 parked). Each scope-type is **one small handoff**
to codex-1, end-to-end (verbatim-anchor → substrate-2 ingest → verify), logged below. The first establishes the
reusable mechanism; later types apply it. **Structural** scope-types first; the **pattern-scope layer**
(flexiarg applications — the subtle "anatomy of patterns") is its own deferred deep-dive, last.

**Order:** eightfold-phase → loose-section → capability-scope → map-item → relates-to → source-material →
mission-scope-in/out → [pattern-scopes — deferred deep-dive].

| scope-type | status | notes |
|---|---|---|
| eightfold-phase | **INGESTED** by codex-1 2026-06-09 — reusable anchor+ingest mechanism established | 194 scope trees scanned; 346 eightfold-phase hyperedges written with `scope-id = <mission-stem>/<canonical-phase>` where unique and duplicate same-phase headings disambiguated by heading slug/hash; anchor shape is verbatim heading passage + concept-set fingerprint + `:anchor/state`; 15 detached anchors recorded for review, no silent drops |
| loose-section | **INGESTED** by codex-1 2026-06-09 — applied the same anchor+ingest mechanism to non-eightfold headings | 194 scope trees scanned; 1670 loose-section hyperedges written with `scope-id = <mission-stem>/<heading-slug>` where unique and duplicate heading slugs disambiguated by an original-scope hash; anchor shape unchanged; 7 detached anchors and 6 duplicate-heading disambiguations recorded |
| capability-scope | **INGESTED** by codex-1 2026-06-09 — swap-not-add reconcile implemented | 194 scope trees scanned; 461 capability-scope hyperedges written with heading-slug ids + verbatim anchors; 4 detached anchors and 48 heading-slug collision disambiguations recorded. Retroactive sweep retracted legacy position-anchored docs for migrated types: eightfold-phase 16 hyperedges + 16 scope entities, loose-section 49 + 49, capability-scope 68 + 68. Raw type queries now show 0 legacy `M-<stem>:scope-NNN` and 0 `:position` anchors for all three migrated types. Parent-link subtlety: capability scopes often nest under phases, but this pass keeps the scope hyperedge mission-anchored and records original parent only in props; stable parent wiring should be handled as a dedicated nesting pass rather than guessing cross-type ids. |
| capability-scope — **claude-3 review VERIFIED 2026-06-09** | /ego war-machine: loose-section 16→3, eightfold→3, capability→13 verbatim, **0 legacy scope-NNN across all 3 migrated types** — swap confirmed against the live store, not just self-report | (see row above) |
| map-item | **INGESTED** by codex-1 2026-06-09 — bullet/list-item anchor variant added | 194 scope trees scanned; 588 map-item hyperedges written; 63 legacy map-item hyperedges + 63 scope entities retracted; 15 detached anchors and 112 slug-collision disambiguations recorded. Raw type query shows 0 legacy `M-<stem>:scope-NNN` and 0 `:position` anchors for map-item. Bullet-anchor verdict: added `list-item-passage` search, but the detector's `map-item` scopes are mostly heading/subheading lines rather than literal bullets in this corpus (`14` list-item anchors, `559` contains-line anchors, `15` detached), so relates-to/source-material should expect the same mixed bullet/heading behaviour rather than pure bullets. |
| **deferred D1 sub-task: capability nesting pass** | parked | capability scopes nest under phases; codex-1 (correctly) kept them mission-anchored + recorded original parent in props rather than guess cross-type ids — stable parent-wiring is its own pass once all types are in |
| map-item — **claude-3 review VERIFIED 2026-06-09** | /ego war-machine: 12 map-item edges, **0 legacy scope-NNN**, verbatim ids. Bonus: confirmed the typed `scope/concept/<c>` nodes are the **concept-fingerprint cross-index** (evidence ×21, mission ×20, sorry ×17 — semantically right for war-machine), NOT duplication; canonical scopes 57-distinct ~1:1. Whole scope hypergraph structure sound. | — |
| relates-to | **INGESTED** by codex-1 2026-06-09 — cross-mission relation edges wired | 194 scope trees scanned; 235 relates-to hyperedges written as source-mission → existing target mission-node + anchor scope; 8 legacy relates-to hyperedges + 8 scope entities retracted; 106 distinct target mission identifiers linked, 12 dangling target references logged as `:target/state :detached`, 0 position anchors and 0 legacy `M-<stem>:scope-NNN` remain for the type. Target mission nodes were resolved by existing substrate entity lookup only (`M-...` / `mission|M-...`), never fabricated; war-machine /ego shows target mission-doc nodes such as M-aif-head, M-stack-inhabitation, M-self-representing-stack rather than duplicate ad hoc nodes. |
| relates-to — **claude-3 review VERIFIED 2026-06-09** | /ego war-machine: 8 relates-to scopes (verbatim), **0 legacy**, 34 `mission/doc` target nodes — edges connect to EXISTING mission nodes, no fabricated dupes; 12 dangling refs `:detached` not invented. First real inter-mission topology, clean. | — |
| source-material | **INGESTED** by codex-1 2026-06-09 — file/source reference edges wired | 194 scope trees scanned; 415 source-material hyperedges written as source-mission → existing source/file node + anchor scope; 4 legacy source-material hyperedges + 4 scope entities retracted; raw type query shows 0 legacy `M-<stem>:scope-NNN` and 0 `:position` anchors. 139 source references linked to 99 distinct existing file/source nodes; 276 dangling source refs logged as `:source/state :detached` / `:source-file-not-found`. No file nodes were fabricated: the resolver only reused existing entity lookup by source ref or repo-d/file candidate. War-machine /ego shows linked file/source targets such as `futon3/holes/war-room.md`, `futon5a/data/alignment.edn`, and `GET /api/alpha/missions`. |
| source-material — **claude-3 review VERIFIED 2026-06-09** | /ego war-machine: 17 source edges (verbatim), **0 legacy**, real `source/file` targets, no fabricated nodes. **Coverage finding:** 276/415 source refs (66%) `:detached`/`:source-file-not-found` — substrate-2's file-reflection covers only some repos, so most cited source isn't reflected yet. Correct (no fabrication); flagged for later coverage work. | — |
| mission-scope-in/out | **INGESTED** by codex-1 2026-06-09 — LAST structural type complete | 194 scope trees scanned; 632 `mission-scope-in` hyperedges and 508 `mission-scope-out` hyperedges written from explicit boundary bullets; 6 legacy in + 6 legacy out hyperedges and their scope entities retracted; 0 detached anchors; raw type queries show 0 legacy `M-<stem>:scope-NNN` and 0 `:position` anchors. Polarity facet shape: scope/entity and hyperedge props carry `:scope/polarity :scope/in` or `:scope/out`, with ids shaped `<mission-stem>/scope-in/<slug>` / `<mission-stem>/scope-out/<slug>`. War-machine /ego shows both committed and excluded boundaries with verbatim bullet anchors. This completes the structural half of D1; remaining work is deferred pattern-scopes + stable parent-wiring, not another structural type. |
| (deferred deep-dives) | queued | **pattern-scopes** (flexiarg applications — the subtle "anatomy of patterns") + **capability nesting pass** (stable parent-wiring) remain after the structural set |
