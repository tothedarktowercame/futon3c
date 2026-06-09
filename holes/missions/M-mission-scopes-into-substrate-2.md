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

### D1.1 — Live maintenance loop  *(the "without manual repair" exit condition)*
D1's structural ingest is **batch**. Exit condition #1 requires a mission edit to
**re-resolve (or `:detached`-log) without manual repair**. D1.1 builds the incremental
maintenance pipeline — the Arxana annotation-maintenance discipline where *identity
survives edits*:
- **(a) Single-mission re-detect:** `mission_scope_detect.py <mission.md>` regenerates
  that one mission's scope-tree JSON (already supports per-file args →
  `futon6/data/mission-scope-trees/<stem>.json`).
- **(b) Incremental diff-and-patch ingest** (`mission-scope-ingest --mission <stem>`):
  instead of the blind full-swap, **diff stored-vs-newly-detected** scopes for that
  mission and patch:
  - **added** → upsert (verbatim-anchored, as today);
  - **removed** (no longer detected) → retract the scope + its hyperedges (incl. nesting);
  - **moved / reworded** → the subtle case. A reworded heading changes the slug → the
    `scope-id`, which naively reads as remove+add and **destroys the scope's identity
    and edges** (nesting, concept links, parent-wiring). Use the stored
    **`:anchor/fingerprint`** (concept-set) to recognise *same scope, reworded* and
    **rename-in-place** — preserve `:hx/id` / `:scope/original-id` and all edges, update
    only the verbatim passage + slug;
  - **unresolvable** (verbatim search fails *and* no fingerprint match) → `:detached`
    editorial-log, never silent loss.
- **(c) Fuzzy re-resolve:** when verbatim search fails, fall back to fingerprint
  similarity to relocate the anchor (the reworded-heading path).
- **Idempotent:** re-running on an unchanged mission is a no-op (no churn).
- **Verify:** edit a throwaway test mission (add a section, remove one, reword a
  heading), run `--mission <stem>`, confirm added appears, removed retracted, reworded
  keeps the *same* `:hx/id` + edges with a new passage, nothing else churns.

### D1.2 — Watcher auto-trigger  *(makes the maintenance loop automatic — closes exit-condition #1)*
D1.1 is the mechanism but it's still **manual** (`--mission <stem>`). D1.2 makes a mission
edit re-resolve **without manual repair** by wiring the multi-watcher to fire D1.1 on save.
The hard constraint is the watcher hot-loop: it must stay **trivial** — heavy work in the
cycle cascades to the one JVM (the blackboard-backpressure failure mode; a hot-path stall
already bounced the JVM this session). Architecture:
- **Hot-path = enqueue only.** When a per-cycle changed path matches the mission-doc
  pattern (`futon*/holes/**/M-*.md`), push its stem onto a `!pending-missions` set. That's
  *all* the cycle does — no detect, no ingest, no Python shell-out, no HTTP.
- **Async debounced drainer** (model it on the sibling `futon3c.process-watchdog`
  scheduled executor — read it first, I-4): on its own schedule, drain stems that have been
  **quiet for ~N seconds** (debounce coalesces burst saves) and run D1.1 for each. **Strongly
  prefer spawning an isolated child process** (`clojure -M -m
  futon3c.scripts.mission-scope-ingest --mission <stem>`) so the heavy detect+ingest runs
  *off the serving JVM* entirely. If done in-process instead, it MUST be on the drainer's
  own thread, never the watcher cycle thread, and bounded.
- **Idempotent + safe:** D1.1 is a no-op on an unchanged mission, so redundant fires are
  harmless. Debounce prevents per-keystroke thrash.
- **Observable:** each auto-run logs (mission, diff-counts, `:detached`) like a heartbeat;
  failures surface (notify / `:last-error`), never silently swallowed.
- **Activation:** reload the edited watcher ns via Drawbridge `load-file` (the sanctioned
  path — README-multi-watcher.md), **never** a JVM restart. Verify with a throwaway mission:
  save an edit, confirm the watcher auto-runs D1.1 within a cycle+debounce. If anything looks
  off during the live reload, STOP and bell — do not restart.

### D1.3 — Pattern-scopes  *(applied: a mission→pattern edge, four-form grammar)*
A pattern-scope is categorically unlike the 7 structural types: not a region of text but a
**methodological move** — "here this mission applies pattern P." Structurally it's the
pattern-analog of `relates-to` / `source-material`: a **cross-entity** scope whose target is
an existing **pattern node** (the flexiarg registry, already reflected in substrate-2),
anchored verbatim at the citation, **linked never fabricated**, `:detached` if the pattern
isn't a known node. **APPLIED only** — near-miss / uncited / inferred patterns are *virtual*
(Hawkins-style priming; hallucination-risk if ungrounded) and are explicitly OUT of D1 (they
belong to the cascade / futon3a thread and `M-autonomous-pattern-lifecycle`'s vPSR/vPUR).

**The four forms (grammar), weakest→strongest attestation:**
- `(pattern P)` — a distinctive flexiarg cited in the mission prose. Weak attestation
  ("referenced," not necessarily "used"); broad coverage. **← the v1 codex handoff.**
- `(psr (pattern P))` — a Pattern-Selection-Record block (Pattern chosen / Candidates /
  Rationale). `:record :psr` + rationale/candidates facets.
- `(pur (pattern P))` — a Pattern-Use-Record block (Pattern / Actions / Outcome / Prediction
  error). `:record :pur` + outcome/prediction-error facets. The prediction-error is the
  grounding signal the virtual patterns lack.
- `(pxr (psr #i) (pur #i))` — **derived**: co-index a PSR with its PUR on the same pattern →
  the complete select→use→outcome unit; the labeled example the downstream priming layer learns from.

**Anchor on the field-template markers, not headings.** Real PUR/PSR blocks use the canonical
`Pattern:` / `Actions:` / `Outcome:` / `Prediction error:` (PUR) and `Pattern chosen:` /
`Candidates:` / `Rationale:` (PSR) lines, but their *headings* are non-uniform (`## PUR`,
`# PUR-A1:`, `### PUR — <pattern>`, `## vPUR`, inline). Anchor on the field-block, robust to the
heading zoo. **Use-vs-mention is handled by the layers, not a filter:** `(pattern)` = referenced
(weak); `(pur)` = used-with-outcome (strong). A mission merely *discussing* PSRs (e.g.
M-capability-star-map's "psrs/purs" field list) yields at most weak `(pattern)` citations, never a `(pur)`.

**The missing discipline (root cause).** PSR/PUR aren't uniform because there is **no uniform
place for them to land** — unspecified in `mission-lifecycle.md`, never formalized in
substrate-2. So a parallel deliverable: specify the `psr`/`pur` landing convention in
`mission-lifecycle.md` (informal) → the scope grammar here (formal). Once landed, D1.1/D1.2 keep
them current as missions adopt it. Historical corpus today = mostly bare `(pattern)`, ~13–19 with
real `(psr)`/`(pur)`.

**Sequencing:**
- **(a) v1 — `(pattern P)` ingest → codex (now).** Add a `pattern` binder to
  `mission_scope_detect.py` (distinctive flexiarg citations; the citation passage as the verbatim
  anchor); ingest it in `mission_scope_ingest.clj` reusing the `relates-to`/`source-material`
  cross-entity machinery (target = existing pattern node, fetch-not-fabricate, `:detached` on
  unknown). Flows through detect→ingest→D1.1/D1.2 like every other binder.
- **(b) `(psr)`/`(pur)` → by hand (Joe + claude-3).** Small N (~13–19): find the attested blocks,
  wire the richer facets, derive `(pxr)` where both exist.
- **(c) Hawkins/priming on `(pxr)` → downstream**, routed to `M-autonomous-pattern-lifecycle`
  (PUR-prediction-error → precision prior; vPSR/vPUR replay). Not D1.

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
| source-material — **claude-3 review VERIFIED 2026-06-09** | /ego war-machine: 17 source edges (verbatim), **0 legacy**, real `source/file` targets, no fabricated nodes. **Coverage finding:** 276/415 source refs (66%) `:detached`/`:source-file-not-found` — substrate-2's file-reflection covers only some repos, so most cited source isn't reflected yet. Correct (no fabrication); flagged for later coverage work. → **handed to codex-3 (job …189) as a multi_watcher investigation** (Joe 2026-06-09: substrate-1↔2 reflection likely broken; no standalone watcher process running). Parallel lane — keeps codex-1's D1 clear. **codex-3 verdict (job …189, read-only):** watcher is ALIVE (embedded `futon3c.watcher.multi`, 14 roots, healthy heartbeats) — NOT dead. The 66% is mostly a **resolver blind-spot**: `resolve-source-file` only checks entity-docs, but most files live as hyperedge ENDPOINTS (`hyperedges?end=<file>` finds them) → false-negative detach (140/276 detached files exist on disk). Plus (2) this boot is `cold-scan? false` (no backfill of unchanged files) and (3) label mismatch (watcher `futon4-elisp-d`/`futon5-d2`/`futon6-py-d` vs resolver's canonical `futon4-d`/`futon5-d`/`futon6-d`). True residual = relative/truncated/sha/api/out-of-root refs (need policy, not fabricated nodes). Remediation = (1) resolver endpoint-lookup + re-ingest backfill, (2) label hygiene + explicit cold-scan — both need Joe's go-ahead. **CORRECTION (codex-3 re-brief, job …193): the futon3c JVM was RESTARTED ~10:02 UTC during the investigation** (prime suspect: a heavy unbounded `hyperedges?type=<large-type>` GET that full-materializes before limit — NOT a Drawbridge reload; see [[feedback_heavy_hyperedge_type_query]]). So codex-3's *runtime* findings (event-n/cycle-n/"no backfill since boot") are restart artifacts — use only current-boot. The **durable, source-level diagnosis stands** and needs no watcher-death: (a) `resolve-source-file` only entity-looks-up, never `hyperedges?end=` → endpoint-only file nodes invisible; (b) bootstrap.clj roots `futon4-elisp-d`/`futon5-d2`/`futon6-py-d` ≠ resolver-canonical `futon4-d`/`futon5-d`/`futon6-d`; (c) `cold-scan? false` is set explicitly in bootstrap (steady-state, every boot) → watcher only projects post-boot *changes*, never backfills unchanged files. JVM currently healthy (watcher cycling, watchdog clean); 3100 serves HTML again but authed `/ego` routes lost their session on restart. **QUANTIFIED (claude-3, bounded `end=` probes 2026-06-09 — NOT the heavy type-scan): substrate-1↔2 are essentially in sync; the watcher is fine.** The 237 distinct detached refs decompose: **~98 (41%) inherently un-resolvable** (3 commit-SHAs, 11 brace/glob artifacts, 15 bare filenames, 38 relative/out-of-root, 3 `.claude` paths, 28 other — correctly `:detached`, not files); **139 clean repo-paths (118 on disk), of which 117 ARE present in substrate-2 as canonical-label endpoints** (resolver false-negatives — `resolve-source-file` only entity-looks-up); **exactly 1 truly absent** (cold-scan gap, `futon3b/src/futon3/gate/canon.clj`); 0 label-mismatches in this set. So the real fix is **only remediation (1) — the D1 resolver endpoint-lookup** (recovers the 117); label-hygiene (2) and cold-scan have ~zero impact here and can be deprioritized. **DONE + VERIFIED 2026-06-09 (claude-3, direct — Joe opted out of handoff):** added `endpoint-exists?` (bounded `?end=&limit=1`) + endpoint fallback to `resolve-source-file` (no fabrication); clj-kondo 0/0, parens OK; re-ran `--binder source-material` (idempotent, 0 legacy retract); store now **303 `:linked` / 112 `:detached`** (was 139/276) — the 112 are the inherently-un-resolvable citations. Documented in `futon3c/README-multi-watcher.md` (false-alarm + fix + heavy-`type=` pitfall). Code+doc uncommitted (commit on request). | — |
| mission-scope-in/out | **INGESTED** by codex-1 2026-06-09 — LAST structural type complete | 194 scope trees scanned; 632 `mission-scope-in` hyperedges and 508 `mission-scope-out` hyperedges written from explicit boundary bullets; 6 legacy in + 6 legacy out hyperedges and their scope entities retracted; 0 detached anchors; raw type queries show 0 legacy `M-<stem>:scope-NNN` and 0 `:position` anchors. Polarity facet shape: scope/entity and hyperedge props carry `:scope/polarity :scope/in` or `:scope/out`, with ids shaped `<mission-stem>/scope-in/<slug>` / `<mission-stem>/scope-out/<slug>`. War-machine /ego shows both committed and excluded boundaries with verbatim bullet anchors. This completes the structural half of D1; remaining work is deferred pattern-scopes + stable parent-wiring, not another structural type. |
| mission-scope-in/out — **claude-3 review VERIFIED 2026-06-09 (via 7071 raw store)** | counts match §7 exactly (632 in / 508 out); **0 `:position`**, perfect polarity (632 `:scope/in`, 508 `:scope/out`); the 498/620 `M-..:scope-NNN` strings are all under `:scope/original-id` provenance — **0 in any identity field** (`:hx/id`/`:scope-id`/`:from`/`:to`). **STRUCTURAL HALF OF D1 COMPLETE.** Note: 3100/ego auth-walled this session (`Not authenticated`); gate moved to `GET :7071/api/alpha/hyperedges?type=mission-scope/<binder>`. | — |
| capability nesting pass | **DONE + VERIFIED 2026-06-09 (codex-2, 7071 bounded raw-store reads)** | Added `--wire-parents`: builds `{original :scope/id → verbatim :scope/id}` across all structural `mission-scope/<binder>` types, reads detector JSON for each child's original parent, resolves by correspondence only, then fetch-merge-upserts child scope hyperedge/entity props with `:scope/original-parent`, `:scope/parent <verbatim-parent-scope-id>`, `:scope/parent-state :linked`, `:scope/parent-resolve-by :original-id-correspondence`, plus idempotent `mission-scope/nesting` hyperedges `{entity, parent, child}`. Counts: capability-scope **461/461 linked**, 0 detached, 0 missing parent targets (376 → eightfold-phase, 85 → loose-section); all child types with recorded parents linked, 0 detached: eightfold 346, loose 1404, map-item 588, relates-to 235, source-material 415, scope-in 632, scope-out 508. `mission-scope/nesting` count = **4803**. Temporary bare-type repair verified: canonical type counts restored (eightfold 346, loose 1670, capability 461, map-item 588, relates-to 235, source 415, in 632, out 508), bare binder types 0. **claude-3 INDEPENDENT review (author≠reviewer) CONFIRMS via own 7071 bounded reads:** canonical counts intact, bare types 0, nesting=4803, capability 461/461 `:scope/parent` set to verbatim ids (sample `a-sorry-enterprise/identify`, `action-cost-modelling/derive`), 0 nil / 0 legacy. **→ STRUCTURAL HALF OF D1 COMPLETE.** Only the pattern-scope deep-dive remains. (Git: commit 86f2bc1 has `--wire-parents` but NOT claude-3's `endpoint-exists?` resolver fix — committed `resolve-source-file` is stale vs the store until that fix is committed; working tree holds both.) |
| D1.1 live maintenance loop | **DONE + VERIFIED 2026-06-09 (codex-2, throwaway mission + 7071 bounded raw-store reads)** | Added `--mission <stem>` incremental path: re-runs `mission_scope_detect.py` for one mission, diffs stored-vs-detected scopes across all structural binders, patches added scopes, retracts removed scope docs + all bounded `?end=<scope-id>&limit=5000` hyperedges, and preserves identity for reworded scopes by matching original-id/exact-id plus conservative heading-token + concept-set similarity. Reword updates the existing scope hyperedge/entity in place with new `:anchor/passage`, `:anchor/heading`, `:scope/reworded-scope-id`, `:scope/maintenance-state :re-resolved`; it keeps the original `:hx/id` and endpoint identity so parent/concept/source/relates/nesting edges survive. Verification used temporary `M-d11-maintenance-test`: seeded 8 scopes; edit removed Alpha, reworded Beta, added Gamma; incremental report = `stored 8 / detected 8 / unchanged 5 / removed 2 / added 2 / reworded 1 / detached 0`; 7071 verified Alpha count 0, Gamma present, and Beta capability kept `hx|mission-scope|d11-maintenance-test/beta-section` while passage changed to `### Beta Renamed Section`. Unchanged rerun = `unchanged 8`, added/removed/reworded all 0. Throwaway mission file, generated JSON, and store docs cleaned (`remaining 0`). **claude-3 INDEPENDENT review (author≠reviewer):** commit `ff5335a` confirmed to include the `endpoint-exists?` resolver fix (committed `resolve-source-file` now current vs store — git gap closed) + the `--mission` diff-patch; zero `d11-maintenance-test` residue; real canonical counts intact (461/346/1670/588 — no collateral damage); clj-kondo 0/0. Edge-survival on reword verified *by design* (stable `:hx/id`/scope-id + new slug as a facet ⇒ referencing edges never dangle), not just by codex-2's run (test correctly cleaned up). **Exit-condition status: the re-resolve MECHANISM is done; it is still MANUAL (`--mission`). "without manual repair" is fully met only once D1.2 (watcher auto-trigger on save) lands.** |
| D1.2 watcher auto-trigger | **CODED, LIVE VERIFY STOPPED 2026-06-09 (codex-2)** | Added the enqueue-only hot-path design in `futon3c.watcher.multi`: mission-doc changes enqueue stems into `!pending-missions`; a sibling scheduled executor drains quiet stems after a 10s debounce and runs isolated child processes (`clojure -M -m futon3c.scripts.mission-scope-ingest --mission <stem>`), recording recent runs / `:last-error` and surfacing failures through `notify-send`. Static gates passed (`clj-kondo` 0/0; `check-parens` clean) and sanctioned Drawbridge `load-file` succeeded. **Live verification was stopped by invariant:** watcher status reported `:running? true` but stale cycle timestamps (`last-cycle-finished-at 2026-06-09T11:41:47Z`, `last-cycle-started-at 2026-06-09T11:41:52Z`) and did not observe the throwaway `M-d12-watcher-test` file within cycle+debounce; per D1.2 instructions, codex-2 stopped, removed the throwaway file, stopped the newly-started drainer, and did **not** claim exit-condition #1 closed. No JVM restart, no forced tick, no unbounded 7071 queries. |
| pattern — D1.3(a) v1 applied-only | **INGESTED + VERIFIED 2026-06-09 (codex-3, bounded 7071 raw-store reads)** | Added distinctive flexiarg literal detection to `mission_scope_detect.py` (library `*.flexiarg` basenames, `>=2` hyphens and `>=12` chars) and a `pattern` cross-entity binder in `mission_scope_ingest.clj`. Scope id shape is `<mission-stem>/pattern/<flexiarg-slug>`; duplicate citations remain idempotent via the existing swap-not-add path. Ingest expands each detected pattern end to one mission→pattern edge and resolves only existing targets: direct entity lookup, bounded cached `pattern/library` entities (`limit=5000`), then reflected `.flexiarg` endpoint evidence via bounded `hyperedges?end=&limit=1`; no pattern nodes are fabricated. Verification: temp detector pass produced 197 scope-tree files; pattern-only ingest wrote **564** `mission-scope/pattern` hyperedges, **282 linked / 282 detached**, **232 distinct cited patterns**, **94 distinct linked patterns**, **0 anchor detachments**, 0 legacy retracts. Anchor subtlety: detection groups all first-seen pattern citations in a section, then ingest expands to per-pattern scopes anchored to the citation-containing passage/section line; v1 intentionally records weak "referenced" attestation only, not psr/pur/pxr "used" semantics. Spot-check: `M-capability-star-map` links existing targets such as `candidate-pattern-action-space`, `evidence-over-assertion`, `scope-before-action`, and `guardrails-vs-tooling`; `logic-model-before-code` is detected but correctly remains `:detached` because the live store currently has neither a `pattern/library` node nor a reflected endpoint for `futon3-d/file/library/mission-coherence/logic-model-before-code.flexiarg`. |
| (deferred deep-dive) | queued | **pattern-scopes** (flexiarg applications — the subtle "anatomy of patterns") is the only remaining D1 piece after the nesting pass |
