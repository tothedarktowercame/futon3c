# Mission: M-populate-substrate-2

**Date:** 2026-06-25
**Status:** HEAD + IDENTIFY drafted; MAP grounded in live census (port 7071) + prior art; awaiting Joe ratification of the deliverable set and phase. Mission-close and phase transitions are Joe's call.
**Owner:** claude-2 (drafting), pending assignment.
**Relates to:** Campaign `futon3c/holes/campaigns/C-substrate-completion.md` (this mission populates the nodes/edges that campaign's metric contract reads).
**Living map:** `futon2/holes/substrate-2-explainer.html` — the kitchen inventory; doubles as this mission's progress display (boxes go green as deliverables land).

---

## HEAD

### Operator shape

Substrate-2 — the XTDB hypergraph on `:7071` (futon1a) — is named for *inference over a live model of the stack*. Today it is a **palimpsest**: 203 types are declared, ~35 are populated, and the populated layers are the sediment of several ingestion campaigns run at different times under inconsistent labels. Some layers are richly built and watcher-maintained (the commit spine; ~15k mission-scope hyperedges). Some are built but frozen and disconnected (a 71k-edge structural code graph at a single HEAD, never joined to the commit spine). And the layer the substrate is *named for* — the argument/proof relations that let belief propagate — is entirely empty.

The kitchen has a fridge and a sink and a lot of cupboards, but the cupboards aren't wired to each other, half were installed by different contractors who left, and there's no cooking happening. This mission builds the kitchen out so you can actually cook in it: reconcile what exists, join the disconnected layers, build the missing inferential layer, and keep an honest live inventory so it never silently rots again.

### The question

**What does it take to turn substrate-2 from a multi-campaign fact store with graph lookup into a coherent, time-travelable inference substrate — and in what order, given that several layers are already built but disconnected?**

The unknown specification (why this is a mission, not a task): how the inference layer composes over the populated relations, and how to reconcile the palimpsest without discarding hard-won prior work. The *gaps* are findable facts (named below). The *composition* is open.

### Anti-glibness discipline

- Do not call a layer "empty" without querying the right type family. (The first explainer did exactly this — it queried `mission` entities and missed ~15k `mission-scope/*` hyperedges. Every count in MAP below is a live query as of 2026-06-25.)
- Do not redo finished work. M-mission-scopes and the April structural-graph ingest *landed*; credit them and build on them.
- Do not claim a relation layer "supports inference" until edges exist AND a named consumer query reads them (the E-sorry-typing T-A4 gate).
- Do not promote borrowed-prior guesses (the mission-miner's `:mined-structural` moves) as if they were constructed proofs.
- "Lightweight"/"bounded slice" is banned as cover for incomplete work — name the mechanism.

### **IF:** block (for the sigil probe)

**IF:** substrate-2 should be a coherent, time-travelable inference substrate over the live stack
**HOWEVER:** it is a palimpsest — built layers sit disconnected under inconsistent labels, the code graph is frozen at HEAD, and the inferential relation layer is empty
**THEN:** reconcile the existing layers, join the commit spine to the code graph, version it, populate the argument/proof relations from named sources, and keep a live inventory
**BECAUSE:** the layers the substrate is named for (propagation, time-travel, posteriors) cannot exist until the connective tissue is built and the existing sediment is made coherent

> Sigil not yet computed. Per futon6 CLAUDE.md, the 8-bit exotype + xenotype-32 is a *computed* artifact — run `futon6/.venv/bin/python futon5/scripts/head_exotype_probe.py futon3c/holes/missions/M-populate-substrate-2.md --emit-health` (recipe in `futon5/README-sigils.md`). Offered, not fabricated.

---

## 1. IDENTIFY

The tension is **coherence-under-sediment**, not greenfield emptiness. substrate-2 was populated incrementally by missions that each solved their own slice and stopped:

- **M-live-geometric-stack** (April, futon3) ingested the structural code graph (contains/coverage/tests) under `-d`/`phase-1` labels — a single HEAD snapshot.
- The **commit/history layer is also frozen**: the newest commit vertex in the store is dated **2026-05-21**, while futon3c's actual git HEAD is 2026-06-24. The whole code/history side stopped getting updates ~5 weeks ago.
- **M-mission-scopes-into-substrate-2** (June) landed the mission-scope layer — and is the *only* layer that has seen June. It is the success story precisely because it is the one thing that stayed live.
- **E-substrate-2-sorry-typing** (May) shipped a partial sorry representation; **E-substrate-2-timetravel** (June) chartered the versioning fix but explicitly deferred implementation.

**The liveness failure is the core wound.** substrate-2 is supposed to be an up-to-date model of the stack supporting fast inference; instead its code/history half is a stale batch dump. The watcher machinery *exists and is wired* — `multi/run-cycle!` → `ingest-new-commits-for-root!` → `commit-ingest/ingest-new-commits!` on a 5s poll, with `:edits` resolution — but it is evidently **not ingesting** into the live store (no June commits under any label). And even if re-activated, its `:edits` resolution reads `code/v05/var` (`multi.clj:842`, `commit_ingest.clj:450`), which is **empty** — so the commit→code bridge is double-blocked. This is good news in one sense: liveness is a *re-activation + repair* problem, not a green-field build.

The result: each layer is real, but they don't compose *and most aren't live*. The commit spine knows *when*; the code graph knows *what's-in-what*; nothing links them; and both stopped in May. There is no `edits` edge (commit→def), no versioning, and no argument/proof relations at all. So the substrate can look things up — against a May snapshot — but cannot reason and cannot see today's code.

**This is the named implementation mission** that E-substrate-2-timetravel asked Joe to charter (its exit criterion: "the future implementation mission is named").

---

## 2. MAP — live census + prior-art credit (2026-06-25)

Status legend: ✅ built & live · ⚠️ built but frozen/disconnected/partial · ⛔ declared but empty · 📦 lives in a sibling store.

### Code & history
| Layer | Live count | Status | Prior art |
|---|---|---|---|
| Commit spine (commit / precedes / authored / author) | 3,255 / 3,242 / 3,255 / 207 | ⚠️ **FROZEN at 2026-05-21** (newest commit in store); SHA-pinned but ~5 weeks stale; watcher wired but not ingesting | futon3c watcher `commit_ingest.clj`; `multi/run-cycle!` |
| Structural code graph (`contains` / `coverage`) | 63,046 / 8,906 | ⚠️ populated but frozen at April `-d`/`phase-1` labels | M-live-geometric-stack; `E-substrate-2-elisp-projection` (RESOLVED) |
| `code/v05/var` / `edits` / `calls` | **115,148 / 184,368 / 74,133** | ⚠️ heavily populated but frozen at April; NOT 0 (see 2026-06-25 correction — the "0" was a query-timeout artifact) | `commit_ingest.clj`; April batch |
| Tests | 28,993 | ⚠️ bare names; linked to defs via `coverage` only | M-live-geometric-stack |
| file→mission / commit→mission / block-trailer | 2,118 / 4 / 5 | ⚠️ bare paths at HEAD; commit→mission near-blind (4 of 3,255) | futon3c watcher; `E-substrate-2-directed-edge-id` (RESOLVED — explains the `dir:` 3rd endpoint) |

### Missions, scopes, work
| Layer | Live count | Status | Prior art |
|---|---|---|---|
| `mission-scope/*` (7 structural types + nesting + pattern) | ~15,000 (loose 4,923 · nesting 4,835 · source 1,423 · cap 530 · eightfold 405 · map 693 · in 651 · out 527 · pattern 618) | ✅ watcher-maintained, verbatim-anchored, idempotent | **M-mission-scopes-into-substrate-2** (the success story) |
| `mission-scope/psr` · `pur` | 9 · 5 | ⚠️ thin | same |
| Sorries (`:sorry` entities) | 293 | ⚠️ from stack-geometry mining | — |
| `code/v05/sorry` hyperedges · `related-mission` edges | 23 · 28 | ⚠️ the sorry-typing v0, partially shipped | **E-substrate-2-sorry-typing** (deferred `:addresses/:raises/:bites/:resolves` behind T-A4) |
| `:depends-on-sorry` / `:closes` | 0 / 0 | ⛔ the sorry-island has no outbound edges | E-sorry-typing (gated) |
| Mission / claim / proposition / question entities | 8 / 0 / 2 / 5 | ⚠️/⛔ near-empty assertion layer | — |

### Argument & inference
| Layer | Live count | Status | Prior art |
|---|---|---|---|
| Argument/proof relations (`constructs`, `closes`, `depends-on-sorry`, `uses-definition`, `would-refute`, `supported-by`, `attacks-claim`, `implemented-by`) | **all 0** | ⛔ the inferential connective tissue, entirely empty | — |
| Patterns | 0 here | 📦 in futon3a meme.arrow / futon3b library | — |
| Learning-update / evidence | 47 / 15 | ✅ thin but live | — |
| Inference engine (propagation, posteriors) | n/a | ⛔ none — traversal & filter only | — |
| Metric contract (`d`, node-identity, curvature/continuity cuts) | design delivered | ✅ contract exists; reads the above | **M-substrate-metric** (C-substrate-completion keystone) |
| Move-mining (have→want `:close-hole` arrows for abstaining missions) | overlay only | 🚧 `data/diffsub-moves-mined.edn`, NOT promoted | futon6 `mission_mine_moves.py` |

---

## 3. Candidate deliverables (the kitchen build-out)

Ordered so each rests on the previous. Implementation-neutral where the spec is open; specific where the gap is a fact. **D0 is the first car** — a substrate that doesn't see today's code can't be reconciled or reasoned over.

**D0 — Re-activate liveness (turn the lights back on).**
The code/history half froze on 2026-05-21. Diagnose why the wired watcher (`multi/run-cycle!` → `commit-ingest/ingest-new-commits!`, 5s poll) isn't ingesting into the live store — not started? roots empty? erroring silently? Re-activate via the sanctioned path (Drawbridge `load-file` / watcher start, **never** a JVM restart), confirm new commits flow within a cycle, and add a liveness heartbeat/staleness alarm so a stalled watcher is loud, not silent. This is the single most urgent deliverable: it is the difference between a live model and a stale dump, and it is what the whole substrate is *for*.

**D1 — Reconcile the palimpsest (clean the kitchen, count the utensils).**
Decide which ingests are canonical; unify the label regimes (`-d`, `-d2`, `phase-1`, `futon2-phase1`, …); determine whether the April structural graph (contains/coverage) is watcher-maintained or stale, and re-home it under a maintained label if needed. Reconcile the three sorry representations (293 entities vs 23 hyperedges vs 14 registry entries in `futon2/data/sorrys.edn`). Output: a single canonical-label map and a staleness report. *Without this, D2–D6 build on sand.*

**D2 — The commit↔code bridge (`edits` edges).**
Populate `code/v05/edits` (commit → def) joining the temporal commit spine to the structural graph. Reuse the elisp/clojure projectors from E-substrate-2-elisp-projection. **Blocker (found 2026-06-25):** the live `:edits` resolver reads `code/v05/var` to map changed files → defs (`multi.clj:842`, `commit_ingest.clj:450`), but `var` is empty — so the var-vertex layer must be (re)populated and label-matched first, or `:edits` resolution returns nothing. Precondition for any temporal code query.

**D3 — Versioned / time-travelable code (E-timetravel's chartered fix).**
Write contains/calls/coverage/edits with XTDB **valid-time = commit timestamp**; **file-level projection first** (the cheaper v1 E-timetravel recommends). **Historical re-ingest IS in scope (Joe, 2026-06-25)** — not forward-only; recover April→now so `db-as-of` works across the existing history, not just going forward. Un-gates `db-as-of` code queries, κ time-travel, T(v,c), drift.

**D4 — The argument/proof relation layer (the connective tissue).**
Populate `constructs` / `closes` / `depends-on-sorry` / `uses-definition` / `would-refute` / `supported-by` / `attacks-claim` from *named sources*, each passing the E-sorry-typing **T-A4 gate** (canonical source field + normalized target + named consumer). Three feeders: (a) the sorry-typing deferred edge families; (b) a **promotion path** from the mission-miner's overlay arrows into the store (the 🚧 gap — borrowed-prior moves stay tagged `:mined-structural`, never laundered as proofs); (c) BHK arrows from claude-4's `:constructed`/`:open` arrow store. This is the layer that turns lookup into propagation.

**D5 — Claims/propositions + discharge the sorry-island.**
Populate the assertion layer (claims/propositions) so D4's relations have endpoints; wire `depends-on-sorry`/`closes` from the 293 sorries to the code/claims they gate, so closing a sorry propagates.

**D6 — The inference layer (priors→posteriors).**
The propagation machinery over D4's relations — the line between database and substrate. Composes with M-substrate-metric's `d` and the C-substrate-completion consumers (M-aif2 curvature, M-differentiable-code continuity). *Spec genuinely open — this is the mission's hardest unknown; keep it implementation-neutral until D1–D4 land.*

**D7 — The living inventory (don't let the kitchen rot again).**
Make `substrate-2-explainer.html` auto-generated: counts queried live from `:7071`, not hand-typed. It becomes the mission's progress display and a permanent guard against the "queried the wrong type, declared it empty" failure that this very investigation hit. Pairs with D0's staleness alarm.

**D8 — Graph + vector co-query (catch up to the neo4j + pgvector bar).**
The comparison point (Rob's live Lean corpus in neo4j + pgvector) is graph **and** vector retrieval over one model. We have both pieces but in *separate* stores: the typed hypergraph here (futon1a/substrate-2) and embeddings in the sibling meme store (futon3a, BGE per the superpod-embeddings note; `mission_structure_embed.py`). They are not co-located or co-queried. Deliver a retrieval path that joins them — substrate-2 node identity ↔ futon3a embedding — so "find structurally + semantically near X" is one query. This is the "fast and efficient inference" half of the substrate's purpose, and the most direct way to close the gap with the neo4j+pgvector bar. *Spec open; depends on D0–D1 (stable, live node identity) before embeddings can be reliably keyed to it.*

---

## 4. Exit conditions (draft — for Joe to ratify/revise)

0. **Liveness:** a commit pushed today appears in substrate-2 within one watcher cycle, and a staleness alarm fires if ingestion stalls. (D0) — *the gating exit condition; without it the rest is decoration.*
1. **Coherence:** one canonical-label map exists; `pgrep`-style census of substrate-2 returns counts attributable to a maintained ingest path, with staleness flagged. (D1)
2. **Join:** `code/v05/edits` is non-empty and a query 'what defs did commit C touch' returns correct results. (D2)
3. **Time-travel:** `db-as-of <past-commit>` returns the code structure *as of that commit*, demonstrated on one worked example. (D3)
4. **Propagation:** at least one argument/proof relation type is populated from a named source and read by a named consumer query; closing one sorry demonstrably discharges a dependent. (D4/D5)
5. **Inventory:** the explainer renders live counts with zero hand-typed numbers. (D7)

D6 (full inference layer) is explicitly *not* an exit condition for v1 — it is the mission's open horizon, ratified separately once D1–D5 hold.

---

## 5. Notes

- **Discipline carried from prior art:** verbatim-anchor identity (never position); swap-not-add idempotent ingest; watcher hot-path enqueue-only + debounced drainer; Drawbridge `load-file` reload, never JVM restart; no writes to `:7071` without operator go-ahead (the boundary E-sorry-typing held).
- **Relationship to C-substrate-completion:** M-substrate-metric built the *contract* (what `d` is, node identity). This mission builds the *population* the contract reads. They are complementary; this mission likely belongs under that campaign — Joe's call.
- **Why now:** the substrate-2 explainer made the gaps legible and the mission-miner cross-check showed at least one gap (D4) already has an upstream generator waiting for a promotion path. The kitchen is half-built; the blueprint is finally visible.

---

## Checkpoint log

### 2026-06-25 — D0 (re-activate liveness): DONE & verified; latency follow-on opened

**Root cause of the freeze.** `bootstrap.clj:456` defaulted the env gate
`FUTON3C_MULTI_WATCHER_COMMIT_INGEST` to **false**, so the watcher ran with
`:commit-ingest? false`. The watcher itself was healthy the whole time (cycle
1049, `last-error nil`) — it just never ran the per-cycle commit catch-up. So
the code/history layer froze at the last batch (2026-05-21) while file-ingest
kept the mission-scope and file→mission layers live. Not a crash; a silent flag.

**Fix.** Flipped the bootstrap default `false → true` (env var retained as a
perf opt-out), with an explaining comment. Gates: check-parens clean, clj-kondo
0/0. Joe rebooted the JVM to pick it up (env read at boot; no live JVM restart).

**Verified live.** Post-reboot the per-cycle `ingest-new-commits!` caught up the
~5-week backlog automatically (unbounded `since-sha..HEAD`); within ~3 cycles
**14/14 repos' current HEAD is in substrate-2** (futon7a-d ingested for the first
time). A throwaway empty commit in futon3c landed in the store (then
`reset --soft`'d away; the commit vertex persists — correct bitemporal behavior).

**Tool delivered.** `futon3c/scripts/substrate2_liveness_probe.sh` — the manual
form of D0's staleness alarm. Reports the watcher's `:commit-ingest?` flag and,
per repo, whether HEAD's sha is in the store (O(1) `GET /api/alpha/hyperedge/
hx:code/v05/commit:<sha>`; falls back to the latest non-merge ancestor when HEAD
is a merge, since the ingest skips diff-less merge commits).

**New finding — the latency that justified the original default-off is real.**
With commit-ingest on, each cycle runs far past the 5s interval (a new commit
takes ~1–2 min to land, not one cycle). Cause: the `:edits` resolver calls
`query-repo-vars-by-file` *every cycle, per repo* (`multi.clj:842`), fetching all
`code/v05/var` + `code/v05/test` edges (~29k test edges) over HTTP. It is
**wasted work today** — it resolves `:edits` against `code/v05/var`, which is **0
rows**, so it pays full latency to produce zero edits.

**Exit-condition 0 status:** *partially met* — a commit pushed today appears, but
in ~1–2 min, not within one 5s cycle. Closing it fully needs the hot-path fix.

**Open follow-ons (in recommended order):**
1. ~~**D0.1 hot-path optimization**~~ — **DONE 2026-06-25 (see below).**
2. **D2 var-layer + `:edits` bridge** — once liveness is cheap, build the var
   vertices so the (now-justified) `:edits` resolution actually produces
   commit→def edges. The expensive query only earns its cost after D2.
3. Add an automated commit-freshness staleness alarm (D7), seeded by the probe.

### 2026-06-25 (later) — D0.1 (hot-path optimization): DONE & verified

**Change.** `multi.clj` `ingest-new-commits-for-root!` — wrapped the
`query-repo-vars-by-file` fetch in a `delay`, so the ~29k-edge var+test query
only fires on cycles that actually have new commits to resolve (it had run every
cycle, per repo, regardless). `ingest-commits-batch!` only calls `file->vars`
inside `(when (seq commits) ...)`, so the delay is never forced on the common
no-new-commits cycle. Reload-safe by design (the scheduler indirects through
`#'safe-cycle!`). Gates: check-parens clean, clj-kondo 0 errors (1 pre-existing
unrelated warning). Reloaded live via Drawbridge `load-file` (no JVM restart).

**Measured result:**
- New commit time-to-land: **>48s → 2s.**
- Cycle cadence: **~30–40s/cycle (dwelling in :query-repo-vars-by-file) → ~4–5s,
  phase :idle** between cycles.
- 14/14 repos still IN-STORE.

**Exit-condition 0 (commit-today within one cycle): now MET.**

Working-tree changes (uncommitted, persist on disk across reboot): the `delay`
fix in `multi.clj` and the bootstrap default-flip in `bootstrap.clj`. The live
JVM already runs the fix via the reload; a reboot loads it from disk.

### 2026-06-25 (D2 investigation) — CORRECTION: "var/edits empty" was a measurement artifact

**I was wrong across several turns.** I reported `code/v05/var = 0` and
`edits = 0`, built a "double-block" claim on it, and scoped D2 as "build the var
layer." All false. The code layer is **heavily populated** (true counts via
count-pushdown XTDB query):

| type | true count | type | true count |
|---|---|---|---|
| var | **115,148** | edits | **184,368** |
| calls | **74,133** | contains | **63,046** |
| namespace | 6,109 | coverage | 8,906 |
| test | 28,993 | commit | 5,419 |

**Root cause of the false zeros:** `hyperedges-by-type` (futon1a routes.clj)
pulled `(pull e [*])` for EVERY matching entity and sorted the whole set before
applying the limit — so any large type **timed out and returned empty** at the
high limits I counted with. The data was there all along (individual var docs
were fetchable by id).

**Operational consequence (the real D2 blocker):** the same timeout makes
commit-ingest's `query-repo-vars-by-file` (which queries `?type=var&repo=<label>`
with no limit) time out and silently return `{}` (`catch Exception _ {}`,
multi.clj:843) → `file->vars` always empty → **new commits get no `edits` edges**.
The 184k edits are frozen at April; nothing post-April links commits to defs.
So the commit↔code bridge isn't *missing* — it's *stale*, starved by a broken read.

**Reader fix — DONE & verified (futon1a routes.clj `hyperedges-by-type`):**
fetch entity ids first (cheap index lookup), sort ids, pull ONLY the returned
docs; `:count` is the true total when unfiltered. Reloaded via Drawbridge
`load-file` (no restart). Gates: check-parens clean, clj-kondo 0 errors.
Verified: `?type=var&limit=1000` → count 115,149 in 4.5s (was: timeout→0);
`?type=edits&limit=5` → count 184,368 (was: timeout). **Inventory is fixed.**

**Still open — the operational edit-resolution.** The reader fix did NOT fix
`query-repo-vars-by-file`: its repo-filtered, no-limit query still must pull all
115k var docs to find one repo's ~6k (repo is not an indexed attribute), so it
still returns `{}`. Recommended next step (D2.1, futon3c-side): resolve
file→vars by **parsing the changed files locally** (`collect-file` at repo-root)
instead of querying substrate-2 — small, fast, correct for the live case, and it
sidesteps the unindexed-repo problem entirely. (Alternative: index `:repo` as a
top-level hyperedge attribute + backfill — larger, write-pipeline change.)

**Consequent doc corrections:** the MAP "Code detail" row above is updated; the
earlier D0.1 note "wasted against var=0" is wrong (var is populated; the work
was wasted because the *read times out*, not because the data is absent); the
substrate-2 explainer's "Code detail: 0 var/edits" cell is likewise a timeout
artifact and is being corrected.

**Cleanup note:** one junk test vertex `futon3c-d/probe.test/xyz` (the +1 in the
115,149 count) remains from D2 probing; remove via a proper retract path.

### 2026-06-25 — D2.1 (live commit↔code edit-resolution): DONE & verified

**Change (multi.clj `ingest-new-commits-for-root!`):** resolve a changed file's
vars by parsing the file locally — `(collect-file (str root "/" rel-path))` →
unprefixed `:var/qname`s — instead of querying substrate-2. This replaces the
doubly-broken `query-repo-vars-by-file` (timed out → `{}`; and keyed by absolute
`:source-file` while `files-changed` yields relative paths, so lookups never
matched). `ingest-edits-for-commit!` per-repo-prefixes the qnames, matching the
var-vertex endpoint convention. Wrapped in try/catch so one unparseable file
can't abort a commit's other edits. Subsumes D0.1 (no per-cycle substrate-2
fetch left to defer). Gates: check-parens clean, clj-kondo 0 errors. Reloaded
via Drawbridge.

**Verified end-to-end:** committed a scratch `.clj` with 2 defns →
within ~3s the watcher posted exactly **2 `code/v05/edits` edges**:
`[<sha>, futon3c-d/scratch-d21-probe/probe-one]` and `probe-two` (with the
`dir:` synthetic endpoint). Unit-checked the resolver too: it returns 39 vars
for `agency/logic.clj`. Throwaway commit + scratch file removed afterward.

**D2 status:** the commit↔code `edits` bridge is now **live** — new commits link
to the defs they change. Remaining for substrate-2 completeness:
- `query-repo-vars-by-file` is now **dead code** (no callers) — remove.
- The structural graph (var/contains/coverage bulk) is still **frozen at April**;
  live file-ingest only refreshes *changed* files. A cold-scan would refresh it.
- Historical accuracy (old commits resolve against *current* file content, not
  the content at that commit) remains the **D3** valid-time-versioning concern.
- Two junk vertices from probing (`probe.test/xyz`, `scratch-d21-probe/*`) +
  the test commit's edges persist as orphaned bitemporal records.

### 2026-06-25 — D3 scoping (feasibility) + decision

**Feasibility: green — it's a threading job, no architectural blocker.** XTDB v1
supports valid-time on writes: `[:xtdb.api/put doc valid-time]` (3rd element).
The stack currently emits 2-element puts everywhere (e.g. `pipeline.clj:123`),
and `run-write!` passes a write-fn's tx-ops straight to `submit-tx!` — so a
write-fn CAN emit valid-time'd puts. Threading needed across layers:
1. futon3c commit-ingest (has commit ts + changed files) → pass valid-time.
2. post-hyperedge HTTP path (futon3c → futon1a) → carry a valid-time param.
3. futon1a route → write-fn → pipeline → the put op's 3rd element.
4. **Wrinkle:** `verify-materialized!` checks the tx at *current* time; a
   past-valid-time doc may not verify the same way — needs handling.

**Decision (Joe, 2026-06-25):** **historical re-ingest is required** — D3 is not
forward-only. Recover April→now so `db-as-of` answers across existing history.
Also: commit-ingest currently writes only `edits`; versioning the *structural*
graph (contains/calls — what's HEAD-frozen) means commit-ingest must also emit
those at the commit's valid-time. Open sub-questions for the build: file-level
projection scope; how to drive the historical re-ingest (replay all commits per
repo, emitting structure at each commit's valid-time — large); idempotency on
re-run; the materialization-check interaction. D3 is mission-scale — likely its
own focused session.
