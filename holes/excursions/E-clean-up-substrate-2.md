# Excursion: clean up substrate-2 — honest accounting, the negative space, and the registry (E-clean-up-substrate-2)

**Date:** 2026-06-26 · **Status:** IDENTIFY + MAP grounded in live census (port 7071, 2026-06-26) — charter for handoff (owned end-to-end by one agent; Joe assigns).
**Authored by:** claude-2 (scoping + the census archaeology; not committing the cleanup writes).
**Parent / relates:**
[[M-populate-substrate-2]] (this excursion is its **negative-space map + hygiene blocker** — what is *not* populated, why, and what cleaning the registry actually costs) ·
[[E-substrate-2-timetravel]] (the D3 bitemporal layer this census reads through) ·
[[M-goals-and-holes]] · [[M-operational-vocabulary]] (own the semantic-relation families B/C/D below — populating those = doing those missions) ·
[[E-C-vector-live]] (a downstream consumer of the goal/hole relations once they exist).
**Repos:** futon1a (`:7071` substrate-2; `src/futon1a/model/type_registry.clj` — the derived registry; `src/futon1a/core/pipeline.clj:129/188` — where type-ops ride `run-write!`; `src/futon1a/api/routes.clj` — `/census`) · futon3c (the watcher/ingest writers; `.admintoken`; Drawbridge `:6768`) · futon2 (`holes/substrate-2-explainer.html` — the living inventory).

---

## HEAD

"Can we populate the remaining 35%?" turned out to be the wrong question, asked against wrong numbers. This excursion is the honest answer: **what is actually in substrate-2, what is genuinely empty, why, and what it costs to clean the catalog so "% populated" means something.**

Three findings reframe everything:

1. **The headline "~35 of 200 populated" was a timeout-sniff artifact** — the exact footgun substrate-2's tooling exists to kill (a high-`limit` type scan times out → returns empty → looks unpopulated). Ground truth via count-pushdown: **~130 distinct types are actually populated** in the store.
2. **The registry and the data are largely disjoint universes.** The heaviest populations are **off-catalog** — `code/v05/var` = 125,304, `code/v05/edits` = 185,340, `mission-scope/eightfold-phase` = 408 are **not declared types at all**. So "% of the declared registry populated" measures a different thing than "% of the data that's typed."
3. **The declared registry is a derived, append/upsert-only byproduct of `run-write!` with no deletion path.** Its 208 docs include **22 noise entries** (11 abstract globs + 11 cross-kind / encoding dups). Cleaning them is **not** a cheap pass — there is no first-class "delete a type" op in the codebase.

### The question

**What is the honest inventory of substrate-2, and what is the minimal, safe set of changes that makes the catalog honest (denominator) and fills the negative space worth filling (numerator) — given that ~20% of the "gap" is empty-by-design and ~50% is the deliverable of other in-flight missions?**

### Discipline this inherits (read first)

- **Never assert "empty" without a count-pushdown or by-id check.** A `0` from a `?type=` scan is usually a timeout lie (footgun #1, `futon1a/README-census.md`).
- **Never bypass `run-write!`** with a raw `xtdb/delete` to "clean" the registry — futon1a's rules forbid it and there is zero precedent. If the registry needs pruning, build the path properly.
- **"Cheap"/"lightweight" is banned as cover.** This excursion exists *because* a billed-cheap pass turned out to need new gated code — say so, don't paper over it.

---

## 1. IDENTIFY — two distinct questions tangled together

The operator ask ("populate the remaining 35%") conflates:

- **(Q-num) the numerator** — which *kinds of thing* are genuinely missing from substrate-2 and worth populating, and
- **(Q-den) the denominator** — the declared-type catalog is polluted (globs + dups) and incomplete (heavy data off-catalog), so any "%" is meaningless until the catalog is made honest.

They have different owners and different costs. Q-den is hygiene (this excursion). Q-num is mostly **other missions'** deliverables (mapped below), plus one clean standalone campaign.

## 2. MAP — the live census (port 7071, 2026-06-26)

### 2.1 Method (reproducible)

Counts are **per-type count-pushdown** (bound type, indexed, fast) — the path `/api/alpha/census` uses:

```clojure
;; one type, fast (entity → :entity/type ; relation/hyperedge → :hx/type):
(ffirst (xtdb.api/q (xtdb.api/db (:node @futon3c.dev/!f1-sys))
                    (quote {:find [(count e)] :where [[e :hx/type :code/v05/var]]})))
```

> **Gremlin to avoid:** a *free-variable distinct* over `:hx/type` (`{:find [t] :where [[e :hx/type t]]}`) errors in the Drawbridge eval ("macroexpanding at (1:1)") while the identical form over `:entity/type` works. Cause unknown; not worth chasing. **Use bound count-pushdown per declared type instead** (loop over the catalog) — that is how every number below was produced. Probe scripts used this session: `/tmp/census_all.clj`, `/tmp/honest_denom.clj`, `/tmp/dup_probe.clj` (re-create from the snippets here; `/tmp` is not durable).

> `/api/alpha/census` is built + committed but **only live after the next JVM reboot** (the HTTP handler is a boot closure). Until then, count via Drawbridge.

### 2.2 The numbers

| Measure | Value |
|---|---|
| Declared type-docs in registry | **208** |
| — of which abstract globs (`*`-suffixed) | 11 |
| — of which cross-kind / encoding dup-docs | 11 |
| **Honest real declared types** (208 − 11 − 11) | **186** |
| Declared types that are populated | **109** (≈ **59%** of 186) |
| Distinct populated types in the *store* (declared **or not**) | **~130** (99 entity-types + ~30 hyperedge-types) |
| Heaviest populations (all **off-catalog**) | `code/v05/edits` 185,340 · `code/v05/var` 125,304 · `mission-scope/eightfold-phase` 408 |

The two "populated" numbers differ because **109** counts populated *declared* types while **~130** counts *all* populated types in the store — the gap is the off-catalog campaigns (the entire `code/v05/*` structural graph + the `mission-scope/*` family are not registry types).

### 2.3 The negative space — 99 empty declared types, in 6 families (not one fill job)

| Family | ~n | What it'd take | Disposition |
|---|---|---|---|
| **A. Glob parents + dup-docs** | ~22 | dedupe + abstract-flag (see §3) | **Never populate** — catalog hygiene, fixes the denominator |
| **B. Sorry/claim/definition graph** — `:closes :depends-on-sorry :defines :constructs :implemented-by :generates-question :responds-to :attacks-claim :would-refute :supported-by :minted-from :produces :roll-up-of :references :inspired-by :evolved-into …` | ~20 | a semantic miner per relation | **= [[M-goals-and-holes]] / [[M-operational-vocabulary]] deliverables** — populating these IS those missions |
| **C. Pattern-language decomposition** — `:pattern/has-{because,context,if,then,however,next-steps,conclusion,sigil} :pattern/includes :pattern/tensions :pattern/component-parent :sigil/sigil :prototype/has-sigil` | ~13 | a pattern-miner | gated on M-pattern work |
| **D. Arxana interest/theme graph** — `:arxana/{theme,scholium,tension-edge,metaphor-theme,…} :interest/{holds,grounded-in,tagged,witnessed-in,coalesces-from}` | ~14 | Arxana persisting to substrate-2 | gated — Arxana writes its own store today |
| **E. Document structure** — `:docbook/{entry,heading,toc} :figure :figure/tags :media :media/lyrics :diagram :diagram/core :diagram/includes :example :excursion` | ~15 | one essays/anthology/docbook ingest campaign | **cleanest standalone — no mission dependency** (reuses the file-ingest pattern) |
| **F. Mission/flight + clustering structure** — `:has-phase :next-phase :has-operator-turn :has-failed-route :has-agent-view :covers-repo :lives-in-repo :belongs-to :leaf-parent :in-{cluster,leaf}-{full,identify} :open-world/{entity,relation}` | ~15 | flight-ingest + anthology clustering emit these | partly emittable now |

**Reframe:** ~20% of "the 35%" is empty-by-design (A), ~50% is the work of missions already on the board (B/C/D — each needs its own miner/writer), and only ~15% (E) is a clean "just run a campaign" with no upstream dependency.

## 3. The hygiene blocker (Q-den) — why "cheap" broke

### 3.1 The exact noise

- **11 globs** (abstract type-families, never instantiated by design): `::pattern/*`, `:arxana/*`, `:devmap/*`, `:i/*`, `:me/*`, `:model/*`, `:pattern/*`, `:person/*`, `:prototype/*`, `:sigil/*` (+1).
- **11 dup-docs** (same `:type/id`, two docs): cross-kind (`:arxana`, `:model`, `:pattern`, `:diagram`, `:model/{descriptor,penholder}`, `:pattern/{component,language,library}`, `:arxana/media-{lyrics,track}` registered as **both** `:entity` and `:relation`) **or** two xt-id encodings (`type|entity|:pattern` colon vs `type|entity|pattern` no-colon — relic of a retired writer; current `type-id->xt-id` only emits colon-form). **All confirmed empty under every kind.**

### 3.2 Why removing them is not cheap

- **The registry has no deletion path.** Type-docs are emitted *inside* `run-write!` as a byproduct of domain writes (`pipeline.clj:129/188` → `types/tx-ops-for-docs`), canonicalized by `xt/id`, "always emits puts." There is no first-class "retract a type" op anywhere.
- **A raw `xtdb/delete` is forbidden** (bypasses the mandated pipeline; futon1a CLAUDE.md; no precedent). So pruning requires **new gated code**.
- **"Mark abstract"** needs a `:type/abstract?` field that does not exist in `type-doc` (`{:xt/id :type/id :type/kind :type/parent :type/aliases}`) — a (small) schema addition plus a setter that runs post-derivation.
- **Regeneration is not a risk for these specific docs** (good news): all 22 are empty, so no live domain doc carries their type, so the derived registry won't recreate them. A one-time prune would stick.

### 3.3 The fork

- **(A) Honest denominator by convention — cheap, zero writes.** Adopt **186** as the real declared-type count (exclude `*`-globs, collapse dup-docs) in the census tooling + explainer + `README-census.md`, so "% populated" is meaningful (109/186 ≈ 59%). No registry mutation. **Recommended now.**
- **(B) Real catalog cleanup — deferred, gated.** Build a registry-pruning op (a proper delete path) + a `:type/abstract?` field, with clj-kondo + tests, to actually remove the 22 noise docs and flag the globs. A scoped follow-on, **not** cheap.

## 4. DERIVE — open design (the driver owns this; framed, not decided)

1. **Pruning path (for B).** Where should a `retract-type!` live — `type_registry.clj` as a first-class op, applied via `submit-tx` on the node, or a `run-write!`-integrated "tombstone" so it shares the audit path? Decide whether deletion is even the right model vs. a `:type/deprecated?`/`:type/abstract?` flag that the catalog endpoint filters (no deletion, fully reversible, regeneration-safe). **Flag-not-delete is likely the right answer** — it preserves hierarchy (globs *are* meaningful abstract parents) and never fights the derive-from-data invariant.
2. **xt-id canonicalization.** Fix `type-id->xt-id` so a single encoding is canonical (colon-form), and migrate/merge the no-colon relics. Guard against the two-encoding split recurring.
3. **Cross-kind policy.** Decide whether one `:type/id` may legitimately be both `:entity` and `:relation`. If not, the registry derivation should reject/dedupe; if yes, the catalog/census must report per-(id,kind), not per-id.
4. **Numerator order (Q-num).** Family **E** (document structure) is the standalone campaign; **B/C/D** feed back to their owning missions (don't bulk-fill from here). **F** is partly free from flight-ingest. Sequence these as their own tickets, not as one "populate the rest."
5. **Off-catalog reconciliation.** Decide whether the heavy `code/v05/*` + `mission-scope/*` families *should* be registered types (so the catalog reflects reality) — or whether the catalog is intentionally a different (curated) layer and the census should report both populations explicitly.

## 5. ARGUE — decisions to ratify

- **Honest denominator first (A), registry mutation second (B)** — Joe's "cheap pass" maps to (A); (B) is real work and gets its own ticket. (Pending Joe.)
- **Flag, don't delete** (likely) — reversible, regeneration-safe, preserves the abstract-parent hierarchy; aligns with the derive-from-data invariant rather than fighting it.
- **Don't bulk-fill the semantic families (B/C/D)** — they are other missions' deliverables; this excursion *maps* them and hands them back, it does not mine them.
- **E is the one campaign this excursion could legitimately spawn** — clean sources, no upstream dependency.

## 5b. INSTANTIATE — part A done (claude-10, 2026-06-26; Joe: "do A first, come to B")

Part **A ratified and executed** (Q-den hygiene, zero registry writes). Part **B deferred** (Joe: "come to B").

**Tool-verified live census (2026-06-26, via `catalog_census.bb` — supersedes the morning hand-count):** the registry **drifted 208→216 docs** in hours (it's an append-only `run-write!` byproduct) — proving exit-2's point that the denominator must be a *tool*, not a frozen number. Current: **216 docs → 196 distinct ids → 186 real declared types** (10 globs dropped) · **101 populated (≈54%)** · 85 empty · off-catalog heavies `code/v05/edits` 185,340 / `code/v05/var` 125,306. (101 vs the morning's 109: re-counted with the correct per-kind attribute — verified **0 kind-mislabels, 0 both-positive**, so 101 is exact.)

**Delivered:**
- **exit-2** — `futon3c/scripts/catalog_census.bb`: one Drawbridge count-pushdown pass → honest denominator + glob/dup classification + per-type populations + off-catalog; writes a durable snapshot (`holes/excursions/substrate-2-catalog-census.edn`). Re-runnable.
- **exit-1** — honest denominator (as *method + current numbers + tool ref*, not a frozen figure) now in `substrate-2-explainer.html` header, `futon1a/README-census.md` (TL;DR + footgun #4), and `M-populate-substrate-2.md` (pointer note + HEAD body). Stale "~35/203" survive only as debunked history.
- **exit-4** — the §2.3 six-family negative-space map rendered on the explainer (`.negspace` section): 85 empty types in 6 families, each colour-coded + labelled by owning mission (A hygiene · B/C/D other-missions · E standalone · F partly-free). Playwright-verified: 0 JS errors, diagram intact.
- **exit-5** — audit found + **corrected a contradicted claim**: the explainer's PATTERN node said "0 instances / lives elsewhere", but the census shows pattern STRUCTURE heavily populated (`:pattern/component` 12,179 · `:pattern/clause` 6,299 · `:pattern/library` 4,635 · `:pattern/language` 44); only the pattern-*language* decomposition (`:pattern/has-{if,then,…}`, `:pattern/tensions`, sigil bindings — all count-verified 0) is empty. Node rewritten to `st:"partial"`. All proof/argument relations re-confirmed 0 by count-pushdown.

**Exit-3 (B): the delete path is BUILT — see §5c.** (Joe overrode flag-not-delete: a real delete path is the future-proof bet — GDPR right-to-erasure needs actual erasure, not a flag.)

## 5c. INSTANTIATE — part B: the GDPR erasure path (claude-10, 2026-06-26; Joe: "create the delete path … GDPR … gate behind joe … CLI")

**Decision:** flag-not-delete → **build a real, first-class, gated delete path** (Joe: future-proof for GDPR right-to-erasure). Erasure uses XTDB **`evict`** (removes a doc AND its entire bitemporal history — a `:delete` only tombstones, so `db-as-of` still recovers the data; that fails GDPR). It rides the gated pipeline, not a raw evict.

**Delivered (all futon1a):**
- `core/pipeline.clj` — **`run-erase!`**: L4 validate (non-empty eids + reason) → L3 authz (gated to penholder **"joe"**, `erase-allowed-penholders`) → L0 durable evict, with the counter-ratchet permitting the deliberate drop. Emits a **non-revealing `:erasure-event` audit** (actor·reason·count·**sha256** of the eids·time — no content). Plus **`retract-type!`** (sugar over run-erase! for type-docs, via the new resolver).
- `core/invariants.clj` — taught the counter-ratchet to **see `:xtdb.api/evict`** (op-id + apply-op), so erasure is counted as a drop and must be explicitly permitted (no silent bypass of the count guard).
- `model/type_registry.clj` — `type-xt-ids-present` (resolves a type-id to all present kind/encoding variants).
- `scripts/erase.bb` — the **CLI** (no API surface): drives the running node via Drawbridge as penholder "joe"; **default dry-run** (shows each target's population as a safety guard); `--execute` requires `--reason`.
- `test/futon1a/core/erase_test.clj` — 5 tests / 23 assertions (L4 gating, joe-only L3 gate, the evict+audit tx shape with no raw-eid leak, ratchet-sees-evict). `clojure -X:test` green; clj-kondo 0 errors; check-parens OK.

**Live-proven** (loaded into the JVM via Drawbridge `load-file`; round-trip on a throwaway doc): non-joe → refused; evict → doc gone **and `entity-history` = 0** (true erasure, not a tombstone); audit-event persisted with sha256 + no raw eids. CLI dry-run shows the ⚠️ population guard firing on a populated type.

**Catalog-cleanup finding (the actual erasure is Joe's gated call):** "the 22 noise docs" is really **per-doc, not per-type-id**, and **the `*`-globs are load-bearing abstract parents** (14 type-docs — several *populated*, e.g. `:pattern/library`→`:pattern/*` — set a glob as `:type/parent`). So globs must **not** be erased without re-parenting. The clearly-safe first cut is the **8 no-colon encoding relics** (retired-writer artifacts) for which the colon-canonical doc survives — zero type lost, zero parent orphaned, zero data touched:
`type|entity|{arxana/media-track, model/descriptor, pattern, pattern/component, pattern/library, person}` · `type|relation|{pattern/has-sigil, pattern/includes}`.

**Named follow-ons:** (a) glob removal via a re-parenting migration (children → namespace parent) — globs are *kept* for now; (b) the empty cross-kind dups (a per-type canonical-vs-relic curation); (c) GDPR generalisation — subject-erasure across entity/relation/hyperedge + a re-ingestion suppression list (eviction alone doesn't hold for *live* data the watcher would re-derive).

## 6. Exit conditions (testable; provisional until ARGUE)

1. The explainer header + `README-census.md` + the mission HEAD state the **honest denominator** (186 real declared types; 109 populated ≈ 59%; ~130 store-side populated incl. off-catalog) — replacing every stale "~35 of 200". *(A — cheap.)*
2. A reproducible **catalog-census tool** exists (not `/tmp`) that emits per-type populations + the glob/dup classification, so the numbers can be re-derived on demand and the explainer never drifts.
3. **Either** the 22 noise docs are flagged/removed via a gated path (no raw delete) **or** a `:type/abstract?`-style flag + catalog filter makes them disappear from "% populated" honestly. *(B — gated.)*
4. The negative-space map (§2.3) is reflected on the explainer (the 6 families as bands/tags) so the kitchen's empty cupboards are *visible*, with each labeled by its owning mission.
5. No new "empty" claim anywhere in the substrate-2 docs that wasn't produced by a count-pushdown or by-id check.

---

### Provenance (this session, 2026-06-26)

Census run live against `:7071` via Drawbridge count-pushdown. Corrected the relation→`:hx/type` mapping after a first pass mis-counted 75 relation types via `:entity/type` (artifact: 75 → 72 truly empty). Confirmed heavy types off-catalog (grep of `/types` for `code/v05/*` empty). Confirmed no deletion path (registry emitted in `run-write!`, `pipeline.clj:129/188`; no `retract-type!` in source). The billed-cheap hygiene pass was found to require new gated code; surfaced rather than executed as a raw write.
