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

## 6. Exit conditions (testable; provisional until ARGUE)

1. The explainer header + `README-census.md` + the mission HEAD state the **honest denominator** (186 real declared types; 109 populated ≈ 59%; ~130 store-side populated incl. off-catalog) — replacing every stale "~35 of 200". *(A — cheap.)*
2. A reproducible **catalog-census tool** exists (not `/tmp`) that emits per-type populations + the glob/dup classification, so the numbers can be re-derived on demand and the explainer never drifts.
3. **Either** the 22 noise docs are flagged/removed via a gated path (no raw delete) **or** a `:type/abstract?`-style flag + catalog filter makes them disappear from "% populated" honestly. *(B — gated.)*
4. The negative-space map (§2.3) is reflected on the explainer (the 6 families as bands/tags) so the kitchen's empty cupboards are *visible*, with each labeled by its owning mission.
5. No new "empty" claim anywhere in the substrate-2 docs that wasn't produced by a count-pushdown or by-id check.

---

### Provenance (this session, 2026-06-26)

Census run live against `:7071` via Drawbridge count-pushdown. Corrected the relation→`:hx/type` mapping after a first pass mis-counted 75 relation types via `:entity/type` (artifact: 75 → 72 truly empty). Confirmed heavy types off-catalog (grep of `/types` for `code/v05/*` empty). Confirmed no deletion path (registry emitted in `run-write!`, `pipeline.clj:129/188`; no `retract-type!` in source). The billed-cheap hygiene pass was found to require new gated code; surfaced rather than executed as a raw write.
