# Review queue and validation record

## Validation actually performed (distinct from mathematical verification)

- Source pins and sha256 hashes for all 12 files (6 statements + 6 solutions)
  recorded in `selection-manifest.md`; recomputed from the apm-lean working
  tree at `6da56460`.
- `grep -c sorry` over each pinned Main.lean: 0 for all six.
- Dependency consistency: each reconstruction's cited lemma names were
  checked to exist in the pinned Main.lean (read in full for t01A03, t91J02,
  t93A04, t92J07, a97J04; a94A02 read through the measure-decomposition
  stage). ConstructionTargets imports named in the reconstructions were
  confirmed to exist.
- Authorship claims traced to campaign-ledger seat entries (timestamps in
  manifest discussion) and, for a97J04, the in-source memory citation, whose
  evidence entry was read in full.
- **No Lean elaboration was run for this packet and none is claimed.**
  Elaboration evidence is historical (ledger gates, `#print axioms` lines in
  sources, prior audit trail). Also not performed: memory attachment edits,
  canonical library edits, any contact with the transfer-pilot corpus or its
  Student/TA, sealed holdouts, Claude calls, live campaigns, RUN4 paths.

## Review queue (for a future reviewer/owner; nothing actioned here)

1. **D1 (left-inverse-injectivity-obstruction)** — two witnesses in packet
   plus census-T3 corroboration and sibling t91A05 reuse note. Ready for
   pattern-draft review *if* a third independent (non-sibling-infrastructure)
   witness is found; t00A04 is the recorded boundary case and should be
   cited in any HOWEVER clause. Needs: decision whether it belongs in
   math-informal (strategy) or a subject-suffixed family.
2. **D2 (null-bad-values-vs-positive-measure-target)** — strongest
   candidate: two in-packet witnesses plus two census witnesses (t94J05,
   t95J04) already named. Action: re-inspect t94J05/t95J04 sources to
   confirm they implement the same construction, then promotion review.
3. **D3 (summable-dominator-then-identify)** — the a97J04 memory already
   exists and is reviewed; the cheapest library win is *linking* that memory
   to a pattern-id and adding the a94A02 example, rather than minting new
   prose. Decide: extend the existing memory (conditions 1–3 are the needed
   granularity) vs new two-witness pattern. Coordinate with whoever owns
   memory captions; do not touch during the transfer pilot.
4. **Stale status.json hygiene** (outside this packet's scope, flagged):
   t93A04/t92J07 status.json still says partial/1–2 sorries while their
   Main.lean at HEAD is sorry-free and was closed 2026-09-09. Any consumer
   filtering on status.json will misclassify these. Recommend the apm-lean
   owner regenerate those two status files.
5. **Unresolved matters.**
   - a94A02 authorship stays *mixed* (zai-1 and ams-codex-1 both have closer
     hops); resolving it needs artifact-level commit attribution beyond the
     ledger, if it matters downstream.
   - Whether `ams-scribe-1`/`ams-codex-N` seats count as "Codex" vs a
     separate scribe class is a naming convention question for codex-16;
     this packet calls them all Codex-family per the seat names, with scribe
     seats distinguished where they appear.
   - Bounded-search caveat: absence claims are scoped to the searches listed
     in `comparison-table.md` (canonical math families at `a376e404`, the
     364-memory retained snapshot, futon3c evidence-store tag queries). The
     39 untracked coined-pattern deposits inventoried by the prior census
     were NOT read (consistent with that census's own limit).
