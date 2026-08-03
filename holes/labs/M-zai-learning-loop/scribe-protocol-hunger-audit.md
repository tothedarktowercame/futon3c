# Scribe protocol amendment: the hunger audit (Joe, 2026-08-03)

Failed retrieval queries are demand signals — the runner's own
vocabulary for what it wanted, at the moment it wanted it. The store
records supply; hungry queries record demand. Every scribe pass now
includes:

## The step

1. **Collect the session's hungry queries** — every memory-tool call
   that returned empty or noise (from the SEQ-0.5 receipt fields, the
   boundary-comment consultation trails, and the phase reports).
   Exclude degraded-under-load results (receipt `degraded? true`) —
   only genuine emptiness is hunger; degradation is noise.
2. **For each, ask: did the concept get grounded later in the
   session?** (Found in Mathlib, proved locally, or documented as a
   gap.)
   - **Grounded → demand-side tagging:** the resulting memory MUST
     carry the hungry query's literal vocabulary as tags — copy the
     asked terms, don't paraphrase into supply vocabulary. Evidence
     for why: E10's mid-solve hunger ("residue theorem rectangle
     Cauchy-Goursat") closed exactly because the scribe tags matched
     the asked terms; E10's phase-A hunger (["contour" "integral"
     "residue"]) still misses the j07 memories because their
     supply-side tags (:contour-integral :Cauchy-formula) sit NEAR
     but not ON the demand terms — exact-match tag queries fail on
     near.
   - **Not grounded → an open-hunger memory:** a small entry
     recording the query, the proof stage, and what was sought —
     a demand signal for corpus growth, satisfiable by a future
     session's scribe pass (at which point the open-hunger entry is
     superseded by the grounded memory, inheriting its tags).
3. **Tag-backfill corollary:** the accumulated hungry-query log is
   the CORRECT source for backfilling tags on the 500+ older
   memories — recorded asker vocabulary beats invented vocabulary.

## Why this works (one day's evidence)

The demand/supply vocabulary mismatch is the persistent layer-4
failure: E9's exit interview (imagined tags didn't exist), E10 phase A
(contour tag queries empty), the psr index (densest endpoint lexically
camouflaged). Hungry queries are literal recordings of asker
vocabulary — the only tag source that cannot mismatch by
construction.

## Corollary: silent runners starve the corpus twice

E9 (invitation-only) produced ZERO demand signals — no queries, so
no hunger log, so nothing for this audit to consume. The instruction
fix (E10's two-part frame) is what makes the hunger audit possible at
all: a runner that never asks neither consumes the store nor teaches
it what to stock.
