# Memory/pattern citation grading — the instrument (settled 2026-08-06)

Converged across claude-10/claude-12 after three rounds of mutual
refutation (each round's claim killed by a cheap check the other ran).
This file is the surviving protocol; the derivation lives in the bell
transcripts and the futon6 warrant docs.

## Markers (machine-checked; `citation_lint` in statement_campaign.py)

- `-- (Memory: e-<id>)` — store memories ONLY (ids begin `e-`).
- `-- (Pattern: <ns>/<name>)` — pattern-library ids ONLY.
- Cross-labeled markers are lint failures. Pattern citations are
  cascade-side evidence and never count toward M5 memory-use.

## Plan-impact field (recorded at recon/pass-1 time, per hint)

`SUPPLIED` (gave a route/lemma not already held) / `CONFIRMED`
(corroborated one already derived) / `UNCLEAR`. Anti-gradient wording
in the templates: an honest CONFIRMED outranks an optimistic SUPPLIED.

## The SUPPLIED veto-grep (testimony gets a mechanical veto)

At grade time, grep the memory's route terms against the problem's
shipped scaffolding **as of the chain's `sha-pre`**:

    git show <sha-pre>:problems/<id>/informal-solution.md
    git show <sha-pre>:problems/<id>/problem.md

If the route terms are present, SUPPLIED is unavailable — the grade is
mechanically forced to CONFIRMED regardless of self-report.

Why sha-pre and not filenames: chains MODIFY scaffolding files
(measured: proof-outline.md 52 post-creation modifications,
informal-solution.md 22, problem.md 0). A runner-written route term in
a veto source produces a false CONFIRMED (under-credits memory — safe
direction, still bias). Reading at sha-pre makes contamination
structurally impossible. sha-pre is captured per dispatch (SEQ-0.1).

## Use-witness typing (claude-12's, unchanged)

`cited-only` / `route-followed-direct` / `route-followed-mediated`
(load-bearing in an IMPORTED artifact; same-file factoring is direct)
/ `uncited-route-followed`. Channel recorded: pull / push / packet /
unknown. Warrant recorded: mechanical (grep+compile) / forensic.

## Edge ledger as of 2026-08-06

- retrieval-hit → memory-used: SOLID, n=1 (a97J03), pull, mechanical.
- memory-used → problem-solved: DASHED. a97J03's memory was
  route-CONFIRMING (forced by informal-solution.md at sha-pre).
- SUPPLIED availability: per-(problem, memory), grep decides;
  demonstrated available in at least one case (a96A08), so far
  unexercised.
- Pattern-use mechanically cited: a03J01 (6 sites, 2 referents) —
  cascade-side.
- Zero-citation close (denominator): a94A08.

## Design finding (per-problem form)

Memory value concentrates where shipped scaffolding is thin — and
thinness is measurable per problem at sha-pre, not assumable. 170/462
shipped informal-solutions contain backticked Mathlib identifiers;
blanket claims in either direction are dead.
