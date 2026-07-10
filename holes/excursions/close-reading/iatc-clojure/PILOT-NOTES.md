# IATC Argument-to-Clojure — pilot (2026-06-15)

A Codex-pool + handoff-review prototype of the **§6 successor superpod stage**
("Argument-to-Clojure compilation", `futon6/holes/handoffs/rob-presuperpod-crossmsc-2026-06-14.md`),
which otherwise presupposes the superpod LLM. Done instead with the same
Codex-pool method used for the expository-scope run.

## What ran

1. **Checker built first** (codex-3, reviewed): `futon6/scripts/iatc_argcheck.bb`
   — Babashka, enforces all 7 §6 gates (EDN parses; node/edge source loci; edge
   endpoints resolve; `:ref` resolves or is a hole; `:meta` not an object-layer
   `:conclusion`; subproof scopes nested; warrant gaps explicit as
   `{:kind :missing-warrant …}`). Validated: 3 hand-built seed graphs (Example
   A/B/C) PASS; one negative fixture per gate FIRES. clj-kondo + check-parens clean.
2. **Pilot pool** (codex-1, codex-2), few-shot-anchored by seeds A/B/C, each graph
   self-gated against the checker until PASS before bell-back.

## Output — 13 argument graphs, all checker-PASS

- `0905.0595/` (codex-1, 5): prop0.2-proof, cor0.3-trivial-model,
  th0.4-ar-counterexample, th0.4-rectification-contradiction, prop0.4-combinatorial-model.
- `0807.1872/` (codex-2, 4): L1.3-C1.4, C1.6-no-localization, C1.7-brown,
  explicit-nonrepresentability.
- `1005.2653/` (codex-2, 4): antipode-from-duality, fourier-basic-properties,
  fourier-transfer, ehresmann-connection.

Re-run to verify: `futon6/scripts/iatc_argcheck.bb <dir>` (0 FAIL across all three).

## Review findings (claude-3, real gate)

- Independent checker re-run: 13/13 PASS, 0 FAIL.
- Fidelity (read `th0.4-ar-counterexample` and `ehresmann-connection`): graphs
  faithfully capture the inference chains, source-anchored, nothing restated as
  object content. The AR-counterexample graph **improved on seed A** (correctly
  placed the passage under th0.4 where the seed mislabeled it prop0.2).
- **Honest typed holes:** each `:missing-warrant` names exactly what the prose
  elides (presentability-factorization rule; "Dually" left unspelled; convolution
  star-autonomy asserted without proof; the Kleisli-algebroid↔Ehresmann-groupoid
  speculative aside with no comparison theorem). The speculative aside is encoded
  as a `:may-be-related` edge + `:depends-on` weak support + explicit hole —
  reusing the close-reading `expository-connection` vocabulary.

## Verdict

The §6 stage is **producible by Codex-pool + handoff-review**, with the checker
offloading structural review so human review is purely about fidelity. These 13
graphs can seed the superpod stage's few-shot prompt. Scales by fan-out (like the
expository run) once a larger IATC-scope exemplar set exists.

**Next step (not yet done):** render integration — surface these `.edn` graphs in
the dp-anatomy showcase IATC areas (`dp_enrich.py` / `dp_anatomy_html.py`), so the
compiled argument layer displays alongside the object-layer marks.
