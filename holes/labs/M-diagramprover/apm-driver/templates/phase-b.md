PHASE B - now solve apm-{{problem_id}} end-to-end (same session as
your phase-A reconnaissance; your hint list is your starting
context).

1. Use your phase-A hints where they genuinely help. CITE every
   memory whose content you actually use, with the exact comment form
   `-- (Memory: e-<id>)` adjacent to the informed code, and every
   PATTERN used with `-- (Pattern: <ns>/<name>)`. The forms are
   machine-checked and distinct: `Memory:` only for store ids
   (beginning `e-`); `Pattern:` only for pattern-library ids.
   If a hint turns out not to help, drop it without ceremony.
2. CONTINUE looking things up at these moments, mid-solve: whenever
   a Mathlib search comes back empty, and BEFORE writing any
   boundary comment or declaring any blocker - one memory lookup
   first (psr_search with your obstacle vocabulary, or tag-based
   memory_search), then proceed. If a lookup returns nothing useful,
   note it in a comment and move on - do not force relevance.
3. THE WORK: statement first, faithfully, from {{bundle_path}}
   (problem.md, problem.tex, informal-solution.md) - structural
   repairs documented in comments if the source statement is
   defective; no weakening. Then prove: aim zero sorries, exit 0 on
   lake env lean {{main_lean_path}}, validate continuously. A
   partial is a classified boundary, valid only after genuine
   sustained attempts, with a boundary comment recording: the APIs
   searched, the routes tried, the exact remaining bridge, and the
   routes NOT investigated.
4. Before committing: verbatim #print axioms on the main theorem;
   statement re-read against the informal problem.
5. status.json + proof-outline.md honest; commit path-limited to
   {{bundle_path}} with a problem-specific message.

REPORT: final sorry count, axiom output verbatim, commit sha, the
Lean statement verbatim, which phase-A hints you used vs dropped
(by number), and every mid-solve lookup you made (query, what came
back, used or not).
