# E-mfuton-scope-extension-20261003

**Requisition:** in-progress — invoke-1791000436540-30723-f13e6e78

**VERDICT (2026-10-09, provisional):** ACTIVE — Requisition in-progress (invoke-1791000436540) dispatched 2026-10-03 within the last 30 days; no completion recorded. _(WM status classification by zai-4, medium confidence; not yet confirmed by the author.)_

Implement extend-compiler-type-scope for owner path in /home/joe/code/mfuton-sbcl using /tmp/mfs-blank@f5d45940f10e54b8608da0cee9b6bf724e5d8a2e Python oracle and Lean4 ground truth. Read workspace AGENTS.md. Preserve concurrent work; no Python changes, internal collaborators, commits or shared manifests/curriculum edits.
Exclusive new production lisp/src/compiler-scope-extension.lisp and optional lisp/src/source-typeclass-scope-extension.lisp; test lisp/test/compiler_scope_extension_differential.py; evidence this directory. Coordinator is implementing merge-source-kind-layouts in separate lisp/src/compiler-layout-merge.lisp, available via ASDF shortly; use that helper (accepts base/local sequences -> vector, last wins at first key position, EQ preserved), do not write it yourself.
Port real SourceTypeclassScope.extend including carry_materialized_environment semantics and csimp rebuild, kernel additions, demanded hydration, local layout construction and merging as python_types_compiler_scope.extend_compiler_type_scope. Existing hydration environment operations may help but do not omit state preservation. Explicitly reject unsupported options consistent with build-compiler-type-scope. Demonstrate persistent parent, changed occurrence invalidation, cache sharing/copying rules and dynamic kernel injection with real successor Python oracle. Include native parsed Array target plus extended aliases/structures. No dummy authorities. Run focused differential, constructor and csimp regressions, SBCL and check-parens. Report next missing body call and precise limits. Environment /tmp/mfs-venv/bin/python; PATH /home/joe/.elan/bin; MFUTON_HOME=/home/joe/code; parser shim /home/joe/code/mfuton-linux-binding/shim/.lake/build/lib/libMfutonLeanPyShim_MfutonLeanPyShim.so. Bell codex-18 delivery.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-4, high confidence); not yet confirmed by the author._

- [ ] Implement extend-compiler-type-scope (SourceTypeclassScope.extend incl. carry_materialized_environment, csimp rebuild, kernel additions, hydration, layout merg…
- [ ] Differential lisp/test/compiler_scope_extension_differential.py demonstrates persistent parent, changed occurrence invalidation, cache sharing/copying and dyna…
- [ ] Gates pass: focused differential, constructor and csimp regressions, SBCL, check-parens.
- [ ] Report names the next missing body call and precise limits; bell codex-18.
