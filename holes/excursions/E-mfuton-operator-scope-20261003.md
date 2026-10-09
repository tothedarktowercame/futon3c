# E-mfuton-operator-scope-20261003

**Requisition:** in-progress — invoke-1791001173681-30733-087009f6

**VERDICT (2026-10-09, provisional):** ACTIVE — Header states the requisition is in-progress (dated 2026-10-03, within the last 30 days). _(WM status classification by zai-5, medium confidence; not yet confirmed by the author.)_

Implement compiler_scope_from_declarations from /tmp/mfs-blank@f5d45940f10e54b8608da0cee9b6bf724e5d8a2e operator_scope.py in /home/joe/code/mfuton-sbcl. Lean4 remains ground truth. Exclusive files lisp/src/compiler-operator-scope.lisp, lisp/test/compiler_operator_scope_differential.py, evidence lisp/parallel/curriculum-targets/operator-scope/. No shared manifest/curriculum edits, Python edits, commits, internal collaborators, or unrelated files.
Coordinator separately implements binary-operator-symbol in mfuton.source in lisp/src/binary-operator-symbol.lisp and integrates it in ASDF shortly. Use it; do not duplicate token Unicode logic. Existing mfuton.raw descendant-nodes/arg/identifier-values and source model carriers are available. Implement real operator methods, module-root instance edges, callee/reference indexes, grounded graph resolution, notation registration identity/precedence, compiler scope query methods and cached-binary-operator overlay. ImportedNotationScope/TranslationUnitNotationScope macro provider execution is outside this packet; do not claim it.
Run real successor differential covering ambiguity, cycles, grounding, notation ordering and identity, cache overlay, per-query type refs, module reference resolution, and native parsed Array fixture plus a real native operator/instance fixture. Keep original raw carrier identities. No dummy authorities or silent omissions. Document and test any unsupported boundary; avoid calling a partial constructor complete. Gates focused differential, relevant regressions, SBCL compile/load, check-parens. Record exact oracle path and revision. Bell codex-18 when ready. Environment /tmp/mfs-venv/bin/python, PATH /home/joe/.elan/bin, MFUTON_HOME=/home/joe/code; shim MFUTON_LEAN_PARSER_LIB=/home/joe/code/mfuton-linux-binding/shim/.lake/build/lib/libMfutonLeanPyShim_MfutonLeanPyShim.so.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-4, high confidence); not yet confirmed by the author._

- [ ] Implement the compiler_scope_from_declarations port in lisp/src/compiler-operator-scope.lisp using the coordinator's binary-operator-symbol.
- [ ] Differential lisp/test/compiler_operator_scope_differential.py covers ambiguity, cycles, grounding, notation ordering/identity, cache overlay, per-query type r…
- [ ] Gates pass: focused differential, relevant regressions, SBCL compile/load, check-parens, with oracle path and revision recorded.
