# E-mfuton-native-installation-20261003

**Requisition:** in-progress — invoke-1790999811446-30715-8f3cacac

**VERDICT (2026-10-09, provisional):** ACTIVE — Requisition header states in-progress with git dispatch on 2026-10-03, within the last 30 days. _(WM status classification by zai-2, medium confidence; not yet confirmed by the author.)_

Work on the native-installation capability in /home/joe/code/mfuton-sbcl while codex-13 connects compiler scope. Read workspace AGENTS.md and lisp/curriculum/capabilities.json. Lean4 ground truth; Python reference /tmp/mfs-blank@f5d45940f10e54b8608da0cee9b6bf724e5d8a2e. No Python production repairs. Do not touch concurrent lisp/transpiler files (another lane has dirty edits).
Determine the real handoff needed between Lisp body compilation/emission and installed-generated-binding, including execution backend, catalog identity, imports, state ownership and failed initialization behavior. Existing installed-binding reads caller-owned hash maps, not real installed modules. Existing standalone transpiler is not the Lisp body compiler and translating a cached Python owner must never be credited as owner compilation. Inspect it read-only for an actual reusable execution/install operation.
Exclusive files: lisp/src/generated-module-installation.lisp only if actual production dependencies permit a faithful installer, lisp/test/generated_module_installation_differential.py, and lisp/parallel/curriculum-targets/installation-contract/. No shared edits, commits or additional dispatch. If implementation is feasible, build and test the generic operation using an actual Lisp-produced module and native runtime, not mock registry injection. If producer/runtime mismatch blocks it, deliver an executable contract check against the real APIs with exact missing operation and typed I/O; do not fabricate successful installation. Include independent owner and member identity/behavior requirements needed for final array-end-to-end test. Follow focused gate/check-parens requirements. Bell codex-18 with evidence and scope. This job does not earn completion until real installation is executed.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-4, high confidence); not yet confirmed by the author._

- [ ] Record the real handoff needed between Lisp body compilation/emission and installed-generated-binding (backend, catalog identity, imports, state ownership, fai…
- [ ] If feasible, build lisp/src/generated-module-installation.lisp as a faithful generic installer using an actual Lisp-produced module and native runtime (no mock…
- [ ] lisp/test/generated_module_installation_differential.py executed against the real APIs with the exact missing operation and typed I/O recorded.
- [ ] Real installation executed — the packet states the job earns no completion until then.
