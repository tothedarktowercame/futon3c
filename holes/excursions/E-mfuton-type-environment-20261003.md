# E-mfuton-type-environment-20261003

**Requisition:** in-progress — invoke-1791001708071-30743-bfdbbc58

**VERDICT (2026-10-09, provisional):** ACTIVE — Requisition header says in-progress and the requisition was recorded 2026-10-03, well within 30 days. _(WM status classification by zai-3, high confidence; not yet confirmed by the author.)_

Port the real compiler_type_environment prerequisite for _module_declaration_drafts from /tmp/mfs-blank@f5d45940f10e54b8608da0cee9b6bf724e5d8a2e into /home/joe/code/mfuton-sbcl, Lean4 ground truth. Exclusive new lisp/src/compiler-type-environment.lisp and optional lisp/src/compiler-environment-binder-defaults.lisp, lisp/test/compiler_type_environment_differential.py, evidence lisp/parallel/curriculum-targets/type-environment/. No shared manifest/curriculum edits, Python edits, commits or internal collaborators.
Coordinator separately ports declaration-body-namespace in lisp/src/declaration-body-namespace.lisp, mfuton.source package, ASDF integrated shortly. Use it. Existing external-declaration-type-context is deliberately only an emitter projection, not a substitute environment.
Implement real environment construction including scope hydration/canonicalization, binder/result variables, substitutions, runtime names, shared scope layout identity, inferred binder default types. Inspect dependencies first: do not substitute empty maps or omit callable authorities for unsupported behavior. If full constructor needs unported generic operations, implement the first bounded reusable operation with real differential and name exact remaining calls, instead of claiming complete constructor. Do not let definition of a carrier masquerade as completed semantic constructor or body compilation. No surface-csimp repairs. Explicit supported options, failure conditions and source ownership. Native parsed Array owner plus representative binder/default/substitution fixtures; meaningful negative controls, object identity/input nonmutation. Pin exact source path+revision of Python oracle; no old baseline. Run focused differential, relevant tests, SBCL compile/load and check-parens. Freeze and bell codex-18 after delivery. Environment /tmp/mfs-venv/bin/python; PATH=/home/joe/.elan/bin; MFUTON_HOME=/home/joe/code; MFUTON_LEAN_PARSER_LIB=/home/joe/code/mfuton-linux-binding/shim/.lake/build/lib/libMfutonLeanPyShim_MfutonLeanPyShim.so.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-4, high confidence); not yet confirmed by the author._

- [ ] Port compiler_type_environment for _module_declaration_drafts in lisp/src/compiler-type-environment.lisp with real scope hydration, binder/result variables, su…
- [ ] Differential lisp/test/compiler_type_environment_differential.py pins the /tmp/mfs-blank oracle revision and covers native Array owner plus binder/default/subs…
- [ ] If the full constructor needs unported operations, deliver the first bounded reusable operation with real differential and name the exact remaining calls.
- [ ] Gates pass: focused differential, relevant tests, SBCL compile/load, check-parens; freeze and bell codex-18.
