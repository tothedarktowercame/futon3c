# E-mfuton-lisp-csimp-20261003

**Requisition:** in-progress — codex-13 job invoke-1790998522127-30707-8bdc3bb9

**VERDICT (2026-10-09, provisional):** ACTIVE — Requisition in-progress on codex-13 dispatched 2026-10-03, within the last 30 days; no completion recorded yet. _(WM status classification by zai-4, medium confidence; not yet confirmed by the author.)_

Implement Lisp csimp replacement and scope rebuilding in /home/joe/code/mfuton-sbcl, following Joe's instruction to catch up with Codex-17's Python port, not repair the old Python baseline. Read /home/joe/code/AGENTS.md and lisp/curriculum/capabilities.json. Ground truth Lean4 v4.29.0 Lean/Compiler/CSimpAttr.lean. Python reference /tmp/mfs-blank at f5d45940f10e54b8608da0cee9b6bf724e5d8a2e, read-only: python_source_csimp_replacements.py plus SourceTypeclassScope.from_declarations rebuilding contract. Tests MUST load Python oracle from /tmp/mfs-blank/src (record imported __file__ and Git hash), not our older local src or mfuton-share. Use a subprocess boundary for oracle serialization if necessary. Do not edit ANY Python production source in either checkout.
Existing Lisp accepted state 20f376e implements pre-csimp make-source-typeclass-owner-index-state. Exclusive new production lisp/src/source-csimp-replacements.lisp and lisp/src/source-typeclass-scope-rebuild.lisp; exclusive test lisp/test/source_csimp_replacements_differential.py; evidence lisp/parallel/curriculum-targets/csimp/. Shared files and ASDF/Makefile/curriculum are coordinator-owned. No commits, additional agents, resets or services changes.
Port generic raw and compact theorem decoding, rejection errors, namespace/source-path/before-line resolution, replacement declarations/runtime dependency edges, absent source handling, and occurrence-preserving rewrite. Implement real scope rebuild after replacement using existing production scope authorities and carry input state faithfully; do not claim complete from-declarations if options/caches still lack authorities. Real native parsed fixtures incl valid @f=@g, malformed attached attribute, nonconstant side, compacted forms, missing source/target, override/conflict behavior according to actual Python, and replaced scope lookup. Compare real Python values and identities, not test stubs. Execute native Lean controls where claims depend on semantics; flag Python/Lean mismatch instead of modifying Python.
Run focused differential, owner-index/scope regressions, check-parens and SBCL load. Retain exact commands/exits/provenance. Report which acceptance for curriculum csimp-replacement passes and any missing rebuilding behavior. We count capability progress now; no need to headline 0/110. Coordinator codex-18 reviews and commits; bell delivery.

## Closure criteria (provisional, 2026-10-09)

_Drafted from this document's own stated goals during the War Machine status classification (zai-4, high confidence); not yet confirmed by the author._

- [ ] lisp/src/source-csimp-replacements.lisp and lisp/src/source-typeclass-scope-rebuild.lisp implement the csimp replacement port and post-replacement scope rebuil…
- [ ] Differential lisp/test/source_csimp_replacements_differential.py loads the /tmp/mfs-blank oracle (recorded __file__ and Git hash) and compares real Python valu…
- [ ] Gates pass: focused differential, owner-index/scope regressions, check-parens and SBCL load.
- [ ] Report states which curriculum csimp-replacement acceptance passes and any missing rebuilding behavior.
